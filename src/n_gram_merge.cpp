#include <Rcpp.h>
#include"refinr.h"
using namespace Rcpp;


// Iterate over all clusters, make mass edits related to each cluster.
// [[Rcpp::export]]
CharacterVector merge_ngram_clusters(List clusters,
                                     CharacterVector n_gram_keys,
                                     CharacterVector univect,
                                     CharacterVector vect) {
  CharacterVector output = clone(vect);

  // Unlist clusters to a char vector, and get uniques.
  CharacterVector clust_unlist = unique(cpp_unlist(clusters));

  // Create maps
  std::vector<std::string> cl_ul = as<std::vector<std::string> >(clust_unlist);
  std::vector<std::string> uni = as<std::vector<std::string> >(univect);
  refinr_map ngram_map = create_map(n_gram_keys, cl_ul);
  refinr_map univect_map = create_map(vect, uni);

  List::iterator clust_end = clusters.end();
  List::iterator iter;

  for(iter = clusters.begin(); iter != clust_end; ++iter) {
    CharacterVector curr_clust = *iter;

    // Create subset of univect using the indices of n_gram_keys that appear
    // in curr_clust.
    int curr_clust_len = curr_clust.size();
    std::vector<int> ngram_idx;
    for(int i = 0; i < curr_clust_len; ++i) {
      std::string curr_clust_str = as<std::string>(curr_clust[i]);
      std::vector<int> curr_ngram_idx = ngram_map[curr_clust_str];
      ngram_idx.insert(
        ngram_idx.end(), curr_ngram_idx.begin(), curr_ngram_idx.end()
      );
    }
    int ngram_idx_len = ngram_idx.size();
    std::vector<std::string> univect_sub(ngram_idx_len);
    for(int i = 0; i < ngram_idx_len; ++i) {
      univect_sub[i] = uni[ngram_idx[i]];
    }

    // Create subset of vect using the elements of univect_sub.
    int univect_sub_len = univect_sub.size();
    std::vector<int> uni_idx;
    for(int i = 0; i < univect_sub_len; ++i) {
      std::vector<int> curr_uni_idx = univect_map[univect_sub[i]];
      uni_idx.insert(
        uni_idx.end(), curr_uni_idx.begin(), curr_uni_idx.end()
      );
    }
    int uni_idx_len = uni_idx.size();
    CharacterVector vect_sub(uni_idx_len);
    for(int i = 0; i < uni_idx_len; ++i) {
      vect_sub[i] = vect[uni_idx[i]];
    }

    // Find the string that appears most frequently in vect_sub.
    String most_freq_string = most_freq_str(vect_sub);

    // Edit all elements of output[uni_idx] to be equal to most_freq_string.
    for(int n = 0; n < uni_idx_len; ++n) {
      output[uni_idx[n]] = most_freq_string;
    }
  }

  return output;
}


// Filter the initial clusters, then pass args along to merge_ngram_clusters().
// [[Rcpp::export]]
CharacterVector merge_ngram_clusters_approx(CharacterVector n_gram_keys,
                                            CharacterVector univect,
                                            CharacterVector vect,
                                            List distmatrices,
                                            double edit_threshold,
                                            List initial_clust) {
  // For each matrix in distmatrices, create clusters of matches within the
  // matrix, based on lowest numeric edit distance (matches must have a value
  // below edit_threshold in order to be considered suitable for merging).
  List clusters = filter_initial_clusters(distmatrices, edit_threshold,
                                          initial_clust);

  // If length of clusters is zero, return vect unedited.
  if(clusters.size() == 0) return(vect);

  // If clusters is no len zero, pass args along to merge_ngram_clusters().
  return(merge_ngram_clusters(clusters, n_gram_keys, univect, vect));
}


// Get initial ngram clusters.
// For each string in unigram_dups, find indices in which that string appears
// in unigram_keys, then use those indices to get a subset of ngram_keys. Add
// the subset to List "out". After "out" is compiled, remove elements of the
// list that have length less than 2.
// [[Rcpp::export]]
List get_ngram_initial_clusters(CharacterVector ngram_keys,
                                CharacterVector unigram_keys) {
  CharacterVector unigram_dups = cpp_get_key_dups(unigram_keys);
  List out(unigram_dups.size());

  // Remove indices from ngram_keys and unigram_keys in which ngram_keys are
  // NA.
  LogicalVector na_idx = !is_na(ngram_keys);
  ngram_keys = ngram_keys[na_idx];
  unigram_keys = unigram_keys[na_idx];

  // Create unordered_map, using unigram_dups as keys, values will be the
  // indices of each dup in unigram_keys.
  std::vector<std::string> dups = as<std::vector<std::string> >(unigram_dups);
  refinr_map unigram_map = create_map(unigram_keys, dups);

  // Iterate over unigram_dups, for each value get the corresponding indices
  // from unigram_map, Use those indices to subset ngram_keys, save the subset
  // to the list output.
  std::vector<std::string>::iterator dups_end = dups.end();
  std::vector<std::string>::iterator iter;
  int i = 0;

  for(iter = dups.begin(); iter != dups_end; ++iter) {
    // Create subset of ngram_keys using the indices from unigram_map that
    // correspond to the current unigram_dup iteration.
    std::vector<int> curr_idx = unigram_map[*iter];
    int curr_idx_len = curr_idx.size();
    CharacterVector curr_ngram(curr_idx_len);
    for(int n = 0; n < curr_idx_len; ++n) {
      curr_ngram[n] = ngram_keys[curr_idx[n]];
    }

    out[i] = curr_ngram;
    i++;
  }

  return out;
}


// Filter stringdist matrices.
// Function takes a list of matrices of edit distances, each created by pkg
// stringdist. For each element of distmatrices, create clusters of matches
// within the matrix, based on lowest numeric edit distance. (matches must
// have a value below edit_threshold in order to be considered a cluster
// suitable for merging).
// [[Rcpp::export]]
List filter_initial_clusters(List distmatrices, double edit_threshold,
                             List clusters) {
  int distmatrices_len = distmatrices.size();
  List out(distmatrices_len);
  LogicalVector na_filter(distmatrices_len);
  String na_val = NA_STRING;

  for(int i = 0; i < distmatrices_len; ++i) {
    // Get current matrix object, establish other variables.
    NumericMatrix curr_mat = distmatrices[i];
    int mat_nrow = curr_mat.nrow();

    if(mat_nrow < 2) {
      CharacterVector curr_clust = clusters[i];
      out[i] = curr_clust;
      na_filter[i] = TRUE;
      continue;
    }

    DoubleVector lows(mat_nrow);
    IntegerVector olap(mat_nrow, 0);

    // For each row of curr_mat, get the min value present. If none are below
    // the edit_threshold, append NA to the output and move to the next
    // iteration. Also ID whether or not any of the rows of curr_mat have more
    // than one cluster match (ie a min value that repeats within any given
    // row).
    for(int row_idx = 0; row_idx < mat_nrow; ++row_idx) {
      DoubleVector curr_row = curr_mat(row_idx,_);
      LogicalVector row_bool(mat_nrow);
      for(int n = 0; n < mat_nrow; ++n) {
        if(n != row_idx) {
          row_bool[n] = TRUE;
        } else {
          row_bool[n] = FALSE;
        }
      }

      curr_row = curr_row[row_bool];
      double lowest = min(curr_row);
      lows[row_idx] = lowest;
      if(lowest < edit_threshold) {
        olap[row_idx] = sum(curr_row == lowest);
      }
    }

    // If none of the rows in curr_mat contain an edit distance value below
    // the edit_threshold, return NA and move on to the next iteration.
    if(is_true(all(lows > edit_threshold))) {
      out[i] = na_val;
      na_filter[i] = FALSE;
      continue;
    }

    // Trim obj olap to only include elements related to clusters that have an
    // edit distance less than edit_threshold.
    olap = olap[olap > 0];

    // Get indices of obj lows that are less than edit_threshold.
    IntegerVector lows_idx = seq(0, mat_nrow - 1);
    lows_idx = lows_idx[lows < edit_threshold];
    int lows_idx_len = lows_idx.size();

    // Generate clusters of char keys based on the edit distance matrix values.
    List clust(lows_idx_len);
    LogicalVector trim_idx(lows_idx_len, TRUE);
    CharacterVector curr_clust = clusters[i];
    for(int n = 0; n < lows_idx_len; ++n) {
      CharacterVector terms = curr_clust[curr_mat(lows_idx[n], _) < edit_threshold];
      clust[n] = unique(terms);
      // Check to see if terms is a complete subset of an existing cluster.
      if(n > 0) {
        for(int k = 0; k < n; ++k) {
          if(complete_intersect(terms, clust[k])) {
            trim_idx[n] = FALSE;
            break;
          }
        }
      }
    }

    // trim obj's clust and olap to only include unique clusters.
    clust = clust[trim_idx];
    olap = olap[trim_idx];

    // If any rows of curr_mat have a min edit distance that repeats,
    // eliminate any clusters that are complete subsets of the longest
    // cluster of the group.
    int clust_len = clust.size();
    if(sum(olap > 1) > 0 and clust_len > 1) {
      NumericVector lens_of_clusts(clust_len);
      for(int n = 0; n < clust_len; ++n) {
        lens_of_clusts[n] = clust.size();
      }

      // Eliminate any clusters that are complete subsets of the longest
      // cluster of the group.
      int max_clust_idx = which_max(lens_of_clusts);
      CharacterVector max_clust = clust[max_clust_idx];
      LogicalVector clust_bool(clust_len);

      for(int n = 0; n < clust_len; ++n) {
        CharacterVector curr_clust = clust[n];
        if(n == max_clust_idx) {
          clust_bool[n] = TRUE;
          continue;
        }

        clust_bool[n] = !complete_intersect(curr_clust, max_clust);
      }

      clust = clust[clust_bool];
    }

    out[i] = clust;
    na_filter[i] = TRUE;
  }

  // Subset to remove empty elements of the output list.
  out = out[na_filter];

  // Flatten nested lists so that each element of clusters is a char vector.
  out = cpp_flatten_list(out);

  return out;
}


// Takes a list of character vectors as input. Each char vector will be a
// string that's been tokenized by individual char. This function iterates
// over the list, for each char vector it will compile every available ngram
// of length equal to arg numgram. Output is a list of ngrams as char vectors.
// [[Rcpp::export]]
List char_ngram(List vects, int numgram) {
  int vects_len = vects.size();
  List out(vects_len);
  int numgram_sub = numgram - 1;

  for(int i = 0; i < vects_len; ++i) {
    CharacterVector curr_vect = vects[i];
    int curr_vect_len = curr_vect.size() - numgram_sub;
    if(curr_vect_len <= 0) {
      out[i] = NA_STRING;
      continue;
    }
    CharacterVector curr_out(curr_vect_len);
    for(int j = 0; j < curr_vect_len; ++j) {
      IntegerVector idx = seq(j, j + numgram_sub);
      CharacterVector curr_out_sub = curr_vect[idx];
      curr_out[j] = collapse(curr_out_sub);
    }
    out[i] = curr_out;
  }

  return(out);
}


// Takes a list of character vectors. For each vector: Get char ngram tokens,
// reduce to unique tokens, then collapse the tokens back together into single
// string.
// [[Rcpp::export]]
CharacterVector cpp_get_char_ngrams(List vects, int numgram) {
  List vects_mod = clone(vects);

  // Get character ngrams for each element of vects.
  if(numgram > 1) {
    vects_mod = char_ngram(vects_mod, numgram);
  }

  // For each element of vects, reduce values down to uniques, then sort
  // values alphabetically.
  vects_mod = cpp_list_unique(vects_mod, TRUE);

  // For each element of vects, combine all ngram strings into a single string,
  // equivelant to calling r func paste(char_vect, collapse = "") on each
  // element of vects.
  CharacterVector out = cpp_paste_list(vects_mod, "");

  return(out);
}
