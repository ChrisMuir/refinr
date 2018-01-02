#include <Rcpp.h>
#include"refinr_header.h"
using namespace Rcpp;


// Iterate over all clusters, make mass edits related to each cluster.
// [[Rcpp::export]]
CharacterVector merge_ngram_clusters(List clusters,
                                     CharacterVector n_gram_keys,
                                     CharacterVector univect,
                                     CharacterVector vect) {
  int clusters_len = clusters.size();
  int vect_len = vect.size();
  int n_gram_keys_len = n_gram_keys.size();
  CharacterVector output = clone(vect);

  for(int i = 0; i < clusters_len; ++i) {
    CharacterVector curr_clust = clusters[i];
    int curr_clust_len = curr_clust.size();

    // Get indices of elements of n_gram_keys that are found in cluster, as obj
    // "ng_idx".
    LogicalVector ng_idx(n_gram_keys_len);

    for(int n = 0; n < curr_clust_len; ++n) {
      LogicalVector clust_match_bool = equality(n_gram_keys, curr_clust[n]);
      ng_idx = ng_idx | clust_match_bool;
    }

    // If there is no intersection between n_gram_keys and curr_clust, stop
    // the current iteration and move on to the next element of clusters.
    if(is_false(any(ng_idx == TRUE))) {
      continue;
    }

    // Get indices of elements of vect that are found in vector
    // univect[ng_idx], as obj "vect_idx".
    CharacterVector univect_sub = univect[ng_idx];
    int univect_sub_len = univect_sub.size();
    LogicalVector vect_idx(vect_len);

    for(int n = 0; n < univect_sub_len; ++n) {
      LogicalVector vect_match_bool = equality(vect, univect_sub[n]);
      vect_idx = vect_idx | vect_match_bool;
    }

    // If there is no intersection between univect[nq_idx] and vect, stop the
    // current iteration and move on to the next element of clusters.
    if (is_false(any(vect_idx == TRUE))) {
      continue;
    }

    // Find the string that appears most frequently in vector vect[vect_idx],
    // as obj "most_freq_string". Edit all elements of vect[vect_idx] to be
    // equal to string "most_freq_string".
    CharacterVector vect_sub = vect[vect_idx];
    CharacterVector uni_vect_sub = sort_unique(vect_sub);
    CharacterVector freq = Rcpp::wrap(table(vect_sub));
    int freq_len = freq.size();
    IntegerVector freq_int(freq_len);
    for(int n = 0; n < freq_len; ++n) {
      freq_int[n] = atoi(freq[n]);
    }
    String most_freq_string = uni_vect_sub[which_max(freq_int)];

    // Make mass edits to vect.
    for(int n = 0; n < vect_len; ++n) {
      if(vect_idx[n]) {
        output[n] = most_freq_string;
      }
    }
  }
  return output;
}


// Get initial ngram clusters.
// For each string in unigram_dups, find indices in which that string appears
// in unigram_keys, then use those indices to get a subset of ngram_keys. Add
// the subset to List "out". After "out" is compiled, remove elements of the
// list that have length less than 2.
// [[Rcpp::export]]
List get_ngram_initial_clusters(CharacterVector ngram_keys,
                                CharacterVector unigram_keys,
                                CharacterVector unigram_dups) {
  int dups_len = unigram_dups.size();
  LogicalVector clust_len_logical(dups_len);
  List out(dups_len);

  for(int i = 0; i < dups_len; ++i) {
    CharacterVector clust = ngram_keys[equality(unigram_keys,
                                                unigram_dups[i])];
    clust = clust[!is_na(clust)];
    if(clust.size() > 1) {
      clust_len_logical[i] = TRUE;
    } else {
      clust_len_logical[i] = FALSE;
    }
    out[i] = clust;
  }
  return out[clust_len_logical];
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
  String na_val = NA_STRING;

  for(int i = 0; i < distmatrices_len; ++i) {
    // Get current matrix object, establish other variables.
    NumericMatrix curr_mat = distmatrices[i];
    int mat_nrow = curr_mat.nrow();
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
      continue;
    }

    // Trim obj olap to only include elements related to clusters that have an
    // edit distance less than edit_threshold.
    olap = olap[olap > 0];

    // Get indices of obj lows that are less than edit_threshold.
    IntegerVector lows_idx = Rcpp::seq(0, mat_nrow - 1);
    lows_idx = lows_idx[lows < edit_threshold];
    int lows_idx_len = lows_idx.size();

    // Generate clusters of char keys based on the edit distance matrix values.
    List clust(lows_idx_len);
    LogicalVector trim_idx(lows_idx_len, TRUE);
    CharacterVector curr_clust = clusters[i];
    for(int n = 0; n < lows_idx_len; ++n) {
      CharacterVector terms = curr_clust[curr_mat(lows_idx[n], _) < edit_threshold];
      clust[n] = terms;
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

    // trim obbj's clust and olap to only include unique clusters.
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

        clust_bool[n] = is_false(all(cpp_in(curr_clust, max_clust)));
      }

      clust = clust[clust_bool];
    }

    out[i] = clust;
  }

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
      NumericVector idx(numgram);
      idx = seq(j, j+numgram_sub);
      CharacterVector curr_out_sub = curr_vect[idx];
      curr_out[j] = collapse(curr_out_sub);
    }
    out[i] = curr_out;
  }
  return(out);
}


//
// [[Rcpp::export]]
CharacterVector cpp_get_char_ngrams(List vects, int numgram) {
  List vects_mod = clone(vects);
  int vects_len = vects.size();
  CharacterVector out(vects_len);

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
  out = cpp_paste_collapse_list(vects_mod);

  return(out);
}
