#include <Rcpp.h>
#include"refinr.h"
using namespace Rcpp;


// Iterate over all clusters, make mass edits to obj "vect", related to each
// cluster.
CharacterVector merge_ngram_clusters(List clusters,
                                     const CharacterVector &n_gram_keys,
                                     const CharacterVector &univect,
                                     const CharacterVector &vect) {
  CharacterVector output = clone(vect);

  // Unlist clusters to a char vector, and get uniques.
  CharacterVector clust_unlist = unique(cpp_unlist(clusters));

  // Create maps
  std::vector<std::string> cl_ul = as<std::vector<std::string> >(clust_unlist);
  std::vector<std::string> uni = as<std::vector<std::string> >(univect);
  refinr_map ngram_map = create_map(n_gram_keys, cl_ul);
  refinr_map univect_map = create_map(vect, uni);

  // initialize iterators.
  List::iterator clust_end = clusters.end();
  List::iterator iter;

  // Initialize variables used throughout the loop below.
  CharacterVector curr_clust;
  String most_freq_string;
  int curr_clust_len;
  int ngram_idx_len;
  int univect_sub_len;
  int uni_idx_len;
  std::string curr_clust_str;
  std::vector<int> uni_idx;
  std::vector<int> curr_uni_idx;
  std::vector<int> ngram_idx;
  std::vector<int> curr_ngram_idx;

  for(iter = clusters.begin(); iter != clust_end; ++iter) {
    curr_clust = *iter;

    // Create subset of univect using the indices of n_gram_keys that appear
    // in curr_clust.
    curr_clust_len = curr_clust.size();
    ngram_idx.clear();
    for(int i = 0; i < curr_clust_len; ++i) {
      curr_clust_str = as<std::string>(curr_clust[i]);
      curr_ngram_idx = ngram_map[curr_clust_str];
      ngram_idx.insert(
        ngram_idx.end(), curr_ngram_idx.begin(), curr_ngram_idx.end()
      );
    }
    ngram_idx_len = ngram_idx.size();
    std::vector<std::string> univect_sub(ngram_idx_len);
    for(int i = 0; i < ngram_idx_len; ++i) {
      univect_sub[i] = uni[ngram_idx[i]];
    }

    // Create subset of vect using the elements of univect_sub.
    univect_sub_len = univect_sub.size();
    uni_idx.clear();
    for(int i = 0; i < univect_sub_len; ++i) {
      curr_uni_idx = univect_map[univect_sub[i]];
      uni_idx.insert(
        uni_idx.end(), curr_uni_idx.begin(), curr_uni_idx.end()
      );
    }
    uni_idx_len = uni_idx.size();
    CharacterVector vect_sub(uni_idx_len);
    for(int i = 0; i < uni_idx_len; ++i) {
      vect_sub[i] = vect[uni_idx[i]];
    }

    // Find the string that appears most frequently in vect_sub.
    most_freq_string = most_freq_str(vect_sub);

    // Edit all elements of output[uni_idx] to be equal to most_freq_string.
    for(int n = 0; n < uni_idx_len; ++n) {
      output[uni_idx[n]] = most_freq_string;
    }
  }

  return output;
}


// Prep steps prior to the merging of clusters, given that approximate string
// matching is NOT being used (via arg edit_threshold). Generate clusters by
// finding all elements of n_gram_keys that have one or more identical
// matches, then pass args along to merge_ngram_clusters().
// [[Rcpp::export]]
CharacterVector ngram_merge_no_approx(const CharacterVector &n_gram_keys,
                                      const CharacterVector &univect,
                                      const CharacterVector &vect) {
  // Find all elements of n_gram_keys that have one or more identical
  // matches.
  CharacterVector n_gram_keys_dups = cpp_get_key_dups(n_gram_keys);

  // If no duplicated keys exist, return vect unedited.
  if(n_gram_keys_dups.size() == 0) {
    return(vect);
  }

  // Cast n_gram_keys_dups as a list.
  List clusters = cpp_as_list(n_gram_keys_dups);

  // Pass clusters and other args along to merge_ngram_clusters().
  return(merge_ngram_clusters(clusters, n_gram_keys, univect, vect));
}


// Prep steps prior to the merging of clusters, given that approximate string
// matching is being used (via arg edit_threshold).
// Create initial clusters, filter clusters based on matrices of numeric
// string edit distance values, then pass args along to merge_ngram_clusters().
// [[Rcpp::export]]
CharacterVector ngram_merge_approx(const CharacterVector &n_gram_keys,
                                   const CharacterVector &one_gram_keys,
                                   const CharacterVector &univect,
                                   const CharacterVector &vect,
                                   const double &edit_threshold,
                                   SEXP method, SEXP weight, SEXP p, SEXP bt,
                                   SEXP q, SEXP useBytes, SEXP nthread) {
  // Get initial clusters.
  List initial_clust = get_ngram_initial_clusters(n_gram_keys, one_gram_keys);

  // Create an edit distance matrix for each initial cluster, using the
  // function "sd_lower_tri()" from the stringdist package.
  List distmatrices = get_stringdist_matrices(initial_clust, method, weight, p,
                                              bt, q, useBytes, nthread);

  // For each matrix in distmatrices, create clusters of matches within the
  // matrix, based on lowest numeric edit distance (matches must have a value
  // below edit_threshold in order to be considered suitable for merging).
  List clusters = filter_initial_clusters(distmatrices, edit_threshold,
                                          initial_clust);

  // If length of clusters is zero, return vect unedited.
  if(clusters.size() == 0) return(vect);

  // Pass args along to merge_ngram_clusters().
  return(merge_ngram_clusters(clusters, n_gram_keys, univect, vect));
}


// Get initial ngram clusters.
// For each string in unigram_dups, find indices in which that string appears
// in unigram_keys, then use those indices to get a subset of ngram_keys. Add
// the subset to List "out". After "out" is compiled, remove elements of the
// list that have length less than 2.
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
  int curr_idx_len;
  std::vector<int> curr_idx;

  for(iter = dups.begin(); iter != dups_end; ++iter) {
    // Create subset of ngram_keys using the indices from unigram_map that
    // correspond to the current unigram_dup iteration.
    curr_idx = unigram_map[*iter];
    curr_idx_len = curr_idx.size();
    CharacterVector curr_ngram(curr_idx_len);
    for(int n = 0; n < curr_idx_len; ++n) {
      curr_ngram[n] = ngram_keys[curr_idx[n]];
    }

    out[i] = curr_ngram;
    i++;
  }

  return out;
}


// Take in a list of character vectors, create a stringdist edit matrix for
// each vector, return a list of edit matrices.
List get_stringdist_matrices(const List &clusters, SEXP method, SEXP weight, SEXP p,
                             SEXP bt, SEXP q, SEXP useBytes, SEXP nthread) {
  int clust_len = clusters.size();
  List out(clust_len);

  for(int j = 0; j < clust_len; ++j) {
    SEXP curr_clust = clusters[j];
    // Run args through stringdist sd_lower_tri C function.
    NumericVector x = stringdist_lower_tri(curr_clust, method, weight, p,
                                           bt, q, useBytes, nthread);
    // Initialize output matrix.
    int mat_dim = Rf_xlength(curr_clust);
    NumericMatrix mat(mat_dim, mat_dim);

    int iter_len = mat_dim - 1;
    int x_val = 0;

    // Loop to fill in the lower and upper tri of mat with the numeric values
    // of x.
    for(int i = 0; i < iter_len; ++i) {
      for(int n = i + 1; n < mat_dim; ++n) {
        // Fill in lower tri.
        mat(n,i) = x[x_val];
        // Fill in upper tri.
        mat(i,n) = x[x_val];
        //Bump x_val.
        x_val++;
      }
    }

    out[j] = mat;
  }

  return(out);
}


// Filter stringdist matrices.
// Function takes a list of matrices of edit distances, each created by pkg
// stringdist. For each element of distmatrices, create clusters of matches
// within the matrix, based on lowest numeric edit distance. (matches must
// have a value below edit_threshold in order to be considered a cluster
// suitable for merging).
List filter_initial_clusters(const List &distmatrices, const double &edit_threshold,
                             const List &clusters) {
  int distmatrices_len = distmatrices.size();
  List out(distmatrices_len);
  LogicalVector na_filter(distmatrices_len);

  CharacterVector curr_clust;
  CharacterVector max_clust;
  CharacterVector terms;
  DoubleVector curr_row;
  IntegerVector lows_idx;
  double lowest;
  int lows_idx_len;
  int clust_len;
  int max_clust_idx;
  int mat_nrow;

  for(int i = 0; i < distmatrices_len; ++i) {
    // Get current matrix object, establish other variables.
    NumericMatrix curr_mat = distmatrices[i];
    mat_nrow = curr_mat.nrow();

    if(mat_nrow < 2) {
      curr_clust = clusters[i];
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
      curr_row = curr_mat(row_idx,_);
      LogicalVector row_bool(mat_nrow);
      for(int n = 0; n < mat_nrow; ++n) {
        if(n != row_idx) {
          row_bool[n] = TRUE;
        } else {
          row_bool[n] = FALSE;
        }
      }

      curr_row = curr_row[row_bool];
      lowest = min(curr_row);
      lows[row_idx] = lowest;
      if(lowest < edit_threshold) {
        olap[row_idx] = sum(curr_row == lowest);
      }
    }

    // If none of the rows in curr_mat contain an edit distance value below
    // the edit_threshold, return NA and move on to the next iteration.
    if(is_true(all(lows > edit_threshold))) {
      out[i] = NA_STRING;
      na_filter[i] = FALSE;
      continue;
    }

    // Trim obj olap to only include elements related to clusters that have an
    // edit distance less than edit_threshold.
    olap = olap[olap > 0];

    // Get indices of obj lows that are less than edit_threshold.
    lows_idx = seq(0, mat_nrow - 1);
    lows_idx = lows_idx[lows < edit_threshold];
    lows_idx_len = lows_idx.size();

    // Generate clusters of char keys based on the edit distance matrix values.
    List clust(lows_idx_len);
    LogicalVector trim_idx(lows_idx_len, TRUE);
    curr_clust = clusters[i];
    for(int n = 0; n < lows_idx_len; ++n) {
      terms = curr_clust[curr_mat(lows_idx[n], _) < edit_threshold];
      terms = unique(terms);
      clust[n] = terms;
      // Check to see if terms is a complete subset of an existing cluster.
      if(n > 0) {
        for(int k = 0; k < n; ++k) {
          if(cpp_all(terms, clust[k])) {
            trim_idx[n] = FALSE;
            break;
          }
        }
      }
    }

    // trim objs clust and olap to only include unique clusters.
    clust = clust[trim_idx];
    olap = olap[trim_idx];

    // If any rows of curr_mat have a min edit distance that repeats,
    // eliminate any clusters that are complete subsets of the longest
    // cluster of the group.
    clust_len = clust.size();
    if(sum(olap > 1) > 0 and clust_len > 1) {
      NumericVector lens_of_clusts(clust_len);
      for(int n = 0; n < clust_len; ++n) {
        lens_of_clusts[n] = clust.size();
      }

      // Eliminate any clusters that are complete subsets of the longest
      // cluster of the group.
      max_clust_idx = which_max(lens_of_clusts);
      max_clust = clust[max_clust_idx];
      LogicalVector clust_bool(clust_len);

      for(int n = 0; n < clust_len; ++n) {
        curr_clust = clust[n];
        if(n == max_clust_idx) {
          clust_bool[n] = TRUE;
          continue;
        }

        clust_bool[n] = !cpp_all(curr_clust, max_clust);
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
List char_ngram(const std::vector<std::string>& strings, int numgram) {
  int strings_len = strings.size();
  List out(strings_len);
  int numgram_sub = numgram - 1;

  IntegerVector idx;
  std::string curr_str;
  std::string curr_token;
  int curr_str_len;

  for(int i = 0; i < strings_len; ++i) {
    curr_str = strings[i];
    curr_str_len = curr_str.size() - numgram_sub;
    if(curr_str_len <= 0 or curr_str == "NA") {
      out[i] = NA_STRING;
      continue;
    }
    CharacterVector curr_out(curr_str_len);
    for(int j = 0; j < curr_str_len; ++j) {
      idx = seq(j, j + numgram_sub);
      curr_token.clear();
      for(int k = 0; k < numgram; ++k) {
        curr_token += curr_str[idx[k]];
      }
      curr_out[j] = curr_token;
    }
    out[i] = curr_out;
  }

  return(out);
}


// Takes a list of character vectors. For each vector: Get char ngram tokens,
// reduce to unique tokens, then collapse the tokens back together into single
// string.
// [[Rcpp::export]]
CharacterVector cpp_get_char_ngrams(const std::vector<std::string> &vects,
                                    int numgram) {

  // Get character ngrams for each element of vects.
  List vects_mod = char_ngram(vects, numgram);

  // For each element of vects, reduce values down to uniques, then sort
  // values alphabetically.
  vects_mod = cpp_list_unique(vects_mod, TRUE);

  // For each element of vects, combine all ngram strings into a single string,
  // equivelant to calling r func paste(char_vect, collapse = "") on each
  // element of vects.
  CharacterVector out = cpp_paste_list(vects_mod, "");

  return(out);
}
