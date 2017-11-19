#include <Rcpp.h>
#include"refinr_header.h"
using namespace Rcpp;


// Meant to mimic the R func %in%. Wrote it because I kept having
// "warning: comparison between signed and unsigned integer expressions [-Wsign-compare]"
// compile issues related to the Rcpp sugar function in().
// [[Rcpp::export]]
LogicalVector cpp_in(CharacterVector x, CharacterVector y) {
  int x_len = x.size();
  LogicalVector out(x_len);

  for(int i = 0; i < x_len; ++i) {
    out[i] = is_true(any(equality(y, x[i])));
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
          if(is_true(all(cpp_in(terms, clust[k])))) {
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


// Given a list of character vectors, return a list of the same length
// containing the unique values of eacch vector.
// [[Rcpp::export]]
List cpp_list_unique(List input, bool sort_vals) {
  int input_len = input.size();
  List out(input_len);

  if(sort_vals) {
    for(int i = 0; i < input_len; ++i) {
      CharacterVector curr_vect = input[i];
      out[i] = unique(curr_vect).sort();
    }
  } else {
    for(int i = 0; i < input_len; ++i) {
      CharacterVector curr_vect = input[i];
      out[i] = unique(curr_vect);
    }
  }
  return out;
}


// Given a list of character vectors, for each vector, remove any strings that
// appear in input vector "removes".
// [[Rcpp::export]]
List remove_strings(List input, CharacterVector removes) {
  int input_len = input.size();
  List out(input_len);

  for(int i = 0; i < input_len; ++i) {
    CharacterVector curr_vect = input[i];
    int curr_vect_len = curr_vect.size();
    LogicalVector curr_vect_matches(curr_vect_len);
    for(int j = 0; j < curr_vect_len; ++j) {
      LogicalVector matches = equality(removes, curr_vect[j]);
      curr_vect_matches[j] = is_false(any(matches == TRUE));
    }
    out[i] = curr_vect[curr_vect_matches];
  }
  return out;
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
    CharacterVector clust = ngram_keys[equality(unigram_keys, unigram_dups[i])];
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


// For each element of clusters, get the number of unique values within vect
// associated with that cluster.
// [[Rcpp::export]]
IntegerVector get_clust_size_no_dict(CharacterVector clusters,
                                     CharacterVector vect,
                                     CharacterVector keys) {
  int clust_len = clusters.size();
  IntegerVector out(clust_len);

  for(int i = 0; i < clust_len; ++i) {
    CharacterVector curr_vect = vect[equality(keys, clusters[i])];
    out[i] = unique(curr_vect).size();
  }
  return out;
}


// For each element of clusters, get the number of unique values within both
// vect and dict associated with that cluster.
// [[Rcpp::export]]
IntegerVector get_clust_size_dict(CharacterVector clusters,
                                  CharacterVector vect,
                                  CharacterVector keys_vect,
                                  CharacterVector dict,
                                  CharacterVector keys_dict) {
  int clust_len = clusters.size();
  IntegerVector out(clust_len);

  for(int i = 0; i < clust_len; ++i) {
    CharacterVector curr_vect = vect[equality(keys_vect, clusters[i])];
    CharacterVector curr_dict = dict[equality(keys_dict, clusters[i])];
    out[i] = unique(curr_vect).size() + unique(curr_dict).size();
  }
  return out;
}


// Given a cluster suitable for merging, find all instances within vect_sub
// that coorspond with that cluster and get the most freqently occuring
// value (this is basically using freq to choose the value from the original
// data that will be used as the template for all elements of a single cluster).
// eg, if we have a cluster value that correspond with these values from the
// original data: c("Bob's Pizza", "bobs pizza", "Bob's Pizza"), then all those
// values would be edited to be "Bob's Pizza", since its the most frequent.
// [[Rcpp::export]]
String most_freq(String clust,
                 CharacterVector keys_sub,
                 CharacterVector vect_sub) {
  LogicalVector match_bool = equality(keys_sub, clust);
  CharacterVector vect_sub_clust = vect_sub[match_bool];

  CharacterVector univect = sort_unique(vect_sub_clust);
  CharacterVector freq = Rcpp::wrap(table(vect_sub_clust));
  int freq_len = freq.size();
  IntegerVector freq_int(freq_len);
  for(int i = 0; i < freq_len; ++i) {
    freq_int[i] = atoi(freq[i]);
  }
  return univect[which_max(freq_int)];
}


// Compare all elements of a char vector to a char string.
//
// Takes a char vector and char string as input, output is a logical vector
// with length equal to the input char vector. This is equivalent to R code
// "vector == string".
// [[Rcpp::export]]
LogicalVector equality(CharacterVector lookupvect,
                       String charstring) {
  int vect_length = lookupvect.size();
  LogicalVector out(vect_length);

  for(int i = 0; i < vect_length; ++i) {
    out[i] = charstring == lookupvect[i];
  }
  return out;
}


// cpp version of R function duplicated()
// [[Rcpp::export]]
LogicalVector cpp_duplicated(CharacterVector vect) {
  return duplicated(vect);
}


// cpp version of R function unique(), but only for char vectors.
// [[Rcpp::export]]
CharacterVector cpp_unique(CharacterVector vect) {
  return unique(vect);
}


// cpp version of R function trimws()
// [[Rcpp::export]]
CharacterVector cpp_trimws(CharacterVector vect) {
  return trimws(vect);
}
