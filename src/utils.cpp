#include <Rcpp.h>
#include"refinr_header.h"
using namespace Rcpp;


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
  int keys_sub_len = keys_sub.size();
  LogicalVector match_bool(keys_sub_len);
  CharacterVector vect_sub_clust;

  match_bool = equality(keys_sub, clust);
  vect_sub_clust = vect_sub[match_bool];

  CharacterVector univect;
  CharacterVector freq;
  univect = Rcpp::wrap( sort_unique( vect_sub_clust ));
  freq = Rcpp::wrap( table( vect_sub_clust ));
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
