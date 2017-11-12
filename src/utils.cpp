#include <Rcpp.h>
#include"refinr_header.h"
using namespace Rcpp;

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
