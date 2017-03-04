#include <Rcpp.h>
#include"refinr_header.h"
using namespace Rcpp;

//' Find the string that's most freqent in a vector.
//'
//' Given a cluster suitable for merging, find all instances within vectsub
//' that coorspond with that cluster and get the most freqently occuring
//' value (this is basically using freq to choose the value from the original
//' data that will be used as the template for all elements of a single cluster).
//' eg, if we have a cluster value that coorespond with these values from the
//' original data: c("Bob's Pizza", "bobs pizza", "Bob's Pizza"), then all those
//' values would be edited to be "Bob's Pizza", since its the most frequent.
//'
//' @param clust character string
//' @param keyssub character vector
//' @param vectsub character vector
// [[Rcpp::export]]
String most_freq(String clust,
                 CharacterVector keyssub,
                 CharacterVector vectsub) {
  int keyssub_len = keyssub.size();
  LogicalVector match_bool(keyssub_len);
  CharacterVector vectsub_clust;

  match_bool = equality(keyssub, clust);
  vectsub_clust = vectsub[match_bool];

  CharacterVector univect;
  CharacterVector freq;
  univect = Rcpp::wrap( sort_unique( vectsub_clust ));
  freq = Rcpp::wrap( table( vectsub_clust ));
  int freq_len = freq.size();
  IntegerVector freq_int(freq_len);
  for(int i = 0; i < freq_len; ++i) {
    freq_int[i] = atoi(freq[i]);
  }
  return univect[which_max(freq_int)];
}
