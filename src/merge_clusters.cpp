#include <Rcpp.h>
#include"refinr_header.h"
using namespace Rcpp;

//' Merge clusters of similar values.
//'
//' Function that performs all merges related to input value clusters.
//' @param clusters character vector
//' @param keys character vector
//' @param vect character vector
//' @param keyssub character vector
//' @param vectsub character vector
// [[Rcpp::export]]
CharacterVector merge_clusters(CharacterVector clusters,
                               CharacterVector keys,
                               CharacterVector vect,
                               CharacterVector keyssub,
                               CharacterVector vectsub) {
  int clust_len = clusters.size();
  int keyssub_len = keyssub.size();
  int keys_len = keys.size();

  for(int i = 0; i < clust_len; ++i) {
    String clust(1);
    clust = clusters[i];
    LogicalVector match_bool_keyssub(keyssub_len);
    CharacterVector vectsub_clust;
    String most_freq_string(1);
    LogicalVector match_bool_keys(keys_len);

    match_bool_keyssub = equality(keyssub, clust);
    vectsub_clust = vectsub[match_bool_keyssub];

    most_freq_string = most_freq(clust, keyssub, vectsub);
    match_bool_keys = equality(keys, clust);
    for(int n = 0; n < keys_len; ++n) {
      if ( match_bool_keys[n] ) {
        vect[n] = most_freq_string;
      }
    }
  }
  return vect;
}
