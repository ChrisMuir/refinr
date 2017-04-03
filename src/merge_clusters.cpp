#include <Rcpp.h>
#include"refinr_header.h"
using namespace Rcpp;

//' Merge clusters of similar values.
//'
//' Function that performs all merges related to input value clusters.
//' @param clusters character vector
//' @param keys_vect character vector
//' @param vect character vector
//' @param keys_vect_sub character vector
//' @param vect_sub character vector
// [[Rcpp::export]]
CharacterVector merge_clusters(CharacterVector clusters,
                               CharacterVector keys_vect,
                               CharacterVector vect,
                               CharacterVector keys_vect_sub,
                               CharacterVector vect_sub) {
  int clust_len = clusters.size();
  int keys_sub_len = keys_vect_sub.size();
  int keys_len = keys_vect.size();
  CharacterVector output = clone(vect);

  for(int i = 0; i < clust_len; ++i) {
    String clust(1);
    clust = clusters[i];
    LogicalVector match_bool_keys_sub(keys_sub_len);
    CharacterVector vectsub_clust;
    String most_freq_string(1);
    LogicalVector match_bool_keys(keys_len);

    match_bool_keys_sub = equality(keys_vect_sub, clust);
    vectsub_clust = vect_sub[match_bool_keys_sub];

    most_freq_string = most_freq(clust, keys_vect_sub, vect_sub);
    match_bool_keys = equality(keys_vect, clust);
    for(int n = 0; n < keys_len; ++n) {
      if ( match_bool_keys[n] ) {
        output[n] = most_freq_string;
      }
    }
  }
  return output;
}
