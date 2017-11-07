#include <Rcpp.h>
#include"refinr_header.h"
using namespace Rcpp;

//' Function that performs all merges related to input value clusters. Can
//' perform merging using values from a dictionary vector.
//' @param clusters character vector
//' @param keys_vect character vector
//' @param vect character vector
//' @param keys_vect_sub character vector
//' @param vect_sub character vector
//' @param keys_dict character vector
//' @param dict character vector
// [[Rcpp::export]]
CharacterVector merge_KC_clusters_dict(CharacterVector clusters,
                                       CharacterVector keys_vect,
                                       CharacterVector vect,
                                       CharacterVector keys_vect_sub,
                                       CharacterVector vect_sub,
                                       CharacterVector keys_dict,
                                       CharacterVector dict) {
  int clust_len = clusters.size();
  int keys_vect_sub_len = keys_vect_sub.size();
  int keys_vect_len = keys_vect.size();
  int keys_dict_len = keys_dict.size();
  CharacterVector output = clone(vect);

  for(int i = 0; i < clust_len; ++i) {
    String clust(1);
    clust = clusters[i];
    LogicalVector match_bool_keys_vect_sub(keys_vect_sub_len);
    CharacterVector vectsub_clust;
    String match_string(1);
    LogicalVector match_bool_keys_vect(keys_vect_len);
    LogicalVector match_bool_keys_dict(keys_dict_len);
    bool not_in_dict(1);
    not_in_dict = true;

    match_bool_keys_vect_sub = equality(keys_vect_sub, clust);
    vectsub_clust = vect_sub[match_bool_keys_vect_sub];
    for(int n = 0; n < keys_dict_len; ++n) {
      if ( clust == keys_dict[n] ) {
        not_in_dict = false;
        match_string = dict[n];
        break;
      }
    }

    if ( not_in_dict ) {
      match_string = most_freq(clust, keys_vect_sub, vect_sub);
    }
    match_bool_keys_vect = equality(keys_vect, clust);
    for(int n = 0; n < keys_vect_len; ++n) {
      if ( match_bool_keys_vect[n] ) {
        output[n] = match_string;
      }
    }
  }
  return output;
}
