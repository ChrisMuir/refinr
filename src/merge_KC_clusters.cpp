#include <Rcpp.h>
#include"refinr_header.h"
using namespace Rcpp;

// Merge key collision clusters of similar values, when no reference dict was
// passed to func "key_collision_merge".
// [[Rcpp::export]]
CharacterVector merge_KC_clusters_no_dict(CharacterVector clusters,
                                          CharacterVector keys_vect,
                                          CharacterVector vect,
                                          CharacterVector keys_vect_sub,
                                          CharacterVector vect_sub) {
  int clust_len = clusters.size();
  int keys_len = keys_vect.size();
  CharacterVector output = clone(vect);

  for(int i = 0; i < clust_len; ++i) {
    String clust = clusters[i];
    String most_freq_string = most_freq(clust, keys_vect_sub, vect_sub);
    LogicalVector match_bool_keys = equality(keys_vect, clust);
    for(int n = 0; n < keys_len; ++n) {
      if (match_bool_keys[n]) {
        output[n] = most_freq_string;
      }
    }
  }
  return output;
}


// Merge key collision clusters of similar values, when a reference dict was
// passed to func "key_collision_merge".
// [[Rcpp::export]]
CharacterVector merge_KC_clusters_dict(CharacterVector clusters,
                                       CharacterVector keys_vect,
                                       CharacterVector vect,
                                       CharacterVector keys_vect_sub,
                                       CharacterVector vect_sub,
                                       CharacterVector keys_dict,
                                       CharacterVector dict) {
  int clust_len = clusters.size();
  int keys_vect_len = keys_vect.size();
  int keys_dict_len = keys_dict.size();
  CharacterVector output = clone(vect);

  for(int i = 0; i < clust_len; ++i) {
    // Establish variables.
    String clust = clusters[i];
    String match_string(1);
    bool not_in_dict = true;

    // Look to see if str clust exists in the dictionary. If so, use that value
    // as the edit value for all members of this cluster.
    for(int n = 0; n < keys_dict_len; ++n) {
      if(clust == keys_dict[n]) {
        not_in_dict = false;
        match_string = dict[n];
        break;
      }
    }

    // If no matching value was found in the dictionary, get the most freq
    // value within vect_sub related to this cluster.
    if (not_in_dict) {
      match_string = most_freq(clust, keys_vect_sub, vect_sub);
    }
    LogicalVector match_bool_keys = equality(keys_vect, clust);
    for(int n = 0; n < keys_vect_len; ++n) {
      if(match_bool_keys[n]) {
        output[n] = match_string;
      }
    }
  }
  return output;
}
