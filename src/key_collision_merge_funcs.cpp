#include <Rcpp.h>
#include"refinr_header.h"
using namespace Rcpp;


// Wrapper for the two KC merge functions (one with a data dict, one without).
// [[Rcpp::export]]
CharacterVector merge_KC_clusters(CharacterVector vect,
                                  CharacterVector keys_vect,
                                  CharacterVector dict,
                                  CharacterVector keys_dict) {
  if(is_true(all(is_na(dict)))) {
    // If dict is NA, get vector of all key values that have at least one
    // duplicate within keys (this creates clusters). Then for each cluster,
    // make mass edits to the values of vect related to that cluster.
    CharacterVector clusters = cpp_get_key_dups(keys_vect);
    return merge_KC_clusters_no_dict(clusters, vect, keys_vect);
  } else {
    // If dict is not NA, get all key_vect values that have:
    // 1. At least one duplicate within key_vect, AND/OR
    // 2. At least one matching value within key_dict.
    // Then for each cluster, make mass edits to the values of vect related to
    // that cluster.
    CharacterVector both_keys(keys_vect.size() + keys_dict.size());
    int i=0;
    for( ; i < keys_vect.size(); i++) both_keys[i] = keys_vect[i] ;
    for(int j = 0; j < keys_dict.size(); i++, j++) both_keys[i] = keys_dict[j];
    CharacterVector clusters = cpp_get_key_dups(both_keys);
    return merge_KC_clusters_dict(clusters, vect, keys_vect, dict, keys_dict);
  }
}


// Merge key collision clusters of similar values, when no reference dict was
// passed to func "key_collision_merge".
// [[Rcpp::export]]
CharacterVector merge_KC_clusters_no_dict(CharacterVector clusters,
                                          CharacterVector vect,
                                          CharacterVector keys_vect) {
  int clust_len = clusters.size();
  int keys_len = keys_vect.size();
  CharacterVector output = clone(vect);

  // Get Logical vector indicating which keys appear in the clusters vector.
  LogicalVector keys_in_clusters = cpp_in(keys_vect, clusters);

  // Create subsets of vect and keys_vect based on which elements of each
  // contain at least one duplicate.
  CharacterVector vect_sub = vect[keys_in_clusters];
  CharacterVector keys_vect_sub = keys_vect[keys_in_clusters];

  // Iterate over clusters, make mass edits to output.
  for(int i = 0; i < clust_len; ++i) {
    String clust = clusters[i];

    // Get indices in which clust appears in keys_vect_sub.
    LogicalVector matches_keys_vect_sub = equality(keys_vect_sub, clust);

    // Subset vect_sub by indices in matches_keys_vect_sub.
    CharacterVector curr_vect = vect_sub[matches_keys_vect_sub];

    // If the number of unique values in curr_vect is greater than one,
    // continue on with the current iteration.
    if(unique(curr_vect).size() > 1){

      // Get the string that appears most often in curr_vect.
      String most_freq_string = curr_vect[which_max(table(curr_vect))];

      // Get indices in which clust appears in keys_vect.
      LogicalVector matches_keys_vect = equality(keys_vect, clust);

      // For each TRUE index of matches_keys_vect, edit output to be equal to
      // most_freq_string.
      for(int n = 0; n < keys_len; ++n) {
        if (matches_keys_vect[n]) {
          output[n] = most_freq_string;
        }
      }
    }
  }

  return output;
}


// Merge key collision clusters of similar values, when a reference dict was
// passed to func "key_collision_merge".
// [[Rcpp::export]]
CharacterVector merge_KC_clusters_dict(CharacterVector clusters,
                                       CharacterVector vect,
                                       CharacterVector keys_vect,
                                       CharacterVector dict,
                                       CharacterVector keys_dict) {
  int clust_len = clusters.size();
  int keys_vect_len = keys_vect.size();
  int keys_dict_len = keys_dict.size();
  CharacterVector output = clone(vect);

  // Get Logical vector indicating which keys appear in the clusters vector.
  LogicalVector keys_in_clusters = cpp_in(keys_vect, clusters);

  // Create subsets of vect and keys_vect based on which elements of each
  // contain at least one duplicate.
  CharacterVector vect_sub = vect[keys_in_clusters];
  CharacterVector keys_vect_sub = keys_vect[keys_in_clusters];

  // Iterate over clusters, make mass edits to output.
  for(int i = 0; i < clust_len; ++i) {
    String clust = clusters[i];

    // Get indices in which clust appears in keys_vect_sub.
    LogicalVector matches_keys_vect_sub = equality(keys_vect_sub, clust);

    // Subset vect_sub and dict by indices in matches_keys_vect_sub.
    CharacterVector curr_vect = vect_sub[matches_keys_vect_sub];

    // Subset dict by indices in which clust appears in keys_dict.
    CharacterVector curr_dict = dict[equality(keys_dict, clust)];

    // If the sum of unique values in curr_vect and unique values in curr_dict
    // is greater than one, continue on with the current iteration.
    if(unique(curr_vect).size() + unique(curr_dict).size() > 1) {
      // Establish variables.
      String most_freq_string(1);
      bool not_in_dict = true;

      // Look to see if clust exists in the dictionary. If so, use that value
      // as the edit value for all members of this cluster.
      int curr_dict_len = curr_dict.size();
      if(curr_dict_len > 0) {
        not_in_dict = false;
        if(curr_dict_len == 1) {
          most_freq_string = curr_dict[0];
        } else {
          most_freq_string = curr_dict[which_max(table(curr_dict))];
        }
      }

      // If no matching value was found in the dictionary, get the most freq
      // value within vect_sub related to this cluster.
      if (not_in_dict) {
        most_freq_string = curr_vect[which_max(table(curr_vect))];
      }

      // Get indices in which clust appears in keys_vect.
      LogicalVector matches_keys_vect = equality(keys_vect, clust);

      // For each TRUE index of matches_keys_vect, edit output to be equal to
      // most_freq_string.
      for(int n = 0; n < keys_vect_len; ++n) {
        if(matches_keys_vect[n]) {
          output[n] = most_freq_string;
        }
      }
    }
  }

  return output;
}
