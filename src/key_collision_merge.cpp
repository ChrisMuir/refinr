#include <Rcpp.h>
#include"refinr.h"
using namespace Rcpp;


// Wrapper for the two KC merge functions (one with a data dict, one without).
// [[Rcpp::export]]
CharacterVector merge_KC_clusters(CharacterVector vect,
                                  CharacterVector keys_vect,
                                  CharacterVector dict,
                                  CharacterVector keys_dict) {
  if(CharacterVector::is_na(dict[0])) {
    // If dict is NA, get vector of all key values that have at least one
    // duplicate within keys (this creates clusters). The "merge_" func will
    // make mass edits to the values of vect related to that cluster.
    CharacterVector clusters = cpp_get_key_dups(keys_vect);
    return merge_KC_clusters_no_dict(clusters, vect, keys_vect);
  } else {
    // If dict is not NA, get all key_vect values that have:
    // 1. At least one duplicate within key_vect, AND/OR
    // 2. At least one matching value within key_dict.
    // The "merge_" func will make mass edits to the values of vect related to
    // that cluster.
    CharacterVector both_keys(keys_vect.size() + keys_dict.size());
    int i = 0;
    for( ; i < keys_vect.size(); i++) both_keys[i] = keys_vect[i];
    for(int j = 0; j < keys_dict.size(); i++, j++) both_keys[i] = keys_dict[j];
    CharacterVector clusters = cpp_get_key_dups(both_keys);
    return merge_KC_clusters_dict(clusters, vect, keys_vect, dict, keys_dict);
  }
}


// Merge key collision clusters of similar values, when no reference dict was
// passed to func "key_collision_merge".
CharacterVector merge_KC_clusters_no_dict(CharacterVector clusters,
                                          CharacterVector vect,
                                          CharacterVector keys_vect) {
  // Create copy of vect to use as the output vector.
  CharacterVector output = clone(vect);

  // Create unordered_map, using clusters as keys, values will be the indices
  // of each cluster in keys_vect.
  std::vector<std::string> cl = as<std::vector<std::string> >(clusters);
  refinr_map keys_vect_map = create_map(keys_vect, cl);

  // Initialize variables used in the loop below.
  std::vector<int> curr_idx;
  int curr_idx_len;
  String most_freq_string;

  // Iterate over clusters, make mass edits to output.
  std::vector<std::string>::iterator clust_end = cl.end();
  std::vector<std::string>::iterator iter;

  for(iter = cl.begin(); iter != clust_end; ++iter) {
    // Create subset of vect using the indices from keys_vect_map that
    // correspond to the current cluster iteration.
    curr_idx = keys_vect_map[*iter];
    curr_idx_len = curr_idx.size();
    CharacterVector curr_vect(curr_idx_len);
    for(int i = 0; i < curr_idx_len; ++i) {
      curr_vect[i] = vect[curr_idx[i]];
    }

    // Get the string that appears most often in curr_vect.
    most_freq_string = most_freq_str(curr_vect);

    // For each index in curr_idx, edit output to be equal to most_freq_string.
    for(int n = 0; n < curr_idx_len; ++n) {
      output[curr_idx[n]] = most_freq_string;
    }
  }

  return output;
}


// Merge key collision clusters of similar values, when a reference dict was
// passed to func "key_collision_merge".
CharacterVector merge_KC_clusters_dict(CharacterVector clusters,
                                       CharacterVector vect,
                                       CharacterVector keys_vect,
                                       CharacterVector dict,
                                       CharacterVector keys_dict) {
  // Create copy of vect to use as the output vector.
  CharacterVector output = clone(vect);

  // Create two unordered_maps, both using clusters as keys. Values for
  // keys_vect_map will be the indices of each cluster in keys_vect. Values for
  // keys_dict_map will be the indices of each cluster in keys_dict.
  std::vector<std::string> cl = as<std::vector<std::string> >(clusters);
  refinr_map keys_vect_map = create_map(keys_vect, cl);
  refinr_map keys_dict_map = create_map(keys_dict, cl);

  // Initialize variables used in the loop below.
  std::string curr_clust;
  std::vector<int> curr_vect_idx;
  int curr_vect_len;
  std::vector<int> curr_dict_idx;
  int curr_dict_len;
  bool not_in_dict;
  String most_freq_string;

  // Iterate over clusters, make mass edits to output.
  std::vector<std::string>::iterator clust_end = cl.end();
  std::vector<std::string>::iterator iter;

  for(iter = cl.begin(); iter != clust_end; ++iter) {
    curr_clust = *iter;

    // Create subset of vect using the indices that correspond to curr_clust.
    curr_vect_idx = keys_vect_map[curr_clust];
    curr_vect_len = curr_vect_idx.size();
    CharacterVector curr_vect(curr_vect_len);
    for(int i = 0; i < curr_vect_len; ++i) {
      curr_vect[i] = vect[curr_vect_idx[i]];
    }

    // Check to see if curr_clust appears in keys_dict_map. If it does, create
    // subset of dict using the indices that correspond to curr_clust.
    curr_dict_idx = keys_dict_map[curr_clust];
    curr_dict_len = curr_dict_idx.size();
    CharacterVector curr_dict(curr_dict_len);
    not_in_dict = true;
    if(curr_dict_len > 0) {
      not_in_dict = false;
      for(int i = 0; i < curr_dict_len; ++i) {
        curr_dict[i] = dict[curr_dict_idx[i]];
      }
    }

    // Establish most_freq_string. If curr_clust exists in curr_dict, get
    // most_freq_string from curr_dict. Otherwise get most_freq_string from
    // curr_vect.
    if(not_in_dict) {
      most_freq_string = most_freq_str(curr_vect);
    } else {
      if(curr_dict_len == 1) {
        most_freq_string = curr_dict[0];
      } else {
        most_freq_string = most_freq_str(curr_dict);
      }
    }

    // For each index in curr_vect_idx, edit output to be equal to
    // most_freq_string.
    for(int n = 0; n < curr_vect_len; ++n) {
      output[curr_vect_idx[n]] = most_freq_string;
    }
  }

  return output;
}
