#include <Rcpp.h>
#include"refinr_header.h"
using namespace Rcpp;


// Merge key collision clusters of similar values, when no reference dict was
// passed to func "key_collision_merge".
// [[Rcpp::export]]
CharacterVector merge_KC_clusters_no_dict(CharacterVector clusters,
                                          CharacterVector keys_vect,
                                          CharacterVector vect,
                                          LogicalVector keys_in_clusters) {
  int clust_len = clusters.size();
  int keys_len = keys_vect.size();
  CharacterVector output = clone(vect);

  // Create subsets of vect and keys_vect based on which elements of each
  // contain at least one duplicate.
  CharacterVector vect_sub = vect[keys_in_clusters];
  CharacterVector keys_vect_sub = keys_vect[keys_in_clusters];

  // For each element of clusters, get the number of unique values within vect
  // associated with that cluster. Idea is to skip the merging step for all
  // elements of clusters for which each associated element of vect is already
  // identical. In those spots it's pointless to perform merging.
  IntegerVector csize(clust_len);
  for(int i = 0; i < clust_len; ++i) {
    CharacterVector curr_vect = vect_sub[equality(keys_vect_sub, clusters[i])];
    csize[i] = unique(curr_vect).size();
  }

  if(is_true(any(csize > 1))) {
    clusters = clusters[csize > 1];
    int clust_len = clusters.size();

    // Perform merging on all clusters with length greater than one.
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
  }

  return output;
}


// Merge key collision clusters of similar values, when a reference dict was
// passed to func "key_collision_merge".
// [[Rcpp::export]]
CharacterVector merge_KC_clusters_dict(CharacterVector clusters,
                                       CharacterVector keys_vect,
                                       CharacterVector vect,
                                       CharacterVector keys_dict,
                                       CharacterVector dict,
                                       LogicalVector keys_in_clusters) {
  int clust_len = clusters.size();
  int keys_vect_len = keys_vect.size();
  int keys_dict_len = keys_dict.size();
  CharacterVector output = clone(vect);

  // Create subsets of vect and keys_vect based on which elements of each
  // contain at least one duplicate.
  CharacterVector vect_sub = vect[keys_in_clusters];
  CharacterVector keys_vect_sub = keys_vect[keys_in_clusters];

  // For each element of clusters, get the number of unique values across both
  // vect AND dict associated with that cluster. Idea is to skip the merging
  // step for all elements of cluster for which each associated element of
  // vect is already identical (or identical to an element of dict). In those
  // spots it's pointless to perform merging.
  IntegerVector csize(clust_len);
  for(int i = 0; i < clust_len; ++i) {
    CharacterVector curr_vect = vect[equality(keys_vect, clusters[i])];
    CharacterVector curr_dict = dict[equality(keys_dict, clusters[i])];
    csize[i] = unique(curr_vect).size() + unique(curr_dict).size();
  }

  if(is_true(any(csize > 1))) {
    clusters = clusters[csize > 1];
    int clust_len = clusters.size();

    // Perform merging on all clusters with length greater than one.
    for(int i = 0; i < clust_len; ++i) {
      // Establish variables.
      String clust = clusters[i];
      String match_string(1);
      bool not_in_dict = true;

      // Look to see if clust exists in the dictionary. If so, use that value
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
  }

  return output;
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
