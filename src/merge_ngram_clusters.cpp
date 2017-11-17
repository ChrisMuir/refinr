#include <Rcpp.h>
#include"refinr_header.h"
using namespace Rcpp;


// Iterate over all clusters, make mass edits related to each cluster.
// [[Rcpp::export]]
CharacterVector merge_ngram_clusters(List clusters,
                                     CharacterVector n_gram_keys,
                                     CharacterVector univect,
                                     CharacterVector vect) {
  int clusters_len = clusters.size();
  int vect_len = vect.size();
  int n_gram_keys_len = n_gram_keys.size();
  CharacterVector output = clone(vect);

  for(int i = 0; i < clusters_len; ++i) {
    CharacterVector curr_clust = clusters[i];
    int curr_clust_len = curr_clust.size();

    // Get indices of elements of n_gram_keys that are found in cluster, as obj
    // "ng_idx".
    LogicalVector ng_idx(n_gram_keys_len);

    for(int n = 0; n < curr_clust_len; ++n) {
      LogicalVector clust_match_bool = equality(n_gram_keys, curr_clust[n]);
      ng_idx = ng_idx | clust_match_bool;
    }

    // If there is no intersection between n_gram_keys and curr_clust, stop
    // the current iteration and move on to the next element of clusters.
    if(is_false(any(ng_idx == TRUE))) {
      continue;
    }

    // Get indices of elements of vect that are found in vector univect[ng_idx],
    // as obj "vect_idx".
    CharacterVector univect_sub = univect[ng_idx];
    int univect_sub_len = univect_sub.size();
    LogicalVector vect_idx(vect_len);

    for(int n = 0; n < univect_sub_len; ++n) {
      LogicalVector vect_match_bool = equality(vect, univect_sub[n]);
      vect_idx = vect_idx | vect_match_bool;
    }

    // If there is no intersection between univect[nq_idx] and vect, stop the
    // current iteration and move on to the next element of clusters.
    if (is_false(any(vect_idx == TRUE))) {
      continue;
    }

    // Find the string that appears most frequently in vector vect[vect_idx], as
    // obj "most_freq_string". Edit all elements of vect[vect_idx] to be equal
    // to string "most_freq_string".
    CharacterVector vect_sub = vect[vect_idx];
    CharacterVector uni_vect_sub = sort_unique(vect_sub);
    CharacterVector freq = Rcpp::wrap(table(vect_sub));
    int freq_len = freq.size();
    IntegerVector freq_int(freq_len);
    for(int n = 0; n < freq_len; ++n) {
      freq_int[n] = atoi(freq[n]);
    }
    String most_freq_string = uni_vect_sub[which_max(freq_int)];

    // Make mass edits to vect.
    for(int n = 0; n < vect_len; ++n) {
      if(vect_idx[n]) {
        output[n] = most_freq_string;
      }
    }
  }
  return output;
}
