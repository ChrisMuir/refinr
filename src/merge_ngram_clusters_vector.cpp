#include <Rcpp.h>
#include"refinr_header.h"
using namespace Rcpp;

//' Merge values related to a cluster of length > 1
//'
//' Function that performs all merges related to a single cluster, given that
//' the input cluster is a char vector with length greater than one. Values
//' within arg vect that are related to the cluster are edited/merged, returnm
//' is the edited char vector "vect".
//' @param cluster character vector
//' @param n_gram_keys character vector
//' @param univect character vector
//' @param vect character vector
// [[Rcpp::export]]
CharacterVector merge_ngram_clusters_vector(CharacterVector cluster,
                                            CharacterVector n_gram_keys,
                                            CharacterVector univect,
                                            CharacterVector vect) {
  int vect_len = vect.size();
  CharacterVector output = clone(vect);

  // Get indices of elements of n_gram_keys that are found in cluster, as obj
  // "ng_idx".
  int clust_len = cluster.size();
  int n_gram_keys_len = n_gram_keys.size();
  LogicalVector ng_idx(n_gram_keys_len);
  for(int i = 0; i < clust_len; ++i) {
    LogicalVector clust_match_bool(n_gram_keys_len);
    clust_match_bool = equality(n_gram_keys, cluster[i]);
    ng_idx = ng_idx | clust_match_bool;
  }
  // If there is no intersection between n_gram_keys and cluster, return vect
  // unedited.
  if (sum(ng_idx) == 0 ) {
    return vect;
  }

  // Get indices of elements of vect that are found in vector univect[ng_idx],
  // as obj "vect_idx".
  CharacterVector univect_sub(sum(ng_idx));
  univect_sub = univect[ng_idx];
  int univect_sub_len = univect_sub.size();
  LogicalVector vect_idx(vect_len);
  for(int i = 0; i < univect_sub_len; ++i) {
    LogicalVector vect_match_bool(vect_len);
    vect_match_bool = equality(vect, univect_sub[i]);
    vect_idx = vect_idx | vect_match_bool;
  }
  // If there is no intersection between univect[nq_idx] and vect, return vect
  // unedited.
  if (sum(vect_idx) == 0 ) {
    return vect;
  }

  // Find the string that appears most frequently in vector vect[vect_idx], as
  // obj "most_freq_string". Edit all elements of vect[vect_idx] to be equal
  // to string "most_freq_string".
  CharacterVector vect_sub(sum(vect_idx));
  vect_sub = vect[vect_idx];
  String most_freq_string(1);

  CharacterVector uni_vect_sub;
  CharacterVector freq;
  uni_vect_sub = Rcpp::wrap( sort_unique( vect_sub ));
  freq = Rcpp::wrap( table( vect_sub ));
  int freq_len = freq.size();
  IntegerVector freq_int(freq_len);
  for(int i = 0; i < freq_len; ++i) {
    freq_int[i] = atoi(freq[i]);
  }
  most_freq_string = uni_vect_sub[which_max(freq_int)];

  for(int n = 0; n < vect_len; ++n) {
    if ( vect_idx[n] ) {
      output[n] = most_freq_string;
    }
  }
  return output;
}
