#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// Define std::unordered_map using std::string as keys and std::vector<int>
// as values.
typedef std::unordered_map<std::string, std::vector<int> > refinr_map;

// utils
refinr_map create_map(CharacterVector terms, std::vector<std::string> keys);
bool cpp_all(CharacterVector x, CharacterVector table);
List cpp_list_unique(List input, bool sort_vals);
CharacterVector cpp_paste_list(List input, std::string collapse_str);
CharacterVector cpp_get_key_dups(CharacterVector keys);
List cpp_flatten_list(List list_obj);
String most_freq_str(CharacterVector x);
List cpp_as_list(CharacterVector x);
CharacterVector cpp_unlist(List x);
List remove_strings(List input, CharacterVector removes);
CharacterVector cpp_tolower(CharacterVector x);

// get_fingerprint
CharacterVector cpp_get_fingerprint_KC(CharacterVector vect,
                                       bool bus_suffix,
                                       CharacterVector ignore_strings);

// key_collision_merge
CharacterVector merge_KC_clusters_no_dict(CharacterVector clusters,
                                          CharacterVector vect,
                                          CharacterVector keys_vect);

CharacterVector merge_KC_clusters_dict(CharacterVector clusters,
                                       CharacterVector vect,
                                       CharacterVector keys_vect,
                                       CharacterVector dict,
                                       CharacterVector keys_dict);

CharacterVector merge_KC_clusters(CharacterVector vect,
                                  CharacterVector keys_vect,
                                  CharacterVector dict,
                                  CharacterVector keys_dict);

// n_gram_merge
List get_ngram_initial_clusters(CharacterVector ngram_keys,
                                CharacterVector unigram_keys);

List get_stringdist_matrices(List clusters, SEXP method, SEXP weight, SEXP p,
                             SEXP bt, SEXP q, SEXP useBytes, SEXP nthread);

List filter_initial_clusters(List distmatrices, double edit_threshold,
                             List clusters);

SEXP stringdist_lower_tri(SEXP a, SEXP method, SEXP weight, SEXP p, SEXP bt,
                          SEXP q, SEXP useBytes, SEXP nthread);
