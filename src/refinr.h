#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]


// Data structure used by function most_freq_str()
struct freq_string {
  String mf_str;
  IntegerVector x_tab;
  CharacterVector tab_names;
};

// Define std::unordered_map using SEXP as keys and std::vector<int>
// as values.
typedef std::unordered_map<SEXP, std::vector<int> > refinr_map;


// utils
refinr_map create_map(const CharacterVector &vect,
                      const CharacterVector &clusters);

refinr_map create_map_no_na(CharacterVector &terms,
                            const CharacterVector &keys);

bool cpp_all(const CharacterVector &x, const CharacterVector &table);
List cpp_list_unique(List &input, const bool &sort_vals);
CharacterVector cpp_paste_list(List &input, const std::string &collapse_str);
CharacterVector cpp_get_key_dups(CharacterVector keys);
List cpp_flatten_list(List &list_obj);
void most_freq_str(const CharacterVector &x, freq_string &mfs);
List cpp_as_list(const CharacterVector &x);
CharacterVector cpp_unlist(const List &x);


// key_collision_merge
CharacterVector merge_KC_clusters_no_dict(const CharacterVector &clusters,
                                          const CharacterVector &vect,
                                          CharacterVector &keys_vect);

CharacterVector merge_KC_clusters_dict(const CharacterVector &clusters,
                                       const CharacterVector &vect,
                                       CharacterVector &keys_vect,
                                       const CharacterVector &dict,
                                       const CharacterVector &keys_dict);


// n_gram_merge
List get_ngram_initial_clusters(CharacterVector ngram_keys,
                                CharacterVector unigram_keys);

List filter_initial_clusters(const List &distmatrices,
                             const double &edit_threshold,
                             const List &clusters);

SEXP stringdist_lower_tri(const SEXP &a,
                          const SEXP &method,
                          const SEXP &weight,
                          const SEXP &p,
                          const SEXP &bt,
                          const SEXP &q,
                          const SEXP &useBytes,
                          const SEXP &nthread);

List get_stringdist_matrices(const List &clusters,
                             const SEXP &method,
                             const SEXP &weight,
                             const SEXP &p,
                             const SEXP &bt,
                             const SEXP &q,
                             const SEXP &useBytes,
                             const SEXP &nthread);
