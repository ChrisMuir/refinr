#include <Rcpp.h>
using namespace Rcpp;
LogicalVector equality(CharacterVector lookupvect, String charstring);
String most_freq(String clust, CharacterVector keyssub, CharacterVector vectsub);
LogicalVector cpp_in(CharacterVector x, CharacterVector y);
bool complete_intersect(CharacterVector a, CharacterVector b);
List cpp_list_unique(List input, bool sort_vals);
CharacterVector cpp_paste_collapse_list(List input);
CharacterVector cpp_get_key_dups(CharacterVector keys);
List cpp_flatten_list(List list_obj);
CharacterVector merge_KC_clusters_no_dict(CharacterVector clusters, CharacterVector vect, CharacterVector keys_vect);
CharacterVector merge_KC_clusters_dict(CharacterVector clusters, CharacterVector vect, CharacterVector keys_vect, CharacterVector dict, CharacterVector keys_dict);
