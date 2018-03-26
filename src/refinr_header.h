#include <Rcpp.h>
using namespace Rcpp;

#include <tr1/unordered_map>
using std::tr1::unordered_map;

LogicalVector equality(CharacterVector lookupvect, String charstring);
LogicalVector cpp_in(CharacterVector x, CharacterVector y);
bool complete_intersect(CharacterVector a, CharacterVector b);
List cpp_list_unique(List input, bool sort_vals);
CharacterVector cpp_paste_list(List input, std::string collapse_str);
CharacterVector cpp_get_key_dups(CharacterVector keys);
List cpp_flatten_list(List list_obj);
String most_freq_str(CharacterVector x);
unordered_map <std::string, std::vector<int> > create_map(CharacterVector vect, std::vector<std::string> clusters);
CharacterVector merge_KC_clusters_no_dict(CharacterVector clusters, CharacterVector vect, CharacterVector keys_vect);
CharacterVector merge_KC_clusters_dict(CharacterVector clusters, CharacterVector vect, CharacterVector keys_vect, CharacterVector dict, CharacterVector keys_dict);
