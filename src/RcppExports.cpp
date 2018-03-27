// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// merge_KC_clusters
CharacterVector merge_KC_clusters(CharacterVector vect, CharacterVector keys_vect, CharacterVector dict, CharacterVector keys_dict);
RcppExport SEXP _refinr_merge_KC_clusters(SEXP vectSEXP, SEXP keys_vectSEXP, SEXP dictSEXP, SEXP keys_dictSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type vect(vectSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type keys_vect(keys_vectSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type dict(dictSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type keys_dict(keys_dictSEXP);
    rcpp_result_gen = Rcpp::wrap(merge_KC_clusters(vect, keys_vect, dict, keys_dict));
    return rcpp_result_gen;
END_RCPP
}
// merge_KC_clusters_no_dict
CharacterVector merge_KC_clusters_no_dict(CharacterVector clusters, CharacterVector vect, CharacterVector keys_vect);
RcppExport SEXP _refinr_merge_KC_clusters_no_dict(SEXP clustersSEXP, SEXP vectSEXP, SEXP keys_vectSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type clusters(clustersSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type vect(vectSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type keys_vect(keys_vectSEXP);
    rcpp_result_gen = Rcpp::wrap(merge_KC_clusters_no_dict(clusters, vect, keys_vect));
    return rcpp_result_gen;
END_RCPP
}
// merge_KC_clusters_dict
CharacterVector merge_KC_clusters_dict(CharacterVector clusters, CharacterVector vect, CharacterVector keys_vect, CharacterVector dict, CharacterVector keys_dict);
RcppExport SEXP _refinr_merge_KC_clusters_dict(SEXP clustersSEXP, SEXP vectSEXP, SEXP keys_vectSEXP, SEXP dictSEXP, SEXP keys_dictSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type clusters(clustersSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type vect(vectSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type keys_vect(keys_vectSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type dict(dictSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type keys_dict(keys_dictSEXP);
    rcpp_result_gen = Rcpp::wrap(merge_KC_clusters_dict(clusters, vect, keys_vect, dict, keys_dict));
    return rcpp_result_gen;
END_RCPP
}
// merge_ngram_clusters
CharacterVector merge_ngram_clusters(List clusters, CharacterVector n_gram_keys, CharacterVector univect, CharacterVector vect);
RcppExport SEXP _refinr_merge_ngram_clusters(SEXP clustersSEXP, SEXP n_gram_keysSEXP, SEXP univectSEXP, SEXP vectSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type clusters(clustersSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type n_gram_keys(n_gram_keysSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type univect(univectSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type vect(vectSEXP);
    rcpp_result_gen = Rcpp::wrap(merge_ngram_clusters(clusters, n_gram_keys, univect, vect));
    return rcpp_result_gen;
END_RCPP
}
// ngram_merge_no_approx
CharacterVector ngram_merge_no_approx(CharacterVector n_gram_keys, CharacterVector univect, CharacterVector vect);
RcppExport SEXP _refinr_ngram_merge_no_approx(SEXP n_gram_keysSEXP, SEXP univectSEXP, SEXP vectSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type n_gram_keys(n_gram_keysSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type univect(univectSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type vect(vectSEXP);
    rcpp_result_gen = Rcpp::wrap(ngram_merge_no_approx(n_gram_keys, univect, vect));
    return rcpp_result_gen;
END_RCPP
}
// ngram_merge_approx
CharacterVector ngram_merge_approx(CharacterVector n_gram_keys, CharacterVector univect, CharacterVector vect, List distmatrices, double edit_threshold, List initial_clust);
RcppExport SEXP _refinr_ngram_merge_approx(SEXP n_gram_keysSEXP, SEXP univectSEXP, SEXP vectSEXP, SEXP distmatricesSEXP, SEXP edit_thresholdSEXP, SEXP initial_clustSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type n_gram_keys(n_gram_keysSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type univect(univectSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type vect(vectSEXP);
    Rcpp::traits::input_parameter< List >::type distmatrices(distmatricesSEXP);
    Rcpp::traits::input_parameter< double >::type edit_threshold(edit_thresholdSEXP);
    Rcpp::traits::input_parameter< List >::type initial_clust(initial_clustSEXP);
    rcpp_result_gen = Rcpp::wrap(ngram_merge_approx(n_gram_keys, univect, vect, distmatrices, edit_threshold, initial_clust));
    return rcpp_result_gen;
END_RCPP
}
// get_ngram_initial_clusters
List get_ngram_initial_clusters(CharacterVector ngram_keys, CharacterVector unigram_keys);
RcppExport SEXP _refinr_get_ngram_initial_clusters(SEXP ngram_keysSEXP, SEXP unigram_keysSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type ngram_keys(ngram_keysSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type unigram_keys(unigram_keysSEXP);
    rcpp_result_gen = Rcpp::wrap(get_ngram_initial_clusters(ngram_keys, unigram_keys));
    return rcpp_result_gen;
END_RCPP
}
// filter_initial_clusters
List filter_initial_clusters(List distmatrices, double edit_threshold, List clusters);
RcppExport SEXP _refinr_filter_initial_clusters(SEXP distmatricesSEXP, SEXP edit_thresholdSEXP, SEXP clustersSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type distmatrices(distmatricesSEXP);
    Rcpp::traits::input_parameter< double >::type edit_threshold(edit_thresholdSEXP);
    Rcpp::traits::input_parameter< List >::type clusters(clustersSEXP);
    rcpp_result_gen = Rcpp::wrap(filter_initial_clusters(distmatrices, edit_threshold, clusters));
    return rcpp_result_gen;
END_RCPP
}
// char_ngram
List char_ngram(List vects, int numgram);
RcppExport SEXP _refinr_char_ngram(SEXP vectsSEXP, SEXP numgramSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type vects(vectsSEXP);
    Rcpp::traits::input_parameter< int >::type numgram(numgramSEXP);
    rcpp_result_gen = Rcpp::wrap(char_ngram(vects, numgram));
    return rcpp_result_gen;
END_RCPP
}
// cpp_get_char_ngrams
CharacterVector cpp_get_char_ngrams(List vects, int numgram);
RcppExport SEXP _refinr_cpp_get_char_ngrams(SEXP vectsSEXP, SEXP numgramSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type vects(vectsSEXP);
    Rcpp::traits::input_parameter< int >::type numgram(numgramSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_get_char_ngrams(vects, numgram));
    return rcpp_result_gen;
END_RCPP
}
// most_freq_str
String most_freq_str(CharacterVector x);
RcppExport SEXP _refinr_most_freq_str(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(most_freq_str(x));
    return rcpp_result_gen;
END_RCPP
}
// cpp_flatten_list
List cpp_flatten_list(List list_obj);
RcppExport SEXP _refinr_cpp_flatten_list(SEXP list_objSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type list_obj(list_objSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_flatten_list(list_obj));
    return rcpp_result_gen;
END_RCPP
}
// cpp_paste_list
CharacterVector cpp_paste_list(List input, std::string collapse_str);
RcppExport SEXP _refinr_cpp_paste_list(SEXP inputSEXP, SEXP collapse_strSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type input(inputSEXP);
    Rcpp::traits::input_parameter< std::string >::type collapse_str(collapse_strSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_paste_list(input, collapse_str));
    return rcpp_result_gen;
END_RCPP
}
// complete_intersect
bool complete_intersect(CharacterVector a, CharacterVector b);
RcppExport SEXP _refinr_complete_intersect(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type a(aSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(complete_intersect(a, b));
    return rcpp_result_gen;
END_RCPP
}
// cpp_get_key_dups
CharacterVector cpp_get_key_dups(CharacterVector keys);
RcppExport SEXP _refinr_cpp_get_key_dups(SEXP keysSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type keys(keysSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_get_key_dups(keys));
    return rcpp_result_gen;
END_RCPP
}
// cpp_in
LogicalVector cpp_in(CharacterVector x, CharacterVector table);
RcppExport SEXP _refinr_cpp_in(SEXP xSEXP, SEXP tableSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type table(tableSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_in(x, table));
    return rcpp_result_gen;
END_RCPP
}
// cpp_list_unique
List cpp_list_unique(List input, bool sort_vals);
RcppExport SEXP _refinr_cpp_list_unique(SEXP inputSEXP, SEXP sort_valsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type input(inputSEXP);
    Rcpp::traits::input_parameter< bool >::type sort_vals(sort_valsSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_list_unique(input, sort_vals));
    return rcpp_result_gen;
END_RCPP
}
// remove_strings
List remove_strings(List input, CharacterVector removes);
RcppExport SEXP _refinr_remove_strings(SEXP inputSEXP, SEXP removesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type input(inputSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type removes(removesSEXP);
    rcpp_result_gen = Rcpp::wrap(remove_strings(input, removes));
    return rcpp_result_gen;
END_RCPP
}
// equality
LogicalVector equality(CharacterVector table, String x);
RcppExport SEXP _refinr_equality(SEXP tableSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type table(tableSEXP);
    Rcpp::traits::input_parameter< String >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(equality(table, x));
    return rcpp_result_gen;
END_RCPP
}
// cpp_unique
CharacterVector cpp_unique(CharacterVector vect);
RcppExport SEXP _refinr_cpp_unique(SEXP vectSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type vect(vectSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_unique(vect));
    return rcpp_result_gen;
END_RCPP
}
// cpp_trimws_left
CharacterVector cpp_trimws_left(CharacterVector vect);
RcppExport SEXP _refinr_cpp_trimws_left(SEXP vectSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type vect(vectSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_trimws_left(vect));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_refinr_merge_KC_clusters", (DL_FUNC) &_refinr_merge_KC_clusters, 4},
    {"_refinr_merge_KC_clusters_no_dict", (DL_FUNC) &_refinr_merge_KC_clusters_no_dict, 3},
    {"_refinr_merge_KC_clusters_dict", (DL_FUNC) &_refinr_merge_KC_clusters_dict, 5},
    {"_refinr_merge_ngram_clusters", (DL_FUNC) &_refinr_merge_ngram_clusters, 4},
    {"_refinr_ngram_merge_no_approx", (DL_FUNC) &_refinr_ngram_merge_no_approx, 3},
    {"_refinr_ngram_merge_approx", (DL_FUNC) &_refinr_ngram_merge_approx, 6},
    {"_refinr_get_ngram_initial_clusters", (DL_FUNC) &_refinr_get_ngram_initial_clusters, 2},
    {"_refinr_filter_initial_clusters", (DL_FUNC) &_refinr_filter_initial_clusters, 3},
    {"_refinr_char_ngram", (DL_FUNC) &_refinr_char_ngram, 2},
    {"_refinr_cpp_get_char_ngrams", (DL_FUNC) &_refinr_cpp_get_char_ngrams, 2},
    {"_refinr_most_freq_str", (DL_FUNC) &_refinr_most_freq_str, 1},
    {"_refinr_cpp_flatten_list", (DL_FUNC) &_refinr_cpp_flatten_list, 1},
    {"_refinr_cpp_paste_list", (DL_FUNC) &_refinr_cpp_paste_list, 2},
    {"_refinr_complete_intersect", (DL_FUNC) &_refinr_complete_intersect, 2},
    {"_refinr_cpp_get_key_dups", (DL_FUNC) &_refinr_cpp_get_key_dups, 1},
    {"_refinr_cpp_in", (DL_FUNC) &_refinr_cpp_in, 2},
    {"_refinr_cpp_list_unique", (DL_FUNC) &_refinr_cpp_list_unique, 2},
    {"_refinr_remove_strings", (DL_FUNC) &_refinr_remove_strings, 2},
    {"_refinr_equality", (DL_FUNC) &_refinr_equality, 2},
    {"_refinr_cpp_unique", (DL_FUNC) &_refinr_cpp_unique, 1},
    {"_refinr_cpp_trimws_left", (DL_FUNC) &_refinr_cpp_trimws_left, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_refinr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
