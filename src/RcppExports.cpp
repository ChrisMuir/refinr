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
// cpp_get_char_ngrams
CharacterVector cpp_get_char_ngrams(std::vector<std::string> vects, int numgram);
RcppExport SEXP _refinr_cpp_get_char_ngrams(SEXP vectsSEXP, SEXP numgramSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type vects(vectsSEXP);
    Rcpp::traits::input_parameter< int >::type numgram(numgramSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_get_char_ngrams(vects, numgram));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp_test_all_int
SEXP Rcpp_test_all_int(List x);
RcppExport SEXP _refinr_Rcpp_test_all_int(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp_test_all_int(x));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp_test_lengths
NumericVector Rcpp_test_lengths(List x);
RcppExport SEXP _refinr_Rcpp_test_lengths(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp_test_lengths(x));
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

RcppExport SEXP sd_lower_tri(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_refinr_merge_KC_clusters", (DL_FUNC) &_refinr_merge_KC_clusters, 4},
    {"_refinr_ngram_merge_no_approx", (DL_FUNC) &_refinr_ngram_merge_no_approx, 3},
    {"_refinr_ngram_merge_approx", (DL_FUNC) &_refinr_ngram_merge_approx, 6},
    {"_refinr_get_ngram_initial_clusters", (DL_FUNC) &_refinr_get_ngram_initial_clusters, 2},
    {"_refinr_cpp_get_char_ngrams", (DL_FUNC) &_refinr_cpp_get_char_ngrams, 2},
    {"_refinr_Rcpp_test_all_int", (DL_FUNC) &_refinr_Rcpp_test_all_int, 1},
    {"_refinr_Rcpp_test_lengths", (DL_FUNC) &_refinr_Rcpp_test_lengths, 1},
    {"_refinr_cpp_paste_list", (DL_FUNC) &_refinr_cpp_paste_list, 2},
    {"_refinr_cpp_list_unique", (DL_FUNC) &_refinr_cpp_list_unique, 2},
    {"_refinr_remove_strings", (DL_FUNC) &_refinr_remove_strings, 2},
    {"_refinr_cpp_unique", (DL_FUNC) &_refinr_cpp_unique, 1},
    {"_refinr_cpp_trimws_left", (DL_FUNC) &_refinr_cpp_trimws_left, 1},
    {"sd_lower_tri",                       (DL_FUNC) &sd_lower_tri,                       8},
    {NULL, NULL, 0}
};

RcppExport void R_init_refinr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
