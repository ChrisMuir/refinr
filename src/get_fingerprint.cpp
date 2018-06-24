#include <Rcpp.h>
#include"refinr.h"
using namespace Rcpp;

#include <stringi.h>

// Set up all required stringi GetCCallables
#include <stringi.cpp>


// Call base::iconv() from Rcpp.
CharacterVector iconv_trans(const CharacterVector &str) {
  Environment base("package:base");
  Function iconv = base["iconv"];

  CharacterVector res = iconv(str, Named("to", "ASCII//TRANSLIT"));
  return(res);
}


// stringi::stri_trans_general()
CharacterVector stringi_trans_general(const CharacterVector &str) {
  return(stri_trans_general(str, CharacterVector("latin-ASCII")));
}


// Combine base::iconv() and stringi::stri_trans_general() into one function
// that properly removes accents from ACSII chars and UTF-8 chars.
CharacterVector cpp_remove_accents(const CharacterVector &str) {
  int str_len = str.size();
  CharacterVector out(str_len);

  // Determine which strings in str are UTF-8.
  CharacterVector enc = stri_enc_mark(str);
  LogicalVector is_utf8(str_len);
  for(int i = 0; i < str_len; ++i) {
    is_utf8[i] = enc[i] == "UTF-8";
  }

  out[!is_utf8] = iconv_trans(str[!is_utf8]);
  out[is_utf8] = stringi_trans_general(str[is_utf8]);

  return(out);
}


// Function that attempts to merge common business name suffixes within a
// character string.
CharacterVector cpp_business_suffix(CharacterVector vect) {

  List opts = NULL;

  // Create vector patterns for the regex replacement.
  CharacterVector patterns_regex = CharacterVector::create(
    " incorporated| incorporate",
    " corporation| corporations",
    " company| companys| companies",
    " limited$",
    " division| divisions",
    " enterprises| enterprise"
  );

  // Create vector of replacement strings for the regex replacement.
  CharacterVector repl_regex = CharacterVector::create(
    " inc",
    " corp",
    " co",
    " ltd",
    " div",
    " ent"
  );

  // Perform regex replacement.
  vect = stri_replace_all_regex(
    vect, patterns_regex, repl_regex, LogicalVector(false), opts
  );

  // Create vector of patterns for the fixed replacement.
  CharacterVector patterns_fixed = CharacterVector::create(
    " limited liability co",
    " limited partnership"
  );

  // Create vector of replacement strings for the fixed replacement.
  CharacterVector repl_fixed = CharacterVector::create(
    " llc",
    " lp"
  );

  // Perform fixed replacemenet.
  vect = stri_replace_all_fixed(
    vect, patterns_fixed, repl_fixed, LogicalVector(false), opts
  );

  return(vect);
}


// stringi::stri_split_regex()
List stringi_split_regex(const CharacterVector &str, CharacterVector pattern) {
  IntegerVector n = IntegerVector::create(-1);
  LogicalVector omit_empty = LogicalVector(false);
  LogicalVector tokens_only = LogicalVector(false);
  LogicalVector simplify = LogicalVector(false);
  List opts_regex = NULL;
  return(stri_split_regex(str, pattern, n, omit_empty, tokens_only, simplify,
                          opts_regex));
}


// Takes a char vector as input, converts any empty string values to NA_STRING,
// returns as a char vector.
CharacterVector empty_str_to_na(CharacterVector x) {
  int x_len = x.size();

  for(int i = 0; i < x_len; ++i) {
    if(x[i] == "") {
      x[i] = NA_STRING;
    }
  }

  return(x);
}


// Establish vector of strings to ignore.
CharacterVector get_ignore_strings(CharacterVector x, bool bus_suffix) {
  bool x_is_na = CharacterVector::is_na(x[0]);

  // If bus_suffix is FALSE, return NA_STRING.
  if(!bus_suffix) {
    return(x);
  }

  // If x is NA and bus_suffix is TRUE, create a vector of common
  // business suffix strings to ignore, and return them.
  if(x_is_na and bus_suffix) {
    CharacterVector out =
      CharacterVector::create("inc", "corp", "co", "llc", "ltd", "div",
                              "ent", "lp", "and");
    return(out);
  }

  // If x is not NA and bus_suffix is TRUE, combine the input vector x with a
  // vector of common business suffix strings.
  x = cpp_remove_accents(x);
  CharacterVector ignores =
    CharacterVector::create("inc", "corp", "co", "llc", "ltd", "div",
                            "ent", "lp", "and");
  CharacterVector out(x.size() + ignores.size());
  int i = 0;
  for( ; i < x.size(); i++) out[i] = x[i];
  for(int j = 0; j < ignores.size(); i++, j++) out[i] = ignores[j];

  return(out);
}


// Get vector of key collision keys.
CharacterVector cpp_get_fingerprint_KC(const CharacterVector &vect,
                                       bool bus_suffix,
                                       CharacterVector ignore_strings) {

  // If vect is NA, return NA.
  if(is_true(all(is_na(vect)))) {
    return(CharacterVector(NA_STRING));
  }

  CharacterVector out = clone(vect);

  // Normalize case
  out = cpp_tolower(out);

  // Replace some punctuation with an empty string (want "Ed's" to be 1 word).
  // Replace other punct with a blank space (want "cats,inc" to be 2 words).
  out = stri_replace_all_regex(
    out,
    CharacterVector::create("[;'`\"$]", "[[:punct:]\\+]"),
    CharacterVector::create("", " "),
    LogicalVector(false),
    List(NULL)
  );

  CharacterVector ignores = get_ignore_strings(ignore_strings, bus_suffix);
  if(bus_suffix) {
    // If bus_suffix is TRUE, normalize all common business suffix strings in
    // the output char vector.
    out = cpp_business_suffix(out);
  }

  // Normalize all accent marks in chars.
  out = cpp_remove_accents(out);

  // Trim whitespace.
  out = trimws(out, "both");

  // Split all string elements by space(s).
  List l_out = stringi_split_regex(out, "\\s+");

  // If ignores is not NA, for each element of l_out, remove
  // any string that has a match within vector "ignores".
  if(!CharacterVector::is_na(ignores)) {
    l_out = remove_strings(l_out, ignores);
  }

  // Filter each char vector in l_out to only include the unique values in
  // each vector.
  l_out = cpp_list_unique(l_out, TRUE);

  // For each char vector in l_out, collapse the strings into a single string,
  // return as a CharacterVector.
  out = cpp_paste_list(l_out, " ");

  // Convert empty strings to NA.
  out = empty_str_to_na(out);

  return(out);
}


// cpp version of base::paste() with a 'collapse' argument.
String cpp_paste(const CharacterVector &x, std::string collapse_str) {
  int x_len = x.size();

  if(x_len == 1) {
    return(x[0]);
  }

  std::vector<std::string> x_ = as<std::vector<std::string> >(x);
  std::string out = x_[0];
  for(int i = 1; i < x_len; ++i) {
    out += collapse_str;
    out += x_[i];
  }

  return(wrap(out));
}


// Unigram character tokenizer. Vector input, list output.
List cpp_str_split_char(const CharacterVector &x) {
  int x_len = x.size();
  List out(x_len);
  std::vector<std::string> x_ = as<std::vector<std::string> >(x);

  std::string curr_x;
  int curr_x_len;
  std::vector<char> curr_out;

  for(int i = 0; i < x_len; ++i) {
    curr_x = x_[i];
    curr_x_len = curr_x.size();
    curr_out.clear();
    for(int chr = 0; chr < curr_x_len; ++chr) {
      curr_out.push_back(curr_x[chr]);
    }

    out[i] = curr_out;
  }

  return(out);
}


// Get vector of ngram keys.
CharacterVector cpp_get_fingerprint_ngram(const CharacterVector &vect,
                                          int num_gram,
                                          bool bus_suffix,
                                          CharacterVector ignore_strings,
                                          double edit_threshold) {
  // If edit_threshold is NA, return NA.
  if(R_IsNA(edit_threshold)) {
    return(CharacterVector(NA_STRING));
  }

  CharacterVector out = clone(vect);

  // Normalize case
  out = cpp_tolower(out);

  // Replace some punctuation with an empty string (want "Ed's" to be 1 word).
  // Replace other punct with a blank space (want "cats,inc" to be 2 words).
  out = stri_replace_all_regex(
    out,
    CharacterVector::create("[;'`\"$]", "[[:punct:]\\+]"),
    CharacterVector::create("", " "),
    LogicalVector(false),
    List(NULL)
  );

  CharacterVector ignores = get_ignore_strings(ignore_strings, bus_suffix);
  if(bus_suffix) {
    // If bus_suffix is TRUE, normalize all common business suffix strings in
    // the output char vector.
    out = cpp_business_suffix(out);
  }

  // Remove all "ignore_strings" values and all spaces.
  if(!CharacterVector::is_na(ignores)) {
    // Use values in "ignore_strings" to create a regex of substrings to
    // eliminate from each element of "vect" (also remove all spaces).
    CharacterVector regex = CharacterVector::create(
      "\\b(", cpp_paste(ignores, "|"), ")\\b|\\s"
    );
    out = stri_replace_all_regex(
      out, CharacterVector::create(cpp_paste(regex, "")), CharacterVector(""),
      LogicalVector(false), List(NULL)
    );
  } else {
    out = stri_replace_all_regex(
      out, CharacterVector("\\s+"), CharacterVector(""),
      LogicalVector(false), List(NULL)
    );
  }

  // Normalize all accent marks in chars.
  out = cpp_remove_accents(out);

  if(num_gram == 1) {
    List out_l = cpp_str_split_char(out);
    out_l = cpp_list_unique(out_l, TRUE);
    out = cpp_paste_list(out_l, "");
    return(out);
  }

  out = cpp_get_char_ngrams(as<std::vector<std::string> >(out), num_gram);
  return(out);
}
