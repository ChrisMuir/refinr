#include <Rcpp.h>
#include"refinr_header.h"
using namespace Rcpp;

// Utility helper functions, some are used within R functions, some are used
// within c++ functions, some are used in both.


// Flatten a nested list such that each character vector occupies its own
// element in the return list.
// [[Rcpp::export]]
List cpp_flatten_list(List list_obj) {
  int input_len = list_obj.size();

  // Get the size of the output list by getting the sum of the lengths of each
  // element of the input list.
  int out_len = 0;
  for(int i = 0; i < input_len; ++i) {
    List curr_list = list_obj[i];
    out_len += curr_list.size();
  }

  // Initialize output list.
  List out(out_len);

  int counter = 0;
  List::iterator list_obj_end = list_obj.end();
  List::iterator input_iter;
  for(input_iter = list_obj.begin(); input_iter != list_obj_end; ++input_iter) {
    List curr_list = *input_iter;
    List::iterator curr_list_iter;
    for(curr_list_iter = curr_list.begin(); curr_list_iter != curr_list.end(); ++curr_list_iter) {
      out[counter] = *curr_list_iter;
      counter += 1;
    }
  }
  return out;
}


// Input a list of character vectors, for each char vect apply func "collapse"
// and append to a char vector output object. Rcpp sugar func "collapse" is
// equivelant to R func paste(char_vect, collapse = "").
// [[Rcpp::export]]
CharacterVector cpp_paste_collapse_list(List input) {
  int input_len = input.size();
  CharacterVector output(input_len);

  for(int i = 0; i < input_len; ++i) {
    CharacterVector curr_vect = input[i];
    if(is_false(all(is_na(curr_vect)))) {
      output[i] = collapse(curr_vect);
    } else {
      output[i] = NA_STRING;
    }
  }

  return output;
}


// Input two char vectors (a and b), return TRUE if all of the elements of a
// are found in b, otherwise return FALSE.
// [[Rcpp::export]]
bool complete_intersect(CharacterVector a, CharacterVector b) {
  return is_true(all(cpp_in(a, b)));
}


// Input a char vector, subset to only include duplicated values, remove NA's,
// and then get unique values. Return the subset.
// [[Rcpp::export]]
CharacterVector cpp_get_key_dups(CharacterVector keys) {
  // Subset to only keep those that have a duplicate, remove, NA's, then
  // return unique values.
  keys = keys[duplicated(keys)];
  keys = keys[!is_na(keys)];
  return unique(keys);
}


// Meant to mimic the R func %in%.
// Returns "x %in% table".
// [[Rcpp::export]]
LogicalVector cpp_in(CharacterVector x, CharacterVector table) {
  // If length of table is 1, use function "equality".
  int table_len = table.size();
  if(table_len == 1) {
    return equality(x, table[0]);
  }

  // Else if length of table is less than 4, use a loop.
  if(table_len < 4) {
    LogicalVector out(x.size());
    CharacterVector::iterator table_end = table.end();
    CharacterVector::iterator iter;
    for(iter = table.begin(); iter != table_end; ++iter) {
      LogicalVector matches = equality(x, *iter);
      out = out | matches;
    }
    return out;
  }

  // Else if length of table is 4 or greater, use sugar function "match".
  LogicalVector out = match(x, table) > 0;
  out[is_na(out)] = FALSE;
  return out;
}


// Given a list of character vectors, return a list of the same length
// containing the unique values of each vector.
// [[Rcpp::export]]
List cpp_list_unique(List input, bool sort_vals) {
  int input_len = input.size();
  List out(input_len);

  if(sort_vals) {
    for(int i = 0; i < input_len; ++i) {
      CharacterVector curr_vect = input[i];
      out[i] = unique(curr_vect).sort();
    }
  } else {
    for(int i = 0; i < input_len; ++i) {
      CharacterVector curr_vect = input[i];
      out[i] = unique(curr_vect);
    }
  }
  return out;
}


// Given a list of character vectors, for each vector, remove any strings that
// appear in input vector "removes".
// [[Rcpp::export]]
List remove_strings(List input, CharacterVector removes) {
  int input_len = input.size();
  List out(input_len);

  for(int i = 0; i < input_len; ++i) {
    CharacterVector curr_vect = input[i];
    out[i] = curr_vect[!cpp_in(curr_vect, removes)];
  }
  return out;
}


// Compare all elements of a char vector to a char string.
//
// Takes a char vector and char string as input, output is a logical vector
// with length equal to the input char vector. This is equivalent to R code
// "vector == string".
// [[Rcpp::export]]
LogicalVector equality(CharacterVector lookupvect, String charstring) {
  int vect_length = lookupvect.size();
  LogicalVector out(vect_length);

  for(int i = 0; i < vect_length; ++i) {
    out[i] = charstring == lookupvect[i];
  }
  return out;
}


// cpp version of R function unique(), but only for char vectors.
// [[Rcpp::export]]
CharacterVector cpp_unique(CharacterVector vect) {
  return unique(vect);
}


// cpp version of R function trimws()
// [[Rcpp::export]]
CharacterVector cpp_trimws_left(CharacterVector vect) {
  return trimws(vect, "left");
}
