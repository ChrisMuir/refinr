#include <Rcpp.h>
#include"refinr.h"
using namespace Rcpp;


// Utility helper functions, some are used within R functions, some are used
// within c++ functions, some are used in both.


// Create unordered_map with strings as keys, and integer vectors as values.
refinr_map create_map(CharacterVector terms,
                      std::vector<std::string> keys) {
  int keys_len = keys.size();
  int terms_len = terms.size();

  // Initialize unordered_map.
  refinr_map out;
  for(int i = 0; i < keys_len; ++i) {
    out[keys[i]];
  }

  // Fill values of the map.
  for(int i = 0; i < terms_len; ++i) {
    refinr_map::iterator val = out.find(as<std::string>(terms[i]));
    if(val != out.end()) {
      val->second.push_back(i);
    }
  }

  return(out);
}

// Given a CharacterVector, return the string that appears most frequently.
// Ties are determined by the string that appears first alphabetically.
String most_freq_str(CharacterVector x) {
  IntegerVector x_tab = table(x);
  CharacterVector tab_names = x_tab.attr("names");
  return(tab_names[which_max(x_tab)]);
}


// cpp version of base::as.list(), for character vectors.
List cpp_as_list(CharacterVector x) {
  int x_len = x.size();
  List out(x_len);

  for(int i = 0; i < x_len; ++i) {
    out[i] = x[i];
  }

  return(out);
}


// cpp version of base::unlist(), for character vectors.
CharacterVector cpp_unlist(List x) {
  int x_len = x.size();

  // Get size of output vector.
  int out_len = 0;
  for(int i = 0; i < x_len; ++i) {
    out_len += Rf_length(x[i]);
  }

  // Allocate the vector
  CharacterVector out(out_len);

  // Loop and fill
  int idx = 0;
  for(int i = 0; i < x_len; ++i) {
    CharacterVector curr_vect = x[i];
    std::copy(curr_vect.begin(), curr_vect.end(), out.begin() + idx);

    // Update the index
    idx += curr_vect.size();
  }

  return out;
}


// Flatten a nested list such that each character vector occupies its own
// element in the return list.
List cpp_flatten_list(List list_obj) {
  // Get the size of the output list by getting the sum of the lengths of each
  // element of the input list.
  List::iterator list_obj_begin = list_obj.begin();
  List::iterator list_obj_end = list_obj.end();
  List::iterator count_iter;
  int out_len = 0;
  for(count_iter = list_obj_begin; count_iter != list_obj_end; ++count_iter) {
    out_len += Rf_length(*count_iter);
  }

  // Initialize output list.
  List out(out_len);

  List::iterator iter;
  int n = 0;
  for(iter = list_obj.begin(); iter != list_obj_end; ++iter) {
    List curr_list = *iter;
    List::iterator curr_list_iter;
    for(curr_list_iter = curr_list.begin();
        curr_list_iter != curr_list.end();
        ++curr_list_iter) {
      out[n] = *curr_list_iter;
      n++;
    }
  }

  return out;
}


// Input a list of character vectors, for each char vect concat the strings,
// separated by arg collapse, and append to a char vector output object. The
// functions being applied to each element of the list are equivelant to
// R func paste(char_vect, collapse = collapse).
// [[Rcpp::export]]
CharacterVector cpp_paste_list(List input, std::string collapse_str) {
  CharacterVector output(input.size());
  List::iterator input_end = input.end();
  List::iterator iter;
  int i = 0;
  for(iter = input.begin(); iter != input_end; ++iter) {
    CharacterVector curr_vect = *iter;
    if(is_false(all(is_na(curr_vect)))) {
      int curr_vect_len = curr_vect.size();
      if(curr_vect_len == 1) {
        output[i] = as<std::string>(curr_vect);
        i++;
        continue;
      }
      std::string curr_out = as<std::string>(curr_vect[0]);
      for(int n = 1; n < curr_vect_len; ++n) {
        curr_out += collapse_str;
        curr_out += curr_vect[n];
      }
      output[i] = curr_out;
    } else {
      output[i] = NA_STRING;
    }
    i++;
  }

  return(output);
}


// Input a char vector, subset to only include duplicated values, remove NA's,
// and then get unique values. Return the subset.
CharacterVector cpp_get_key_dups(CharacterVector keys) {
  // Subset to only keep those that have a duplicate, remove, NA's, then
  // return unique values.
  keys = keys[duplicated(keys)];
  keys = keys[!is_na(keys)];
  return unique(keys);
}


// Does string x appear in char vect table.
bool cpp_in(String x, CharacterVector table) {
  int table_len = table.size();
  bool out = false;

  for(int i = 0; i < table_len; ++i) {
    if(table[i] == x) {
      out = true;
      break;
    }
  }

  return(out);
}


// Are all strings of char vect x found in table.
bool cpp_all(CharacterVector x, CharacterVector table) {
  int table_len = table.size();
  int x_len = x.size();

  if(x_len > table_len) {
    return(false);
  }

  bool out = true;
  for(int i = 0; i < x_len; ++i) {
    if(!cpp_in(x[i], table)) {
      out = false;
      break;
    }
  }

  return(out);
}


// Given a list of character vectors, return a list of the same length
// containing the unique values of each vector.
// [[Rcpp::export]]
List cpp_list_unique(List input, bool sort_vals) {
  List out(input.size());
  List::iterator input_end = input.end();
  List::iterator iter;
  int i = 0;

  if(sort_vals) {
    for(iter = input.begin(); iter != input_end; ++iter) {
      CharacterVector curr_vect = *iter;
      out[i] = unique(curr_vect).sort();
      i++;
    }
  } else {
    for(iter = input.begin(); iter != input_end; ++iter) {
      CharacterVector curr_vect = *iter;
      out[i] = unique(curr_vect);
      i++;
    }
  }

  return out;
}


// Given a list of character vectors, for each vector, remove any strings that
// appear in input vector "removes".
// [[Rcpp::export]]
List remove_strings(List input, CharacterVector removes) {
  // Create unordered_set of the "removes" strings.
  int removes_len = removes.size();
  std::unordered_set<std::string> removes_set;
  for(int n = 0; n < removes_len; ++n) {
    removes_set.insert(as<std::string>(removes[n]));
  }

  List out(input.size());
  int input_len = input.size();

  for(int i = 0; i < input_len; ++i) {
    CharacterVector curr_vect = input[i];
    int curr_vect_len = curr_vect.size();
    std::deque<std::string> curr_vect_out;
    for(int n = 0; n < curr_vect_len; ++n) {
      std::string curr_str = as<std::string>(curr_vect[n]);
      if(removes_set.find(curr_str) == removes_set.end()) {
        curr_vect_out.push_back(curr_str);
      }
    }

    out[i] = curr_vect_out;
  }

  return out;
}


// Compare all elements of a char vector to a char string.
//
// Takes a char vector and char string as input, output is a logical vector
// with length equal to the input char vector. This is equivalent to R code
// "vector == string".
LogicalVector equality(CharacterVector table, String x) {
  LogicalVector out(table.size());
  CharacterVector::iterator table_end = table.end();
  CharacterVector::iterator iter;
  int i = 0;

  for(iter = table.begin(); iter != table_end; ++iter) {
    out[i] = *iter == x;
    i++;
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
