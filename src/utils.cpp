#include <Rcpp.h>
#include"refinr.h"
using namespace Rcpp;


// Utility helper functions, some are used within R functions, some are used
// within c++ functions, some are used in both.


// Create unordered_map with strings as keys, and integer vectors as values.
refinr_map create_map(const CharacterVector &terms,
                      const CharacterVector &keys) {
  int keys_len = keys.size();
  int terms_len = terms.size();

  // refinr_map uses pointers to CHARSXP SEXP as keys. Object "ptr" is used
  // to house pointers to the underlying SEXP of CharacterVectors.
  SEXP* ptr;

  // Initialize unordered_map.
  refinr_map out;
  ptr = get_string_ptr(keys);
  for(int i = 0; i < keys_len; ++i) {
    out[ptr[i]];
  }

  // Fill values of the map.
  ptr = get_string_ptr(terms);
  refinr_map::iterator val;

  for(int i = 0; i < terms_len; ++i) {
    val = out.find(ptr[i]);
    if(val != out.end()) {
      val->second.push_back(i);
    }
  }

  return(out);
}


// Create unordered_map with strings as keys, and integer vectors as values,
// and skip over string terms that are NA.
refinr_map create_map_no_na(CharacterVector &terms,
                            const CharacterVector &keys) {
  int terms_len = terms.size();
  int keys_len = keys.size();

  // refinr_map uses pointers to CHARSXP SEXP as keys. Object "ptr" is used
  // to house pointers to the underlying SEXP of CharacterVectors.
  SEXP* ptr;

  // Initialize unordered_map.
  refinr_map out;
  ptr = get_string_ptr(keys);
  for(int i = 0; i < keys_len; ++i) {
    out[keys[i]];
  }

  // Fill values of the map.
  ptr = get_string_ptr(terms);
  refinr_map::iterator val;

  for(int i = 0; i < terms_len; ++i) {
    if(terms[i] == NA_STRING) {
      continue;
    }
    val = out.find(ptr[i]);
    if(val != out.end()) {
      val->second.push_back(i);
    }
  }

  return(out);
}


// Rcpp version of base::tolower()
// NOTE: converts all NA values to string "NA", should only be used on vectors
// that are known to not contain NA values.
// [[Rcpp::export]]
CharacterVector cpp_tolower(const CharacterVector &x) {
  // Normalize case
  CharacterVector out(x.size());
  std::transform(x.begin(), x.end(), out.begin(),
                 make_string_transformer(tolower));
  return(out);
}


// Given a CharacterVector, return the string that appears most frequently.
// Ties are determined by the string that appears first alphabetically.
void most_freq_str(const CharacterVector &x, freq_string &mfs) {
  mfs.x_tab = table(noNA(x));
  mfs.tab_names = mfs.x_tab.attr("names");
  mfs.mf_str = mfs.tab_names[which_max(noNA(mfs.x_tab))];
}


// cpp version of base::as.list(), for character vectors.
List cpp_as_list(const CharacterVector &x) {
  int x_len = x.size();
  List out(x_len);

  for(int i = 0; i < x_len; ++i) {
    out[i] = x[i];
  }

  return(out);
}


// cpp version of base::unlist(), for character vectors.
CharacterVector cpp_unlist(const List &x) {
  int x_len = x.size();

  // Get size of output vector.
  int out_len = 0;
  for(int i = 0; i < x_len; ++i) {
    out_len += Rf_length(x[i]);
  }

  // Allocate the vector
  CharacterVector out(out_len);

  // Loop and fill
  CharacterVector curr_vect;
  int idx = 0;
  for(int i = 0; i < x_len; ++i) {
    curr_vect = x[i];
    std::copy(curr_vect.begin(), curr_vect.end(), out.begin() + idx);

    // Update the index
    idx += curr_vect.size();
  }

  return out;
}


// Flatten a nested list such that each character vector occupies its own
// element in the return list.
List cpp_flatten_list(List &list_obj) {
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

  List curr_list;
  List::iterator iter;
  int n = 0;
  for(iter = list_obj.begin(); iter != list_obj_end; ++iter) {
    curr_list = *iter;
    for(int i = 0; i < curr_list.size(); ++i) {
      out[n] = curr_list[i];
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
CharacterVector cpp_paste_list(List &input,
                               const std::string &collapse_str) {
  CharacterVector output(input.size());

  // Initialize variables used in the loop below.
  CharacterVector curr_vect;
  int curr_vect_len;
  std::string curr_out;

  List::iterator input_end = input.end();
  List::iterator iter;
  int i = 0;
  for(iter = input.begin(); iter != input_end; ++iter) {
    curr_vect = *iter;
    if(is_false(all(is_na(curr_vect)))) {
      curr_vect_len = curr_vect.size();
      if(curr_vect_len == 1) {
        output[i] = as<std::string>(curr_vect);
        i++;
        continue;
      }
      curr_out = as<std::string>(curr_vect[0]);
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
  return unique(noNA(keys));
}


// Does string x appear in char vect table.
bool cpp_in(const String &x, const CharacterVector &table) {
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
bool cpp_all(const CharacterVector &x, const CharacterVector &table) {
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
List cpp_list_unique(List &input, const bool &sort_vals) {

  CharacterVector curr_vect;

  List out(input.size());
  List::iterator input_end = input.end();
  List::iterator iter;
  int i = 0;

  if(sort_vals) {
    for(iter = input.begin(); iter != input_end; ++iter) {
      curr_vect = *iter;
      out[i] = unique(curr_vect).sort();
      i++;
    }
  } else {
    for(iter = input.begin(); iter != input_end; ++iter) {
      curr_vect = *iter;
      out[i] = unique(curr_vect);
      i++;
    }
  }

  return out;
}


// Given a list of character vectors, for each vector, remove any strings that
// appear in input vector "removes".
// [[Rcpp::export]]
List remove_strings(List &input, std::vector<std::string> &removes) {
  // Add "NA" to removes.
  removes.push_back("NA");

  // Create unordered_set of the "removes" strings.
  std::unordered_set<std::string> removes_set(removes.begin(), removes.end());

  int input_len = input.size();
  List out(input_len);
  std::vector<std::string> curr_vect;
  std::deque<std::string> curr_vect_out;

  for(int i = 0; i < input_len; ++i) {
    curr_vect = as<std::vector<std::string> >(input[i]);
    curr_vect_out.clear();
    for(unsigned int n = 0; n < curr_vect.size(); ++n) {
      if(removes_set.find(curr_vect[n]) == removes_set.end()) {
        curr_vect_out.push_back(curr_vect[n]);
      }
    }

    out[i] = curr_vect_out;
  }

  return out;
}


// cpp version of R function unique(), but only for char vectors.
// [[Rcpp::export]]
CharacterVector cpp_unique(const CharacterVector &vect) {
  return unique(noNA(vect));
}


// cpp version of R function trimws()
// [[Rcpp::export]]
CharacterVector cpp_trimws_left(const CharacterVector &vect) {
  return trimws(vect, "left");
}
