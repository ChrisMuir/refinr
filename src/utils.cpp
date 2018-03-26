#include <Rcpp.h>
#include"refinr_header.h"
using namespace Rcpp;

// Utility helper functions, some are used within R functions, some are used
// within c++ functions, some are used in both.


// Create unordered_map with strings as keys, and integer vectors as values.
unordered_map <std::string, std::vector<int> > create_map(
    CharacterVector vect,
    std::vector<std::string> clusters
) {
  int clust_len = clusters.size();
  int vect_len = vect.size();

  // Initialize unordered_map.
  unordered_map<std::string, std::vector<int> > clust_map;
  for(int i = 0; i < clust_len; ++i) {
    clust_map[clusters[i]];
  }

  // Fill values of the map.
  for(int i = 0; i < vect_len; ++i) {
    clust_map[as<std::string>(vect[i])].push_back(i);
  }

  return(clust_map);
}

// Given a CharacterVector, return the string that appears most frequently.
// Ties are determined by the string that appears first alphabetically.
// [[Rcpp::export]]
String most_freq_str(CharacterVector x) {
  IntegerVector x_tab = table(x);
  CharacterVector tab_names = x_tab.attr("names");
  return(tab_names[which_max(x_tab)]);
}


// Flatten a nested list such that each character vector occupies its own
// element in the return list.
// [[Rcpp::export]]
List cpp_flatten_list(List list_obj) {
  // Get the size of the output list by getting the sum of the lengths of each
  // element of the input list.
  List::iterator list_obj_begin = list_obj.begin();
  List::iterator list_obj_end = list_obj.end();
  List::iterator count_iter;
  int i = 0;
  int out_len = 0;
  for(count_iter = list_obj_begin; count_iter != list_obj_end; ++count_iter) {
    List curr_list = *count_iter;
    out_len += curr_list.size();
    i++;
  }

  // Initialize output list.
  List out(out_len);

  List::iterator iter;
  int n = 0;
  for(iter = list_obj.begin(); iter != list_obj_end; ++iter) {
    List curr_list = *iter;
    List::iterator curr_list_iter;
    for(curr_list_iter = curr_list.begin(); curr_list_iter != curr_list.end(); ++curr_list_iter) {
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
  List out(input.size());
  List::iterator input_end = input.end();
  List::iterator iter;
  int i = 0;

  for(iter = input.begin(); iter != input_end; ++iter) {
    CharacterVector curr_vect = *iter;
    out[i] = curr_vect[!cpp_in(curr_vect, removes)];
    i++;
  }

  return out;
}


// Compare all elements of a char vector to a char string.
//
// Takes a char vector and char string as input, output is a logical vector
// with length equal to the input char vector. This is equivalent to R code
// "vector == string".
// [[Rcpp::export]]
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
