// Example of using the C API from stringdist in new package code.
// Code and API is modeled after that of package xts.

#include <Rcpp.h>
using namespace Rcpp;

// The following header file is from the include directory that is
// included with stringdist

#include "stringdist_api.h"


// [[Rcpp::export]]
IntegerVector cpp_list_lens(List x) {
  IntegerVector out = sd_lengths(x);
  return(out);
}
