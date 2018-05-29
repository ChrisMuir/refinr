// Example of using the C API from stringdist in new package code.
// Code and API is modeled after that of package xts.

#include <Rcpp.h>
#include "refinr.h"
using namespace Rcpp;


// [[Rcpp::export]]
SEXP Rcpp_test_all_int(List x) {
  SEXP out = stringdist_all_int(x);
  return(out);
}
