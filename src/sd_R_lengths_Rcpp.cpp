// Example of using the C API from stringdist in new package code.
// Code and API is modeled after that of package xts.

#include <Rcpp.h>
#include "refinr.h"
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector Rcpp_test_lengths(List x) {
  NumericVector out = stringdist_lengths(x);
  return(out);
}
