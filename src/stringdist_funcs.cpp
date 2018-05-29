#include <Rcpp.h>
using namespace Rcpp;

#ifndef _STRINGDIST_API_H
# include "stringdist_api.h"
#endif


NumericVector stringdist_lengths(List X) {
  return(sd_lengths(X));
}

SEXP stringdist_all_int(List X) {
  return(sd_all_int(X));
}
