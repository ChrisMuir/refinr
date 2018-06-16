#include <Rcpp.h>
using namespace Rcpp;

#include <stringdist_api.h>

// stringdist C API functions
//
// The stringdist package makes its C functions available to other R packages
// via the header file "stringdist_api.h".

// Function that wraps the stringdist C function "sd_lower_tri()".
SEXP stringdist_lower_tri(const SEXP &a, SEXP method, SEXP weight, SEXP p, SEXP bt,
                          SEXP q, SEXP useBytes, SEXP nthread) {
  return(sd_lower_tri(a, method, weight, p, bt, q, useBytes, nthread));
}
