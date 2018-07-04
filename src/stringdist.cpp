#include <Rcpp.h>
using namespace Rcpp;

#include <stringdist_api.h>

// stringdist C API functions
//
// The stringdist package makes its C functions available to other R packages
// via the header file "stringdist_api.h".

// Function that wraps the stringdist C function "sd_lower_tri()".
SEXP stringdist_lower_tri(const SEXP &a,
                          const SEXP &method,
                          const SEXP &weight,
                          const SEXP &p,
                          const SEXP &bt,
                          const SEXP &q,
                          const SEXP &useBytes,
                          const SEXP &nthread) {
  return(sd_lower_tri(a, method, weight, p, bt, q, useBytes, nthread));
}
