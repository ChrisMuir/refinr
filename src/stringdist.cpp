#include <Rcpp.h>
using namespace Rcpp;

#include <stringdist_api.h>


SEXP stringdist_lower_tri(SEXP a, SEXP method, SEXP weight, SEXP p, SEXP bt,
                          SEXP q, SEXP useBytes, SEXP nthread) {
  return(sd_lower_tri(a, method, weight, p, bt, q, useBytes, nthread));
}
