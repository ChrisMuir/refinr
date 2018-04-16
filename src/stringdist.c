/*
Example of using the C API from stringdist in new package code.
Code and API is modeled after that of package xts.
*/

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
/*
The following header file is from the include directory that is
included with stringdist
*/
#include "stringdist_api.h"


SEXP list_lens(SEXP x) {
  PROTECT(x);
  int n = length(x);
  SEXP out;
  out = PROTECT(allocVector(INTSXP, n));

  out = sd_lengths(x);

  UNPROTECT(2);
  return out;
}
