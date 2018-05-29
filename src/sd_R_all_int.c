/*
 Example of using the C API from stringdist in new package code.
 Code and API is modeled after that of package xts.
 */

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include "stringdist_funcs.h"

SEXP C_test_all_int(SEXP x) {
  PROTECT(x);
  int n = length(x);
  SEXP out;
  out = PROTECT(allocVector(LGLSXP, n));

  out = stringdist_all_int(x);

  UNPROTECT(2);
  return out;
}
