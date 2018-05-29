/*
Example of using the C API from stringdist in new package code.
Code and API is modeled after that of package xts.
*/

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include "stringdist_funcs.h"

SEXP C_test_lengths(SEXP x) {
  PROTECT(x);
  int n = length(x);
  SEXP out;
  out = PROTECT(allocVector(INTSXP, n));

  out = stringdist_lengths(x);

  UNPROTECT(2);
  return out;
}
