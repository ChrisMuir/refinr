#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

#ifndef _STRINGDIST_API_H
# include "stringdist_api.h"
#endif


SEXP stringdist_lengths(SEXP X) {
  return(sd_lengths(X));
}

SEXP stringdist_all_int(SEXP X) {
  return(sd_all_int(X));
}
