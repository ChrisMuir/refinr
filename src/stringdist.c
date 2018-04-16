/*
Example of using the C API from stringdist in new package code.
Code and API is modeled after that of packages xts and rcppxts.
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


/* max n for objects of length n(n-1).
 *
 */
#ifdef LONG_VECTOR_SUPPORT
#define MAXN ( (R_xlen_t) (0.5 + 1.5 * sqrt((double) R_XLEN_T_MAX)) )
#else
#define MAXN ( (R_xlen_t) (0.5 + 1.5 * sqrt((double) R_LEN_T_MAX)) )
#endif

SEXP get_lower_tri(SEXP a, SEXP method
                     , SEXP weight, SEXP p,  SEXP bt, SEXP q
                     , SEXP useBytes, SEXP nthrd)
{
  // Long vectors on platforms where LONG_VECTOR_SUPPORT is defined.
  R_xlen_t n = xlength(a)
  , N = n*(n-1)/2;

  if ( n > MAXN ){
    error("Length of input vector (%d) exceeds maximum allowed for this platform (%d)",n,MAXN);
  }

  SEXP yy;
  PROTECT(yy = allocVector(REALSXP, N));

  yy = sd_lower_tri(a, method, weight, p, bt, q, useBytes, nthrd);
  UNPROTECT(1);
  return yy;
}
