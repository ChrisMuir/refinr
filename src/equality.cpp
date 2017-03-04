#include <Rcpp.h>
#include"refinr_header.h"
using namespace Rcpp;

//' Compare a char string and char vector
//'
//' Takes a char vector and char string as input, compares the string to every
//' element of the char vector, output is a logical vector with length equal to
//' the input char vector. This is replacing the R code "vector == string".
//'
//' @param lookupvect Character vector.
//' @param charstring Character string.
// [[Rcpp::export]]
LogicalVector equality(CharacterVector lookupvect,
                       String charstring) {
  int vect_length = lookupvect.size();
  LogicalVector out(vect_length);

  for(int i = 0; i < vect_length; ++i) {
    out[i] = charstring == lookupvect[i];
  }
  return out;
}
