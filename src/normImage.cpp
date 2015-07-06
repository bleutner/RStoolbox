#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericMatrix normImageCpp(NumericMatrix& x, NumericVector& M, NumericVector& S){
  int nc = x.ncol();
  for(int i = 0; i < nc; ++i){
   x(_,i) = (x(_,i) - M[i]) / S[i];
  }
return x;
}
