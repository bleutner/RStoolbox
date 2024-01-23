#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericMatrix rescaleImageCpp(NumericMatrix x, NumericVector scal, NumericVector xmin, NumericVector ymin){
  LogicalVector finite = is_finite(scal) & is_finite(xmin) & is_finite(ymin);
	for(int i = 0; i < scal.size(); i++){
	  //
	  if(!finite[i]) {
	    for(int r = 0; r < x.nrow(); r++) {
	      	    x(r,i) = NA_REAL;
	    }
	  } else {
	    x(_, i) = (x(_, i) - xmin[i]) * scal[i] + ymin[i];
	  }
	}
	return x;
}
