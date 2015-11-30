#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericMatrix rescaleImageCpp(NumericMatrix x, NumericVector scal, NumericVector xmin, NumericVector ymin){
	for(int i = 0; i < scal.size(); i++){
		x(_, i) = (x(_, i) - xmin[i]) * scal[i] + ymin[i];
	}
	return x;
}
