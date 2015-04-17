#include<Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericMatrix gainOffsetRescale(NumericMatrix x, NumericVector g, NumericVector o){
	int nl=x.ncol();
	for(int c = 0 ; c < nl ; ++c){
		x(_,c) = x(_,c) * g[c] + o[c];
	}
	return x;
}

