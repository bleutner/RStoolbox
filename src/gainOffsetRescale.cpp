#include<Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericMatrix gainOffsetRescale(NumericMatrix x, NumericVector g, NumericVector o, LogicalVector clamp){
	int nl=x.ncol();
	for(int c = 0 ; c < nl ; ++c){
		x(_,c) = x(_,c) * g[c] + o[c];
		if(clamp[0]) x(_,c) = ifelse(x(_,c) < 0.0, 0.0, x(_,c));
		if(clamp[1]) x(_,c) = ifelse(x(_,c) > 1.0, 1.0, x(_,c));
	}
	return x;
}

