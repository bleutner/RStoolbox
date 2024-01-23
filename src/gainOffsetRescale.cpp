#include<Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericMatrix gainOffsetRescale( NumericMatrix x,  NumericVector g, NumericVector o, LogicalVector clamp){
	int nl = x.ncol();
	int nr = x.nrow();
	for(int i = 0 ; i < nl ; i++){
		for(int p = 0; p < nr; p++){
			double db = x(p,i) * g[i] + o[i];
			if(clamp[0] && db < 0.0) db = 0.0;
			if(clamp[1] && db > 1.0) db = 1.0;
			x(p,i) = db;
		}
	}
	return x;
}
