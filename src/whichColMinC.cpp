#include <Rcpp.h>
using namespace Rcpp;

// Fast equivalent to apply(x, 1, which.min)
// @param x Matrix
// @keywords internal
// [[Rcpp::export]]
IntegerVector whichColMinC(NumericMatrix x, NumericMatrix centers){
	int ncent = centers.nrow();
    int nr = x.nrow();
 	IntegerVector out(nr);
	NumericVector dist(ncent);

	for(int i = 0; i < nr; ++i) {
		for(int c = 0; c < ncent; ++c){
			NumericVector d = centers(c,_) - x(i,_);
			dist[c] = sqrt(sum(d * d));
		}
	out[i] = which_min(dist) + 1;
	}
	return out;
}


/***
cppFunction('
IntegerVector whichColMinC(NumericMatrix x){

    int nr = x.nrow();
 	IntegerVector out(nr);

	for(int i = 0; i < nr; ++i) {
	out[i] = which_min(x(i,_)) + 1;
	}

	return out;
}
')
*/

