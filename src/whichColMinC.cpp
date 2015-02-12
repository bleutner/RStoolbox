#include <Rcpp.h>
using namespace Rcpp;

// Fast equivalent to apply(x, 1, which.min)
// @param x Matrix
// @keywords internal
// [[Rcpp::export]]

IntegerVector whichColMinC(NumericMatrix x){

    int nr = x.nrow();
 	IntegerVector out(nr);

	for(int i = 0; i < nr; ++i) {
	out[i] = which_min(x(i,_)) + 1;
	}

	return out;
}
