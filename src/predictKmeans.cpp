#include <Rcpp.h>
using namespace Rcpp;

// @title Predict kmeans based on assignment to the nearest cluster center
// @param x Matrix
// @keywords internal
// [[Rcpp::export]]
IntegerVector predKmeansCpp(NumericMatrix x, NumericMatrix centers){
	int ncent = centers.nrow();
	int nr = x.nrow();
	IntegerVector out(nr, IntegerVector::get_na());
	NumericVector dist(ncent);

	for(int i = 0; i < nr; i++) {
		if(any(!is_na(x(i,_)))){
			for(int c = 0; c < ncent; c++){
				NumericVector d = centers(c,_) - x(i,_);
				dist[c] = sqrt(sum(d * d));
			}
			out[i] = which_min(dist) + 1;
		}
	}

	return out;
}


