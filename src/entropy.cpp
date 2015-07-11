#include<Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericVector entropyCpp(NumericMatrix& x){
	int i, nr = x.nrow();
	int nl = x.ncol();
	NumericVector p, ptx, out(nr);

	for(i=0; i < nr; ++i){
		// FREQUENCY TABLE
		NumericVector tab = unique(x(i,_));
		NumericVector ts(tab.size());
		for(int c = 0; c < tab.size(); c++){
			for(int j = 0; j < nl; j++){
				if(x(i,j) == tab[c]) ts[c]++;
			}
		}
		// ENTROPY
		p = ts / nl;
		ptx = ifelse(p == 0, 0.0, p * log(p));
		out[i] = -1.0 * sum(ptx);
	}
	return out;
}

