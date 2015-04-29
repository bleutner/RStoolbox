#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericMatrix specSimC(NumericMatrix& x, NumericMatrix& em) {
	int ns = x.nrow();
	int nend = em.nrow();
	NumericMatrix out(ns, nend);
	for(int j = 0; j < nend; ++j){
		double normEM = sum(pow(em(j,_),2));
		for(int i = 0; i < ns; ++i){
			out(i,j) = acos(sum(x(i,_) * em(j,_)) / sqrt(sum(pow(x(i,_),2)) * normEM)) ;
		}
	}
	return out;
}


// Alternative Armadillo version. same speed as Rcpp only
/*
 cppFunction("
		arma::mat specSimCx(arma::mat& x, arma::mat& em) {
	int ns = x.n_rows;
	int nend = em.n_rows;
	arma::mat out(ns, nend);
	out.zeros();
	arma::mat normEM = sum(pow(em,2),1);
	arma::mat normT  = sum(pow(x,2),1);

	for(int j = 0; j < nend; ++j){
			for(int i = 0; i < ns; ++i){
				out(i,j) = acos(accu(x.row(i) % em.row(j)) / sqrt(normT(i,0) * normEM(j,0))) ;
			}
		}

	return out;
}
", depends = "RcppArmadillo")
*/
