#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat predictMlcCpp(NumericMatrix newdata, List model, int nclasses){
int ns = newdata.nrow();
arma::mat out(ns, nclasses + 1, arma::fill::zeros);

for(int c = 0; c < nclasses; c++){
    List classMod = model[c];
    NumericVector m = classMod["m"];
    for(int s = 0; s < ns; s++){
      NumericVector xm(m.size());
      for(int i = 0; i < m.size(); i++){
            xm[i] = newdata(s,i) - m[i] ;
      }
      arma::mat dum = arma::rowvec(xm) * as<arma::mat>(classMod["I"]) * arma::colvec(xm);
      double dummy = dum(0,0);
      double deter = classMod["D"];
      out(s,c + 1) = deter - dummy;
     }
}

for(int s = 0; s < ns; s++){
  arma::uword index = out.submat(s, 1, s, nclasses).index_max();
	out(s,0) = index + 1;
}

return out;
}
