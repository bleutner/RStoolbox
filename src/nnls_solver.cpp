//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
using namespace std;
//using namespace arma; //included for simple mat. multiplication

//[[Rcpp::export]]
arma::mat nnls_solver(arma::mat x, arma::mat A, int iterate = 400, float tolerance = 0.000001){
  int A_nbands = A.n_cols;
  int b_nbands = x.n_cols;
  if( A_nbands != b_nbands) { // catch false inputs
    stop("A and b do not have equal column lengths.");
  }
  
  int A_nEM = A.n_rows;
  int b_npix = x.n_rows;
  arma::mat sol(b_npix, A_nEM+1);
  
  for(int i = 0; i < b_npix; i++){ // parallelization with clusterR possible with this framework? --> test
    
    //vec b_vpix = b.row(i);
    
    arma::vec xv(A_nEM), xstore(A_nEM);
    xv.fill(0);
    xstore.fill(-9999);
    arma::vec xdiff = xv - xstore;
    
    // switching to arma here for nice matrix multiplication
    arma::vec nab = -A * x.row(i).t(); // negative A * b
    arma::mat ata = A * A.t(); // A * transposed A
    
    double temporary;
    int j = 0;
    
    //execute solving loop
    while(j < iterate && max(abs(xdiff)) > tolerance) {
      xstore = xv;
      
      for (int k = 0; k < A_nEM; k++) {
        
        temporary = xv[k] - nab[k] / ata(k,k);
        if (temporary < 0){
          temporary = 0;
        }
        
        if (temporary != xv[k]){
          nab += ((temporary - xv[k]) * ata.row(k).t());
        }
        
        xv[k] = temporary;
      }
      xdiff = xv-xstore;
      ++j;
    }
    
    //predict values
    arma::mat prob = xv.t();
    arma::mat pred = prob * A;
    
    //calculate RMSE
    arma::mat ppdiff = pred.row(0) - x.row(i);
    float rmsem = mean(mean(pow(ppdiff, 2)));
    float rmse = sqrt(rmsem);
    
    arma::mat ret(1, (A_nEM+1));
    
    for(int f = 0; f < A_nEM; f++) {
      ret(0,f) = prob(0,f);
    }
    
    //fill
    ret(0,A_nEM) = rmse;
    sol.row(i) = ret; //xv.t();
  }
  return(sol); //mat
}
