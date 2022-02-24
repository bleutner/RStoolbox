#include <Rcpp.h>
using namespace Rcpp;

// @title Predict kmeans based on assignment to the nearest cluster center
// @param x Matrix
// @param centers Matrix: kmeans cluster centers
// @param returnDistance boolean: return distance to cluster centers
// @keywords internal
// [[Rcpp::export]]
NumericMatrix predKmeansCpp(NumericMatrix& x, NumericMatrix& centers, const bool returnDistance = false ){
  int ncent = centers.nrow();
  int nr = x.nrow();
  NumericMatrix out = no_init(nr, 1);
  // when returning classes we don't need to store all distances
  // hence we can keep `dist` small, i.e. 1 row only
  NumericMatrix dist = no_init(((nr-1) * returnDistance) + 1, ncent); 
  
  if (!returnDistance) {
    std::fill(out.begin(), out.end(), R_NaReal);
  }
  std::fill(dist.begin(), dist.end(), R_NaReal);
  
  for(int i = 0; i < nr; i++) {
    if(all(!Rcpp::is_na(x.row(i)))){
      
      for(int c = 0; c < ncent; c++){
        NumericVector d = centers(c,_) - x(i,_);
        dist(i * returnDistance, c) = sqrt(sum(d * d));
      }
      
      if(!returnDistance) {
        out(i,0) = which_min(dist.row(i * returnDistance)) + 1.0;
      }
    }
  }
  
  if(returnDistance) {
    return dist;
  } else {
    return out;
  }
}