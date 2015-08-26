#include<Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
IntegerVector classQA(NumericVector& x, NumericMatrix rcl){
    int xs = x.size();
    int rclr = rcl.nrow();
    IntegerVector out(xs);
    std::fill(out.begin(), out.end(), NumericVector::get_na() );
    for(int r = 0; r < rclr; r++){
          for(int j = 0; j < xs; j++){
            if(rcl(r,0) == x[j]) out[j] = rcl(r,1);
          }
    }
return out;
}

