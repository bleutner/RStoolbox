#include<Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericVector pwSimilarityCpp(NumericMatrix& img, NumericMatrix& ref, int method){
	int nr = img.nrow();
	NumericVector out(nr), a, b;

	if(method == 1){
		for(int i = 0; i < nr; i++){
			out[i] =  -1.0 * sqrt(sum(pow((ref(i,_) - img(i,_)), 2)));
		}
	} else if(method == 2){
		for(int i = 0; i < nr; i++){
			out[i] =   acos(sum(img(i,_)*ref(i,_))/sqrt(sum(pow(img(i,_), 2))*sum(pow(ref(i,_), 2))));
		}
	} else if(method == 3){
		for(int i = 0; i < nr; i++){
			a = img(i,_) - mean(img(i,_));
			b = ref(i,_) - mean(ref(i,_));
			out[i] = sum(a*b)/sqrt(sum(pow(a,2))*sum(pow(b,2)));
		}
	}
	return out;
}
