#include <Rcpp.h>
using namespace Rcpp;

// @param x NumericVector. Data passed from rasterLayer
// @param classes NumericVector. Classes to be one hot encoded
// @param bg int background value
// @param fg int foreground value
// @param na_rm bool: if na_rm=TRUE, NAs will be set to background value
// @keywords internal
// [[Rcpp::export]]
IntegerMatrix oneHotCpp(NumericVector & x, NumericVector & classes,  int bg,  int fg, bool na_rm) {
    const int nr=x.size();
    const int nc=classes.size();
    IntegerMatrix out(nr,nc);
    if(bg != 0) std::fill( out.begin(), out.end(), bg ) ;
    IntegerVector na_temp(nc,NA_INTEGER);

    for (int i = 0; i < nr; i++) {
        if(!na_rm && ISNAN(x(i))) {
            out(i,_) = na_temp;
        } else {
            for(int c = 0; c < nc; c++) {
                if (x(i) == classes(c)) {
                    out(i, c) = fg;
                    break ;
                }
            }
        }
    }

    return out;
}
