#' Spectral Angle Mapper
#' 
#' @param x RasterBrick or RasterStack. Imagery (usually hyperspectral)
#' @param em Matrix containing endmembers in rows and bands in columns
specSimMap <- function(x, em, method = "sam"){
    
    if(method=="R"){
        sa <- function(emi, xmat){
            acos(colSums(xmat*emi) /  sqrt(colSums(xmat^2) * sum(emi^2)))
        }      
        out <- calc(x, fun = function(xmat, emc = em){ 
                    xmat <- t(xmat)
                    apply(emc, 1, sa, xmat = xmat)
                }, forcefun = TRUE)
    } else  if(method =="CPP"){
        out <- calc(x, fun = function(xi, emc=em) {specSimC(x=xi, em=emc)}, forcefun = TRUE)     
    } else {
        out <- calc(x, fun = function(xi, emc=em) {specSimCx(x=xi, em=emc)}, forcefun = TRUE)
    }
    
    
    out
    
}

