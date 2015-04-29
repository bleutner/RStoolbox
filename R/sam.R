#' Spectral Angle Mapper
#' 
#' @param x RasterBrick or RasterStack. Remote sensing imagery (usually hyperspectral)
#' @param em Matrix containing endmembers in rows and bands in columns
#' @param angles Logical. If \code{TRUE} a RasterBrick containing each one layer per endmember will be returned containing the spectral angles. 
#' If \code{FALSE} a single Layer will be returned in which each pixel is assigned to the closest endmember class.         
sam <- function(x, em, angles = FALSE){
    
    if(ncol(em) != nlayers(em)) stop("The number of columns in em must match the number of bands in x.")
    
    ## Calculate angles
    out <- calc(x, fun = function(xi, emc=em) {specSimC(x=xi, em=emc)}, forcefun = TRUE)     
    names(out) <- rownames(em)
   
    ## Select minimum angle
    if(!angles) out <- which.min(em)
    
    out
    
}

