#' Spectral Angle Mapper
#' 
#' Calculates the angle in spectral space between pixels and a set of reference spectra (endmembers) for image classification based on spectral similarity.
#' 
#' @param x RasterBrick or RasterStack. Remote sensing imagery (usually hyperspectral)
#' @param em Matrix containing endmembers in rows and bands in columns
#' @param angles Logical. If \code{TRUE} a RasterBrick containing each one layer per endmember will be returned containing the spectral angles. 
#' @export 
#' @details 
#' For each pixel the spectral angle mapper calculates the angle between the vector defined by the pixel values and each endmember vector. The result of this is
#' one raster layer for each endmember containing the spectral angle. The smaller the spectral angle the more similar a pixel is to a given endmember class.
#' In a second step one can the go ahead an enforce thresholds of maximum angles or simply classify each pixel to the most similar endmember. 
#' @return RasterBrick or RasterLayer
#' If \code{angles = FALSE} a single Layer will be returned in which each pixel is assigned to the closest endmember class (integer pixel values correspond to row order of \code{em}. 
#' @examples 
#' library(raster)
#' library(ggplot2)
#' ## Load example data-set
#' data(lsat) 
#' 
#' ## Sample endmember spectra 
#' ## First location is water, second is open agricultural vegetation
#' pts <- data.frame(x = c(624720, 627480), y = c(-414690, -411090))
#' endmembers <- extract(lsat, pts)
#' rownames(endmembers) <- c("water", "vegetation")
#' 
#' ## Calculate spectral angles
#' lsat_sam <- sam(lsat, endmembers, angles = TRUE)
#' plot(lsat_sam)
#' 
#' ## Classify based on minimum angle
#' lsat_sam <- sam(lsat, endmembers, angles = FALSE)
#' 
#' \donttest{ggR(lsat_sam, forceCat = TRUE, geom_raster=TRUE) + 
#' 		scale_fill_manual(values = c("blue", "green"), labels = c("water", "vegetation"))}
sam <- function(x, em, angles = FALSE){
    
    if(ncol(em) != nlayers(x)) stop("The number of columns in em must match the number of bands in x.")
    
    
    ## Calculate angles
    out <- calc(x, fun = function(xi, emc=em) {specSimC(x=xi, em=emc)}, forcefun = TRUE)     
    if(is.null(rownames(em))) rownames(em) <- paste0("s", 1:nrow(em)) 
    names(out) <- paste0(rownames(em), "_sa")
   
    ## Select minimum angle
    if(!angles) out <- which.min(out)
    
    return(out)
    
}

