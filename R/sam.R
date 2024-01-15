#' Spectral Angle Mapper
#' 
#' Calculates the angle in spectral space between pixels and a set of reference spectra (endmembers) for image classification based on spectral similarity.
#' 
#' @param img RasterBrick or RasterStack or SpatRaster. Remote sensing imagery.
#' @param em Matrix or data.frame with endmembers. Each row should contain the endmember spectrum of a class, i.e. columns correspond to bands in \code{img}. It is reccomended to set the rownames to class names.
#' @param angles Logical. If \code{TRUE} a RasterBrick containing each one layer per endmember will be returned containing the spectral angles.
#' @param ... further arguments to be passed to \code{\link[terra]{writeRaster}}
#' @export 
#' @details 
#' For each pixel the spectral angle mapper calculates the angle between the vector defined by the pixel values and each endmember vector. The result of this is
#' one raster layer for each endmember containing the spectral angle. The smaller the spectral angle the more similar a pixel is to a given endmember class.
#' In a second step one can the go ahead an enforce thresholds of maximum angles or simply classify each pixel to the most similar endmember. 
#' @return SpatRaster
#' If \code{angles = FALSE} a single Layer will be returned in which each pixel is assigned to the closest endmember class (integer pixel values correspond to row order of \code{em}. 
#' @examples 
#' library(terra)
#' library(ggplot2)
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
#' ggR(lsat_sam, forceCat = TRUE, geom_raster=TRUE) +
#'         scale_fill_manual(values = c("blue", "green"), labels = c("water", "vegetation"))
sam <- function(img, em, angles = FALSE, ...){
	img <- .toTerra(img)
	
    if(is.vector(em)) {
        em <- matrix(em, nrow = 1, ncol=length(em))
    } else if (is.data.frame(em)) {
        em <- as.matrix(em)
    }

    em <- em[,-1]
    if(ncol(em) != nlyr(img)) stop("The number of columns in em must match the number of bands in x.")
    if(!angles && nrow(em) == 1){
        stop(paste0("With only one class an image classification does not make sense.",
                    "\nUse angles=TRUE to calculate the spectral angles for your class without adding a classification on top."),
            call. = FALSE )
    }
    
    ## Calculate angles
    out <- app(img, fun = function(xi, emc=em) {specSimC(x=xi, em=emc)}, ... )     
    if(is.null(rownames(em))) rownames(em) <- paste0("s", 1:nrow(em)) 
    names(out) <- paste0(rownames(em), "_sa")
   
    ## Select minimum angle
    if(!angles) {
        out <- which.min(out)
        names(out) <- "class"
    }
    return(out)
    
}
