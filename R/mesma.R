#' Multiple Endmember Spectral Mixture Analysis (Spectral Unmixing)
#' 
#' \code{mesma} performs a multiple endmember spectral mixture analysis on a multiband raster image.
#' 
#' @param img RasterLayer or RasterBrick or SpatRaster. Remote sensing imagery (usually hyperspectral).
#' @param em Matrix or data.frame with spectral endmembers. Rows represent a single endmember of a class, columns represent the spectral bands (i.e. columns correspond to number of bands in \code{img}). Number of rows needs to be > 1.
#' @param method Character. Select an unmixing method. Currently, only "NNLS" is implemented. Default is "NNLS".
#' \itemize{
#'    \item \code{NNLS:} applies a non-negative least squares (NNLS) regression which is using a sequential coordinate-wise algorithm (SCA) based on Franc et al. (2005).
#' }
#' @param iterate Integer. Set maximum iteration per pixel. Processing time could increase the more iterations are made possible. Default is 400.
#' @param tolerance Numeric. Tolerance limit representing a nearly zero minimal number. Default is 1e-8. 
#' @param verbose Logical. Prints progress messages during execution.
#' @param ... further arguments passed to \link[terra]{writeRaster}.
#' 
#' @return SpatRaster. The object will contain one band per endmember, with each value representing the estimated presence probability of the endmember per pixel (0 to 1), and an RMSE band.
#' 
#' @note Depending on \code{iterate} and \code{tolerance} settings, the sum of estimated presence probabilites per pixel varies around 1.
#' 
#' @references Franc, V., Hlaváč, V., & Navara, M. (2005). Sequential coordinate-wise algorithm for the non-negative least squares problem. In: International Conference on Computer Analysis of Images and Patterns (pp. 407-414). Berlin, Heidelberg.
#' 
#' @author Jakob Schwalb-Willmann
#' @examples
#' 
#' #load packages
#' library(terra)
#' library(RStoolbox)
#'
#' 
#' #make up some endmember spectra: water and land
#' em_names <- c("water", "land")
#' pts <- data.frame(class=em_names, cell = c(47916,5294))
#' em <- lsat[pts$cell]
#' rownames(em) <- em_names
#' 
#' #unmix the image for water and land
#' probs <- mesma(lsat, em, method = "NNLS")
#' 
#' #take a look
#' terra::hist(probs$water)
#' terra::plot(probs$water, col = c("white","blue"))
#' terra::hist(probs$land)
#' terra::plot(probs$land, col = c("white","brown"))
#' 
#' @export
#' 
mesma <- function(img, em, method = "NNLS", iterate = 400, tolerance = 0.00000001, ..., verbose){
  img <- .toTerra(img)
  ## messages
  if(!missing("verbose")) .initVerbose(verbose)
  verbose <- getOption("RStoolbox.verbose")
  
  ## breaking pre-checks
  .vMessage("Checking user inputs...")
  if(!inherits(img, "SpatRaster")){
    stop("'img' needs to be a 'SpatRaster' object.")
  }
  if(!inherits(em, c("matrix", "data.frame"))){
    stop("'em' needs to be a 'matrix' or 'data.frame' class object.")
  }
  if(inherits(em, "data.frame")){
    em <- as.matrix(em)
  }
  if(anyNA(em)){
    stop("'em' is not allowed to contain NA values. Spectra must be consistent.")
  }
  
  method <- toupper(method) 
  meth_avail <- c("NNLS") #available methods

  if(!inherits(method, "character")){
    stop("'method' needs to be a 'character' class object.")
  }
  if(!method %in% meth_avail){
    stop(paste0("Unknown 'method': '", method, "'"))
  }
  if(length(em[,1]) < 2){
    stop("'em' must contain at least two endmembers (number of rows in 'em').")
  }
  if(length(em[1,]) != nlyr(img)){
    stop("'em' and 'img' have different numbers of spectral features (number of columns in 'em'). Both need to represent the same number of spectral bands for equal spectral resolutions/ranges.")
  }
  
  ## hand over to C++ nnls_solver
  .vMessage(paste0("Unmixing imagery using '", method, "'..."))
  if(method == "NNLS"){
    probs <- .paraRasterFun(img, rasterFun = app, args = list(fun = function(xi, na.rm = FALSE) nnls_solver(x = xi, A = em, iterate = iterate, tolerance = tolerance)), wrArgs = list(...))
  }
  
  ## assign band names
  if(length(rownames(em)) != 0){
    names(probs)[1:(nlyr(probs)-1)] <- rownames(em)
  }
  names(probs)[nlyr(probs)] <- "RMSE"
  
  ## return brick
  return(probs)
}