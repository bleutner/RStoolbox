#' Multiple Endmember Spectral Mixture Analysis (Spectral Unmixing)
#' 
#' \code{mesma} performs a multiple endmember spectral mixture analysis on a multiband raster image. Currently, a sequential coordinate-wise algorthm is used to apply a non negative least square regression (NNLS).
#' 
#' @param img RasterBrick or RasterStack. Remote sensing imagery (usually hyperspectral).
#' @param em Matrix or data.frame with spectral endmembers. Rows represent a single endmember of a class, columns represent the spectral bands (i.e. columns correspond to number of bands in \code{img}). Number of rows needs to be > 1.
#' @param method Character. Select a unmixing method. Currently, only "NNLS" is implemented. Default is "NNLS".
#' @param iterate Integer. Set maximum iteration per pixel. Processing time could increase the more iterations are made possible. Default is 400.
#' @param tolerance Floating point. Tolerance limit represneting a nearly zero minimal number. Default is 0.000001. 
#' @param verbose Logical. Prints progress messages during execution.
#' @param ... further arguments passed to \link[raster]{writeRaster}.
#' 
#' @return RasterBrick. The object will contain one band per endmember, with each value representing the estimated presence propabiliy of the endmember per pixel (0 to 1), and an RMSE band.
#' 
#' @note Depending on \code{iterate} and \code{tolerance} settings, the sum of estimated presence propabilites per pixel varies around 1.
#' 
#' @author Jakob Schwalb-Willmann
#' @examples
#' 
#' #load packages
#' library(raster)
#' library(RStoolbox)
#' 
#' #load an example dataset
#' data(lsat)
#' 
#' #make up some endmember spectra: water and land
#' em_names <- c("water", "land")
#' pts <- data.frame(class=em_names, cell = c(47916,5294))
#' em <- lsat[pts$cell]
#' rownames(em) <- em_names
#' 
#' #unmix the image for water and land
#' props <- mesma(lsat, em, method = "NNLS")
#' 
#' #take a look
#' raster::hist(props$water)
#' raster::plot(props$water, col = c("white","blue"))
#' raster::hist(props$land)
#' raster::plot(props$land, col = c("white","brown"))
#' 
#' 
#' @export
#' 
mesma <- function(img, em, method = "NNLS", iterate = 400, tolerance = 0.00000001, ..., verbose){
  
  ## messages
  if(!missing("verbose")) .initVerbose(verbose)
  verbose <- getOption("RStoolbox.verbose")
  
  ## breaking pre-checks
  .vMessage("Checking user inputs...")
  if(class(img)[1] != "RasterBrick"){stop("'img' needs to be a 'RasterBrick' class object.")}
  if(class(em)[1] != "matrix"){stop("'em' needs to be a 'matrix' class object.")}
  meth_avail <- c("NNLS")
  if(class(method) != "character"){stop("'method' needs to be a 'character' class object.")} 
  if(!method %in% meth_avail){stop(paste0("Unknown 'method': '", method, "'"))}
  if(length(em[,1]) < 2){stop("'em' must contain at least two endmembers (number of rows in 'em').")}
  if(length(em[1,]) != dim(img)[3]){stop("'em' and 'img' have different numbers of spectral features (number of columns in 'em'). Both need to represent the same number of spectral bands for equal spectral resolutions/ranges.")}
  
  ## warnings
  if(tolerance > 0.00000001){warning("Caution! 'tolerance' is set to a higher number than default.")}
  if(iterate > 400){warning("Cauton! 'iterate' is set to a lower number than default.")}
  
  ## hand over to C++ nnls_solver
  .vMessage(paste0("Unmixing imagery using '", method, "'..."))
  if(method == "NNLS"){
    probs <- .paraRasterFun(img, rasterFun = calc, args = list(fun = function(xi, na.rm = FALSE) nnls_solver(x = xi, A = em, iterate = iterate, tolerance = tolerance), forcefun = TRUE), wrArgs = list(...))
  }
  
  ## assign band names
  if(length(rownames(em)) != 0){
    names(probs)[1:(nlayers(probs)-1)] <- rownames(em)
  }
  names(probs)[nlayers(probs)] <- "RMSE"
  
  ## return brick
  return(probs)
}