#' Spectral indices
#' 
#' @param inputRaster Raster* object. Typically remote sensing imagery, which is to be classified.
#' @param spectral index to apply
#' @param filename path to output file (optional). If \code{NULL}, standard raster handling will apply, i.e. storage either in memory or in the raster temp directory.
#' @param maskRaster Raster layer containing a binary mask to exclude areas from prediction.
#' @param verbose logical. prints progress, statistics and graphics during execution
#' @return the raster holding the index results  
#' @seealso \code{\link{...}} 
#' @export
spectralIndices <- function(inputRaster, index, filename = NULL, maskRaster = NULL, verbose = FALSE, overwrite = TRUE, ...) {
  # TODO: add indices
  # TODO: how to define indices with >3 bands? e.g. slavi = NIR / (RED + MIR) or EVI
  # TODO: add examples
  # required packages: raster   
  
  
  RedNirIndices <-  list(
                          ratio = (NIR/RED),
                          dvi = NIR-RED,
                          ndvi = (NIR-RED)/(NIR+RED), 
                          tvi = (((NIR-RED)/(NIR+RED))+0.5)^0.5, 
                          msavi = NIR + 0.5 - (0.5 * sqrt((2 * NIR + 1)^2 - 8 * (NIR - (2 * Red)))),
                          msavi2 = (2 * (NIR + 1) - sqrt((2 * NIR + 1)^2 - 8 * (NIR - Red))) / 2,
                          gemi = (((NIR^2 - RED^2) * 2 + (NIR * 1.5) + (RED * 0.5) ) / (NIR + RED + 0.5)) * (1 - ((((NIR^2 - RED^2) * 2 + (NIR * 1.5) + (RED * 0.5) ) / (NIR + RED + 0.5)) * 0.25)) - ((RED - 0.125) / (1 - RED))                   
                          
                          )
                                               
                                                 
                         
  ThreeBandIndices <- list(
                          slavi = NIR / (RED + MIR),
                          EVI = G * ((NIR - RED) / (NIR + C1 * RED - C2 * BLUE + L))# include a G or L specification in command
                  
                          )                      
