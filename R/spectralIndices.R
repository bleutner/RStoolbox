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
  # TODO: how to define indices with >3 bands?
  # TODO: add examples

  
  
  
  