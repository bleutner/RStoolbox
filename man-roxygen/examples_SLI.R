#' @examples 
#' 
#' ## Example data
#' sliFile <- system.file("external/vegSpec.sli", package="RStoolbox")
#' sliTmpFile <- paste0(tempdir(),"/vegetationSpectra.sli") 
#' 
#' ## Read spectral library
#' sli <- readSLI(sliFile)
#' head(sli)
#' plot(sli[,1:2], col = "orange", type = "l")
#' lines(sli[,c(1,3)], col = "green")
#'  
#' ## Write to binary spectral library
#' writeSLI(sli, path = sliTmpFile)
