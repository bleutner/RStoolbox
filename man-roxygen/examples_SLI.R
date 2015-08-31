#' @examples 
#' ## Create fake spectra
#' data <- data.frame(wavelength=350:2500, spectrumA=cumsum(abs(rnorm(2151))), 
#' 						spectrumB=cumsum(abs(rnorm(2151))))
#' pathToFile <- paste0(tempdir(),"/specLib.sli")
#' 
#' ## Write to binary spectral library
#' writeSLI(x = data, path = pathToFile)
#' 
#' ## Read from binary spectral library
#' dataRe <- readSLI(path = pathToFile)
#' 
#' ## Check whether they are the same
#' all.equal(data, dataRe)
