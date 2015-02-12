#' RStoolbox: A Collection of Remote Sensing Tools
#' 
#' The RStoolbox package provides a set of functions which simplify performing standard remote sensing tasks in R.
#' 
#' @section Data Import and Export:
#'  
#' \itemize{
#'  \item \code{\link{readMeta}}: import Landsat metadata from MTL or XML files
#'  \item \code{\link{stackMeta}}: load Landsat bands based on metadata
#'  \item \code{\link{readSLI} & \link{writeSLI}}: read and write ENVI spectral libraries
#'  \item \code{\link{saveRSTBX} & \link{readRSTBX}}: save and re-import RStoolbox classification objects (model and map)
#' }
#' 
#' @section Data Pre-Processing:
#' 
#' \itemize{
#'  \item \code{\link{radCor}}: radiometric conversions and corrections. Primarily, yet not exclusively, intended for Landsat data processing. DN to radiance to reflectance conversion as well as DOS approaches
#'  \item \code{\link{cloudMask}}: mask clouds in Landsat or other imagery which comes with a thermal band
#'  \item \code{\link{histMatch}}: matches the histograms of two scenes
#' }
#' 
#'@section Data Analysis:
#' 
#' \itemize{
#' \item \code{\link{spectralIndices}}: calculate a set of predefined multispectral indices like NDVI
#' \item \code{\link{rasterPCA}}: principal components transform for raster data
#' \item \code{\link{unsuperClass}}: unsupervised classification
#' \item \code{\link{superClass}}: supervised classification
#' \item \code{\link{fCover}}: fractional cover of coarse resolution imagery based on high resolution classificaton
#' }
#' 
#' @section Data Display:
#' 
#' \itemize{
#' \item \code{\link{ggRGB}}: efficient plotting of remote sensing imagery in RGB with ggplot2
#' }
#' 
#' @import raster sp rgeos plyr caret stringr reshape2 ggplot2 
#' @importFrom codetools findGlobals
#' @importFrom geosphere areaPolygon
#' @importFrom parallel parLapply parSapply parApply clusterExport
#' @importFrom proxy dist
#' @importFrom XML xmlParse xmlToList
#' @useDynLib RStoolbox
#' @importFrom Rcpp sourceCpp
#' @docType package
#' @name RStoolbox
NULL
