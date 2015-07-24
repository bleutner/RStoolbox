#' RStoolbox: A Collection of Remote Sensing Tools
#' 
#' The RStoolbox package provides a set of functions which simplify performing standard remote sensing tasks in R.
#' Most functions have built-in parallel support. All that is required is to run \code{\link[raster]{beginCluster}} beforehand.
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
#' 	\item \code{\link{cloudShadowMask}}: mask cloud shadows from cloudMask
#'  \item \code{\link{rescaleImage}}: norm image to match min/max from another image or a specified min/max range.
#'  \item \code{\link{histMatch}}: matches the histograms of two scenes.
#'  \item \code{\link{coregisterImages}}: co-register images based on mutual information.
#'  \item \code{\link{panSharpen}}: sharpen a coarse resolution image with a high resolution image (typically panchromatic)
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
#' \item \code{\link{ggR}}: single raster layer plotting with ggplot2
#' \item \code{\link{ggRGB}}: efficient plotting of remote sensing imagery in RGB with ggplot2
#' }
#' 
#' @import raster sp rgeos plyr caret stringr reshape2 ggplot2 
#' @importFrom foreach getDoParRegistered 
#' @importFrom doParallel registerDoParallel
#' @importFrom codetools findGlobals
#' @importFrom geosphere areaPolygon
#' @importFrom parallel parLapply parSapply parApply clusterExport
#' @importFrom XML xmlParse xmlToList
#' @importFrom stats coefficients lm ecdf median approxfun knots kmeans na.omit complete.cases loadings princomp
#' @importFrom graphics par abline
#' @importFrom methods as
#' @importFrom utils read.csv read.delim str write.table data
#' @importFrom grDevices hsv
#' @useDynLib RStoolbox
#' @importFrom Rcpp sourceCpp
#' @docType package
#' @name RStoolbox
NULL





#' Rlogo as RasterBrick
#'
#' Tiny example of raster data used to run examples. 
#' 
#' @usage data(rlogo)
#' @docType data
#' @keywords datasets
#' @name rlogo
NULL