#' RStoolbox: A Collection of Remote Sensing Tools
#' 
#' The RStoolbox package provides a set of functions which simplify performing standard remote sensing tasks in R.
#' 
#' @section Data import:
#' 
#' \itemize{
#'  \item \code{\link{readMeta}}: import Landsat metadata from MTL or XML files
#'  \item \code{\link{stackMeta}}: load Landsat bands based on metadata
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
#' @import raster sp rgeos geosphere plyr caret stringr XML reshape2 ggplot2 proxy
#' @docType package
#' @name RStoolbox
NULL
