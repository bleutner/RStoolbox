#' RStoolbox: A Collection of Remote Sensing Tools
#' 
#' The RStoolbox package provides a set of functions which simplify performing standard remote sensing tasks in R.
#' 
#' @section Data import:
#' 
#' \itemize{
#'  \item{readMeta}{import Landsat metadata from MTL or XML files}
#'  \item{stackMeta}{load Landsat bands based on metadata}
#' }
#' 
#' @section Data Pre-Processing:
#' 
#' \itemize{
#'  \item{radCor}{radiometric conversions and corrections. Primarily, yet not exclusively, intended for Landsat data processing. DN to radiance to reflectance conversion as well as DOS approaches}
#'  \item{cloudMask}{mask clouds in Landsat or other imagery which comes with a thermal band}
#'  \item{histMatch}{matches the histograms of two scenes}
#' }
#' 
#'@section Data Analysis:
#' 
#' \itemize{
#' \item{spectralIndices}:{ calculate a set of predefined multispectral indices like NDVI}
#' \item{unsuperClass}{unsupervised classification}
#' \item{superClass}{supervised classification}
#' \item{fCover}{fractional cover of coarse resolution imagery based on high resolution classificaton}
#' }
#' 
#' @section Data Display:
#' 
#' \itemize{
#' \item{ggRGB}{efficient plotting of remote sensing imagery in RGB with ggplot2}
#' }
#' 
#' @import raster sp rgeos geosphere plyr caret stringr XML reshape2 ggplot2 
#' @docType package
#' @name RStoolbox
NULL
