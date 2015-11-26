#' Classify Landsat8 QA Band
#' 
#' extracts five classes from QA band: background, cloud, cirrus, snow and water.
#' 
#' @param img RasterLayer. Landsat 8 OLI QA band. 
#' @param type Character. Classes which should be returned. One or more of c("background", "cloud", "cirrus","snow", "water").
#' @param confLayers Logical. Return one layer per class classified by confidence levels, i.e. cloud:low, cloud:med, cloud:high.
#' @param ... further arguments passed to \link[raster]{writeRaster}
#' @seealso \link{encodeQA} \link{decodeQA}
#' @details 
#' By default each class is queried for *high* confidence. See \link{encodeQA} for details. To return the different confidence levels per condition use \code{confLayers=TRUE}.
#' This approach corresponds to the way LandsatLook Quality Images are produced by the USGS.
#' @return Returns a RasterLayer with maximal five classes:
#' \tabular{rr}{
#' class \tab value \cr
#' background \tab 1L \cr 
#' cloud  \tab 2L \cr
#' cirrus \tab 3L \cr
#' snow   \tab 4L \cr
#' water  \tab 5L \cr 
#' }
#' Values outside of these classes are returned as NA.
#' If \code{confLayers = TRUE} then a RasterStack with one layer per condition (except 'background') is returned, whereby each layer contains the confidence level of the condition.
#' \tabular{rr}{
#' Confidence \tab value \cr
#' low    \tab 1L \cr
#' med    \tab 2L \cr
#' high   \tab 3L \cr
#' }
#' @export 
#' @examples
#' library(raster)
#' qa <- raster(ncol = 100, nrow=100, val = sample(1:2^14,  10000))
#' 
#' ## QA classes
#' qacs <- classifyQA(img = qa)
#' ## Confidence levels
#' qacs_conf <- classifyQA(img = qa, confLayers = TRUE)
classifyQA <- function(img, type = c("background", "cloud", "cirrus","snow", "water"), confLayers = FALSE, ...){
	if(any(!type %in% c("background", "cloud", "cirrus","snow", "water")) | !length(type)) stop("type must be element of c('background', 'cloud', 'cirrus','snow', 'water')")
	if(nlayers(img) != 1) stop("img should be a single RasterLayer")   
	
	if(!confLayers){
		
		rclx <- rbind(
				if("background" %in% type) cbind(is = encodeQA(fill = "yes"),    becomes = 1L),
				if("cloud" %in% type)  cbind(is = encodeQA(cloud = "high"),  becomes = 2L),
				if("cirrus" %in% type) cbind(is = encodeQA(cirrus = "high"), becomes = 3L),
				if("snow" %in% type)   cbind(is = encodeQA(snow = "high"),   becomes = 4L),
				if("water" %in% type)  cbind(is = encodeQA(water = "high"),  becomes = 5L))
		
		out <- .paraRasterFun(img, rasterFun = calc, args = list(fun = function(xi, na.rm = FALSE) classQA(x = xi, rcl = rclx), forcefun = TRUE), wrArgs = list(...))
		names(out) <- "QAclass"
		return(out)
	} else {
		
		rclxList <- list(
				cloud = rbind(			
						cbind(is = encodeQA(cloud = "low"),  becomes = 1L),
						cbind(is = encodeQA(cloud = "med"),  becomes = 2L),
						cbind(is = encodeQA(cloud = "high"),  becomes = 3L)),
				
				cirrus = rbind(
						cbind(is = encodeQA(cirrus = "low"), becomes = 1L),
						cbind(is = encodeQA(cirrus = "med"), becomes = 2L),
						cbind(is = encodeQA(cirrus = "high"), becomes = 3L)),
				
				snow =  rbind(		
						cbind(is = encodeQA(snow = "low"),   becomes = 1L),
						cbind(is = encodeQA(snow = "med"),   becomes = 2L),		
						cbind(is = encodeQA(snow = "high"),   becomes = 3L)),
				
				water = rbind(
						cbind(is = encodeQA(water = "low"),  becomes = 5L),
						cbind(is = encodeQA(water = "med"),  becomes = 5L),
						cbind(is = encodeQA(water = "high"),  becomes = 5L)))
		
		out <- lapply(type[type != "background"], function(i){ 
					.paraRasterFun(img, rasterFun = calc, args = list(fun = function(xi, na.rm = FALSE) classQA(x = xi, rcl = rclxList[[i]]), forcefun = TRUE))
				})
		
		out <- stack(out)
		names(out) <- type[type != "background"]
		return(out)
	}
	
}


