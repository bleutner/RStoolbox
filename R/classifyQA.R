#' Classify Landsat8 QA Band
#' 
#' extracts five classes from QA band: background, cloud, cirrus, snow and water.
#' 
#' @param img RasterLayer. Landsat 8 OLI QA band. 
#' @param type Character. Classes which should be returned. One or more of c("background", "cloud", "cirrus","snow", "water").
#' @param ... further arguments passed to \link[raster]{writeRaster}
#' @seealso \link{encodeQA} \link{decodeQA}
#' @details 
#' Each class is queried for *high* confidence. See \link{encodeQA} for details. 
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
#' @export 
#' @examples
#' library(raster)
#' qa <- raster(ncol = 100, nrow=100, val = sample(1:2^14,  10000))
#' qacs <- classifyQA(img = qa)
classifyQA <- function(img, type = c("background", "cloud", "cirrus","snow", "water"), ...){
    if(any(!type %in% c("background", "cloud", "cirrus","snow", "water")) | !length(type)) stop("type must be element of c('background', 'cloud', 'cirrus','snow', 'water')")
    if(nlayers(img) != 1) stop("img should be a single RasterLayer")   
    rclx <- rbind(
            if("background" %in% type) cbind(is = encodeQA(fill = "yes"),    becomes = 1L),
            if("cloud" %in% type) cbind(is = encodeQA(cloud = "high"),  becomes = 2L),
            if("cirrus" %in% type)  cbind(is = encodeQA(cirrus = "high"), becomes = 3L),
            if("snow" %in% type)  cbind(is = encodeQA(snow = "high"),   becomes = 4L),
            if("water" %in% type)  cbind(is = encodeQA(water = "high"),  becomes = 5L))
    .paraRasterFun(img, rasterFun = calc, args = list(fun = function(xi, na.rm = FALSE) classQA(x = xi, rcl = rclx), forcefun = TRUE), wrArgs = list(...))
}


