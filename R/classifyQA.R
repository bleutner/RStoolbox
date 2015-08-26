#' Classify Landsat8 QA Band
#' 
#' extracts five classes from QA band: background, cloud, cirrus, snow and water.
#' 
#' @param img RasterLayer. Landsat 8 OLI QA band. 
#' @param ... further arguments passed to \link[raster]{writeRaster}
#' @seealso \link{encodeQA} \link{decodeQA}
#' @return Returns a RasterLayer with five classes:
#' \tabular{rr}{
#' class \tab value \cr
#' background \tab 1L \cr 
#' cloud  \tab 2L \cr
#' cirrus \tab 3L \cr
#' snow   \tab 4L \cr
#' water  \tab 5L \cr 
#' }
#' @export 
classifyQA <- function(img, ...){
    rcl <- rbind(
            cbind(is = encodeQA(cloud = "high"), becomes = 2),
            cbind(is = encodeQA(cirrus = "high"), becomes = 3),
            cbind(is = encodeQA(snow = "high"), becomes = 4),
            cbind(is = encodeQA(water = "high"), becomes = 5))
    mcl <- reclassify(img, rcl = rcl)
    out <- clamp(mcl, lower=0, upper = 5, useValues = FALSE, datatype = "INT1U", ...)
}


