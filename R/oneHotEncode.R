#' One-hot encode a raster or vector
#' 
#' Splits a categorical raster layer (or a vector) into a multilayer raster (or matrix).
#' 
#' @param img RasterLayer or integer/numeric vector containing multiple classes
#' @param classes integer: vector of classes which should be extracted
#' @param background integer: background value (default = 0)
#' @param foreground integer: foreground value (default = 1)
#' @param na.rm logical: if \code{TRUE}, \code{NA}s will be coerced to the \code{background} value.
#' @param ... further arguments passed to \link[raster]{writeRaster}. Ignored if img is not a RasterLayer, but a numeric/integer vector
#' @return A RasterBrick with as many layers as there are classes. 
#' Pixels matching the class of interest are set to 1, backround values by default are set to 0 (see background argument)
#' @export 
#' @examples 
#' library(raster)
#' 
#' ## example data
#' data(rlogo)
#' sc <- unsuperClass(rlogo, nClasses = 3)
#' 
#' ## one-hot encode 
#' sc_oneHot <- oneHotEncode(sc$map, classes = c(1,2,3))
#' 
#' ## check results
#' sc_oneHot
#' plot(sc_oneHot)
oneHotEncode <- function(img, classes, background = 0, foreground = 1, na.rm = FALSE, ...) {
	img <- .toRaster(img)
    stopifnot(inherits(img, c("RasterLayer", "integer", "numeric")))
    if(inherits(img, "RasterLayer")) {
        out <- calc(img, 
                function(x, cl = classes, bg = background, fg = foreground, na.rm) 
                    oneHotCpp(x, classes = cl, bg = bg, fg = fg, na_rm = na.rm), 
                na.rm = na.rm, forcefun = TRUE, ...)
        names(out) <- paste0("c_", classes)
    } else {
        out <- oneHotCpp(img, classes = classes, bg = background, fg = foreground, na_rm = na.rm)
        colnames(out) <- paste0("c_", classes)
    }
    out
}
