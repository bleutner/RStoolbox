#' One-hot encode a raster or vector
#' 
#' Splits a categorical raster layer (or a vector) into a multilayer raster (or matrix).
#' 
#' @param img RasterLayer, SpatRaster or integer/numeric vector containing multiple classes
#' @param classes integer: vector of classes which should be extracted
#' @param background integer: background value (default = 0)
#' @param foreground integer: foreground value (default = 1)
#' @param na.rm logical: if \code{TRUE}, \code{NA}s will be coerced to the \code{background} value.
#' @param ... further arguments passed to \link[terra]{writeRaster}. Ignored if img is not a RasterLayer, but a numeric/integer vector
#' @return A RasterBrick with as many layers as there are classes. 
#' Pixels matching the class of interest are set to 1, backround values by default are set to 0 (see background argument)
#' @export 
#' @examples 
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
	img <- .toTerra(img)
    stopifnot(inherits(img, c("SpatRaster", "integer", "numeric", "matrix")))
    if(inherits(img, "SpatRaster")) {
        if(nlyr(img)>1) {
          warning(paste0("oneHotEncode() currently works on single layers only, but `img` has ", .nlyr(img), " layers.",
                         "\nDefaulting to first layer.",
                         "\nSubmit a feature request at <https://github.com/bleutner/RStoolbox/issues> if you need it for more layers."),
                  call. = FALSE)
        }
        out <- app(img[[1]], 
                function(x, cl = classes, bg = background, fg = foreground, na.rm) 
                    oneHotCpp(x, classes = cl, bg = bg, fg = fg, na_rm = na.rm), 
                na.rm = na.rm,  ...)
        names(out) <- paste0("c_", classes)
    } else {
        out <- oneHotCpp(img, classes = classes, bg = background, fg = foreground, na_rm = na.rm)
        colnames(out) <- paste0("c_", classes)
    }
    out
}
