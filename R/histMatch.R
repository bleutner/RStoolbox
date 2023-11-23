#' Image to Image Contrast Matching
#' 
#' Performs image to image contrast adjustments based on histogram matching using empirical cumulative
#'  distribution functions from both images.
#' 
#' @param x terra SpatRaster. Source raster which is to be modified.
#' @param ref terra SpatRaster. Reference raster, to which x will be matched.
#' @param xmask RasterLayer. Mask layer for \code{x} to exclude pixels which might distort the histogram, i.e. are not present in \code{ref}. Any NA pixel in \code{xmask} will be ignored (\code{maskvalue = NA}). 
#' @param refmask RasterLayer. Mask layer for \code{ref}. Any NA pixel in \code{refmask} will be ignored (\code{maskvalue = NA}). 
#' @param nSamples Integer. Number of random samples from each image to build the histograms.
#' @param intersectOnly Logical. If \code{TRUE} sampling will only take place in the overlap extent of the two rasters. Otherwise the full rasters will be used for sampling.
#' @param paired Logical. If \code{TRUE} the corresponding pixels will be used in the overlap.
#' @param returnFunctions Logical. If \code{TRUE} the matching functions will be returned instead of applying them to \code{x}. 
#' @param ... Further arguments to be passed to \link[raster]{writeRaster}.
#' @param forceInteger Logical. Force integer output.
#' @note \code{x} and \code{ref} must have the same number of layers.
#' @return A Raster* object of \code{x} adjusted to the histogram of \code{ref}. If \code{returnFunctions  = TRUE} a list of functions (one for each layer) will be returned instead. 
#' @references Richards and Jia: Remote Sensing Digital Image Analysis. Springer, Berlin, Heidelberg, Germany, 439pp.
#' @export
#' @examples 
#' library(ggplot2)
#' library(raster)
#' data(rlogo)
#' ## Original image a (+1 to prevent log(0))
#' img_a <-  rlogo + 1 
#' ## Degraded image b
#' img_b <- log(img_a)
#' ## Cut-off half the image (just for better display)
#' img_b[, 1:50] <- NA
#' 
#' ## Compare Images before histMatching
#' ggRGB(img_a,1,2,3)+
#'         ggRGB(img_b, 1,2,3, ggLayer = TRUE, stretch = "lin", q = 0:1) +
#'         geom_vline(aes(xintercept = 50))+
#'         ggtitle("Img_a vs. Img_b")
#' 
#' ## Do histogram matching
#' img_b_matched <- histMatch(img_b, img_a)
#' 
#' ## Compare Images after histMatching
#' ggRGB(img_a, 1, 2, 3)+
#'         ggRGB(img_b_matched, 1, 2, 3, ggLayer = TRUE, stretch = "lin", q = 0:1) +
#'         geom_vline(aes(xintercept = 50))+
#'         ggtitle("Img_a vs. Img_b_matched")
#' 
#' ## Histogram comparison
#' opar <- par(mfrow = c(1, 3), no.readonly = TRUE)
#' img_a[,1:50] <- NA
#' redLayers <- stack(img_a, img_b, img_b_matched)[[c(1,4,7)]]
#' names(redLayers) <- c("img_a", "img_b", "img_b_matched")
#' 
#' hist(redLayers) 
#' ## Reset par 
#' par(opar)
histMatch <- function(x, ref, xmask = NULL, refmask = NULL, nSamples = 1e5, intersectOnly = TRUE, paired = TRUE,
                      forceInteger = FALSE, returnFunctions = FALSE, ...) {
  nSamples <- min(ncell(ref), nSamples, ncell(ref))

  x <- .toTerra(x)
  ref <- .toTerra(ref)
  ## Define intersecting extent if required. Returns NULL if FALSE
  ext <- if (paired | intersectOnly) intersect(ext(x), ext(ref))
  if (paired & is.null(ext)) {
    paired <- FALSE
    warning("Rasters do not overlap. Precise sampling disabled.", call. = FALSE)
  }
  if (.nlyr(x) != .nlyr(ref)) stop("x and ref must have the same number of layers.")

  if (!is.null(xmask)) {
    .vMessage("Apply xmask")
    xfull <- x
    x <- mask(x, xmask)
  }
  if (!is.null(refmask)) {
    .vMessage("Apply refmask")
    ref <- c(ref, refmask)
  }
  ## Sample histogram data
  .vMessage("Extract samples")

  ref.sample <- as.matrix(spatSample(ref, size = nSamples, na.rm = TRUE, ext = ext, xy = paired))
  if (!is.null(refmask)) ref.sample <- ref.sample[, -ncol(ref.sample), drop = FALSE]

  if (paired) {
    x.sample <- extract(x, ref.sample[, c("x", "y")])
    if (is.vector(x.sample)) x.sample <- as.matrix(x.sample)
    valid <- complete.cases(x.sample)
    ref.sample <- ref.sample[valid, -c(1:2), drop = FALSE]
    x.sample <- x.sample[valid, , drop = FALSE]
  } else {
    x.sample <- as.matrix(spatSample(x, size = nSamples, na.rm = T, ext = ext))
  }

  .vMessage("Calculate empirical cumulative histograms")
  layerFun <- lapply(1:ncol(x.sample), function(i) {
    ## Estimate source empirical cumulative distribution function
    source.ecdf <- ecdf(x.sample[, i])

    ## Estimate and invert reference ecdf
    ecdf.ref <- ecdf(ref.sample[, i])
    kn <- knots(ecdf.ref)
    y <- ecdf.ref(kn)
    minmax <- c(min(values(ref)[i]), max(values(ref)[i]))
    limits <- if (is.na(minmax[1]) || is.na(minmax[2])) {
      range(ref.sample)
    }else{
      minmax
    }
    inverse.ref.ecdf <- approxfun(y, kn, method = "linear", yleft = limits[1], yright = limits[2], ties = "ordered")

    ## Function definition
    histMatchFun <- if (grepl("INT", datatype(ref)[i]) | forceInteger)
      function(values, na.rm = FALSE) { round(inverse.ref.ecdf(source.ecdf(values))) }
    else {
      function(values, na.rm = FALSE) { inverse.ref.ecdf(source.ecdf(values)) }
    }
    histMatchFun
  })

  totalFun <- function(xvals, f = layerFun) {
    if (is.vector(xvals)) xvals <- as.matrix(xvals)
    app <- lapply(1:ncol(xvals), function(i) {
      f[[i]](xvals[, i])
    })
    do.call("cbind", app)
  }

  ## Return LUT functions
  if (returnFunctions) {
    names(layerFun) <- names(x)
    return(layerFun)
  }
  ## Apply histMatch to raster
  .vMessage("Apply histogram match functions")

  out <- terra::app(x, fun = totalFun, ...)

  if (!is.null(xmask)) out <- merge(xfull, out, ..., overwrite = TRUE)
  names(out) <- names(x)

  out
}