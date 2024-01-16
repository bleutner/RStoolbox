#' Plot RasterLayers in ggplot with greyscale
#' 
#' Plot single layer imagery in grey-scale. Can be used with a SpatRaster.
#' 
#' @param img raster
#' @param layer Character or numeric. Layername or number. Can be more than one layer, in which case each layer is plotted in a subplot.
#' @param maxpixels Integer. Maximal number of pixels to sample.
#' @param ext Extent object to crop the image
#' @param alpha Numeric. Transparency (0-1).
#' @param hue Numeric. Hue value for color calculation [0,1] (see \code{\link[grDevices]{hsv}}). Change if you need anything else than greyscale. Only effective if \code{sat > 0}.
#' @param sat Numeric. Saturation value for color calculation [0,1] (see \code{\link[grDevices]{hsv}}). Change if you need anything else than greyscale.
#' @param stretch Character. Either 'none', 'lin', 'hist', 'sqrt' or 'log' for no stretch, linear, histogram, square-root or logarithmic stretch.
#' @param quantiles Numeric vector with two elements. Min and max quantiles to stretch to. Defaults to 2\% stretch, i.e. c(0.02,0.98). 
#' @param ggObj Logical. Return a stand-alone ggplot object (TRUE) or just the data.frame with values and colors
#' @param ggLayer Logical. Return only a ggplot layer which must be added to an existing ggplot. If \code{FALSE} s stand-alone ggplot will be returned.
#' @param geom_raster Logical. If \code{FALSE} uses annotation_raster (good to keep aestetic mappings free). If \code{TRUE} uses \code{geom_raster} (and \code{aes(fill)}). See Details.
#' @param coord_equal Logical. Force addition of coord_equal, i.e. aspect ratio of 1:1. Typically useful for remote sensing data (depending on your projection), hence it defaults to TRUE.
#'         Note however, that this does not apply if (\code{ggLayer=FALSE}).
#' @param forceCat Logical. If \code{TRUE} the raster values will be forced to be categorical (will be converted to factor if needed). 
#' @seealso \link{ggRGB}, \link[=fortifySpatRaster]{fortifySpatRaster}
#' @return 
#' \tabular{ll}{
#'  \code{ggObj = TRUE}:   \tab ggplot2 plot \cr
#'  \code{ggLayer = TRUE}: \tab ggplot2 layer to be combined with an existing ggplot2 \cr
#'  \code{ggObj = FALSE}:  \tab data.frame in long format suitable for plotting with ggplot2, 
#'                          includes the pixel values and the calculated colors  \cr  
#' }
#' @details
#' When \code{img} contains factor values and \code{annotation=TRUE}, the raster values will automatically be converted
#' to numeric in order to proceed with the brightness calculation.
#' 
#' The geom_raster argument switches from the default use of annotation_raster to geom_raster. The difference between the two is that geom_raster performs
#' a meaningful mapping from pixel values to fill colour, while annotation_raster is simply adding a picture to your plot. In practice this means that whenever you 
#' need a legend for your raster you should use \code{geom_raster = TRUE}. This also allows you to specify and modify the fill scale manually. 
#' The advantage of using annotation_raster (\code{geom_raster = TRUE}) is that you can still use the scale_fill* for another variable. For example you could add polygons and 
#' map a value to their fill colour. For more details on the theory behind aestetic mapping have a look at the \href{https://CRAN.R-project.org/package=ggplot2/ggplot2.pdf}{ggplot2} manuals.
#' 
#' @export 
#' @examples
#' library(ggplot2)
#' library(terra)
#' 
#' ## Simple grey scale annotation
#' ggR(rlogo)
#' 
#' ## With linear stretch contrast enhancement
#' ggR(rlogo, stretch = "lin", quantiles = c(0.1,0.9))
#' 
#' ## ggplot with geom_raster instead of annotation_raster
#' ## and default scale_fill*
#' ggR(rlogo, geom_raster = TRUE)
#' 
#' ## with different scale
#' ggR(rlogo, geom_raster = TRUE) +
#'         scale_fill_gradientn(name = "mojo", colours = rainbow(10)) +
#'         ggtitle("**Funkadelic**")
#' 
#' ## Plot multiple layers
#' \donttest{
#' ggR(lsat, 1:6, geom_raster=TRUE, stretch = "lin") +
#'     scale_fill_gradientn(colors=grey.colors(100), guide = "none") +
#'     theme(axis.text = element_text(size=5),
#'           axis.text.y = element_text(angle=90),
#'           axis.title=element_blank())
#' 
#' ## Don't plot, just return a data.frame
#' df <- ggR(rlogo, ggObj = FALSE)
#' head(df, n = 3)
#' 
#' ## Layermode (ggLayer=TRUE)
#' data <- data.frame(x = c(0, 0:100,100), y = c(0,sin(seq(0,2*pi,pi/50))*10+20, 0))
#' ggplot(data, aes(x, y)) +  ggR(rlogo, geom_raster= FALSE, ggLayer = TRUE) +
#'        geom_polygon(aes(x, y), fill = "blue", alpha = 0.4) +
#'        coord_equal(ylim=c(0,75))
#' 
#' ## Categorical data 
#' ## In this case you probably want to use geom_raster=TRUE 
#' ## in order to perform aestetic mapping (i.e. a meaningful legend)
#' rc   <- rlogo
#' rc[] <- cut(rlogo[[1]][], seq(0,300, 50))
#' ggR(rc, geom_raster = TRUE)
#' 
#' ## Legend cusomization etc. ...
#' ggR(rc, geom_raster = TRUE) + scale_fill_continuous(labels=paste("Class", 1:6))
#' }
#' ## Creating a nicely looking DEM with hillshade background
#' terr <- terrain(srtm, c("slope", "aspect"))
#' hill <- shade(terr[["slope"]], terr[["aspect"]])
#' ggR(hill)
#' 
#' ggR(hill) + 
#'    ggR(srtm, geom_raster = TRUE, ggLayer = TRUE, alpha = 0.3) +
#'    scale_fill_gradientn(colours = terrain.colors(100), name = "elevation")
ggR <- function(img, layer = 1, maxpixels = 500000,  alpha = 1, hue = 1, sat = 0, stretch = "none", quantiles = c(0.02,0.98), ext = NULL,
                coord_equal = TRUE, ggLayer=FALSE, ggObj = TRUE, geom_raster = FALSE, forceCat = FALSE) {

  img <- .toTerra(img)

  layer <- unlist(.numBand(img, layer))
  layer <- unlist(.numBand(img, layer))

  multLayers <- if (length(layer)>1) TRUE else FALSE

  if(multLayers & !geom_raster & ggObj) {
    warning("You asked for multiple layers but geom_raster is FALSE.",
            "\ngeom_raster will be reset to TRUE", 
            "\nHint: in case you're looking for a grayscale and facetted plot, use:",
            "\nggR(img, ..., geom_raster=TRUE)+scale_fill_gradientn(colors = grey.colors(100))",
            call. = FALSE)
    geom_raster <- TRUE
  }
  if(multLayers & !geom_raster & ggObj) {
    warning("You asked for multiple layers but geom_raster is FALSE.",
            "\ngeom_raster will be reset to TRUE",
            "\nHint: in case you're looking for a grayscale and facetted plot, use:",
            "\nggR(img, ..., geom_raster=TRUE)+scale_fill_gradientn(colors = grey.colors(100))",
            call. = FALSE)
    geom_raster <- TRUE
  }
  annotation <- !geom_raster

  ex <- ext(img)

  xfort <- spatSample(img[[layer]], maxpixels, ext = ex, method = "regular", as.raster = TRUE)
  ex <- as.vector(ext(xfort))

  dimImg <- dim(xfort)

  df <- lapply(names(xfort), function(layer) {
    df    <- data.frame(extract(xfort, seq_along(values(xfort)), xy = TRUE),
                        layerName = factor(layer, levels = names(xfort)))
    colnames(df) <- c("x", "y", "value", "layerName")
    df
  })
  df <- do.call(rbind, df)

  if(forceCat & !is.factor(df$value)) df$value <- as.factor(df$value)

  if(is.character(df$value)) df$value <- factor(df$value)
  fac <- is.factor(df$value)

  if(fac & (annotation | !ggObj)) {
    .vMessage("img values are factors but annotation is TRUE. Converting factors as.numeric.")
    levelLUT   <- levels(df[,"value"])
    df[,"value"] <- as.numeric(df[,"value"])
  }
  if(!fac & stretch != "none")  {
    for(layer in levels(df$layerName)) {
      df[df$layerName==layer,"value"] <- .stretch(df[df$layerName==layer,"value"], method = stretch, quantiles = quantiles)
    }
  }
  
  if(!(ggObj & !annotation)  ){
    ## Annotation processing
    ## Avoid rescaling "value"s with single values
    df <- lapply(levels(df$layerName), function(layer) {
      normVals    <- suppressWarnings(rescaleImage(df[df$layerName==layer,"value"], ymin = 0, ymax = 1))  ## suppress warnings due to single value bands. rescaleImage returns NA, which is fine.
      uval        <- unique(df[,"value"])
      if(sum(is.finite(uval)) == 1)   normVals[is.finite(df[,"value"])] <- 1
      nona         <- !is.na(normVals)
      df$fill      <- NA
      df[nona, "fill"] <- hsv(h = hue, s = sat, v = normVals[nona], alpha = alpha)
      df
    })
    df <- do.call(rbind, df)
  }
  
  x <- y <- value <- NULL  
  if(ggObj) {       
    if(annotation)  {
      dmat <- matrix(df$fill, nrow=dimImg[1], ncol=dimImg[2], byrow = TRUE)
      ggl  <- annotation_raster(raster = dmat, xmin = ex[1], xmax = ex[2], ymin = ex[3], ymax = ex[4], interpolate = FALSE)
    } else {
      ggl  <- geom_raster(data = df[,c("x","y","value","layerName")], aes(x = x, y = y, fill = value), alpha = alpha)
    }
    if(multLayers) facetObj <- facet_wrap(~layerName)

    if(ggLayer) {
      if(multLayers) {
        return(list(ggl, facetObj))
      } else { 
        return(ggl) 
      }
    }
    
    if(annotation) {   
      dummy <- data.frame(x = ex[1:2],y = ex[3:4], layerName = rep(levels(df$layerName), each = 2) )
      p <- ggplot()  + ggl + geom_blank(data = dummy, aes(x,y))
      if(coord_equal) p <- p + coord_equal()
      if(multLayers) p <- p + facet_wrap(~layerName)
      return(p)
    } else {
      p <- ggplot() + ggl
      if(coord_equal) p <- p + coord_equal()
      if(multLayers) p <- p + facetObj
      return(p)
    }
    
  } else {
    if(fac & (annotation | !ggObj)) df[,"value"] <- factor(levelLUT[df[, "value"]], levels=levelLUT)
    return(df)
  }
  
}
