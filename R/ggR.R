#' Plot RasterLayers in ggplot with greyscale
#' 
#' Plot single layer imagery in grey-scale. Can be used with any Raster* object.
#' 
#' @param img raster
#' @param layer Character or numeric. Layername or number.
#' @param maxpixels Integer. Maximal number of pixels to sample.
#' @param alpha Numeric. Transparency (0-1).
#' @param hue Numeric. Hue value for color calculation [0,1] (see \[grDevices]{hsv}). Change if you need anything else than greyscale. Only effective if \code{sat > 0}.
#' @param sat Numeric. Saturation value for color calculation [0,1] (see \[grDevices]{hsv}). Change if you need anything else than greyscale.
#' @param stretch Character. Either 'none', 'lin', 'hist', 'sqrt' or 'log' for no stretch, linear, histogram, square-root or logarithmic stretch.
#' @param quantiles Numeric vector with two elements. Min and max quantiles to stretch to. Defaults to 2\% stretch, i.e. c(0.02,0.98). 
#' @param ggObj Logical. Return a stand-alone ggplot object (TRUE) or just the data.frame with values and colors
#' @param ggLayer Logical. Return only a ggplot layer which must be added to an existing ggplot. If \code{FALSE} s stand-alone ggplot will be returned.
#' @param geom_raster Logical. If \code{FALSE} uses annotation_raster (good to keep aestetic mappings free). If \code{TRUE} uses \code{geom_raster} (and \code{aes(fill)}). See Details.
#' @param coord_equal Logical. Force addition of coord_equal, i.e. aspect ratio of 1:1. Typically usefull for remote sensing data (depending on your projection), hence it defaults to TRUE.
#'         Note however, that this does not apply if (\code{ggLayer=FALSE}).
#' @param forceCat Logical. If \code{TRUE} the raster values will be forced to be categorical (will be converted to factor if needed). 
#' @seealso \link{ggRGB}, \link[=fortify.raster]{fortify}
#' @return 
#' \tabular{ll}{
#'  \code{ggObj = TRUE}:   \tab ggplot2 plot \cr
#'  \code{ggLayer = TRUE}: \tab ggplot2 layer to be combined with an existing ggplot2 \cr
#'  \code{ggObj = FALSE}:  \tab data.frame in long format suitable for plotting with ggplot2, includes the pixel values and the calculated colors  \cr  
#' }
#' @details
#' When \code{img} contains factor values and \code{annotation=TRUE}, the raster values will automatically be converted
#' to numeric in order to proceed with the brightness calculation. 
#' 
#' The raster package provides a class lookup-table for categorical rasters (e.g. what you get if you run superClass in classification mode). If your raster has a lookup-table ggR will automatically treat it as categorical (see \link[raster]{factor}). 
#' However, the factor status of Raster objects is easily lost and the values are interpreted as numeric. In such cases you should make use of the \code{forceCat = TRUE} argument, which makes sure
#' that ggplot2 uses a discrete scale, not a continuous one.
#' 
#' The geom_raster argument switches from the default use of annotation_raster to geom_raster. The difference between the two is that geom_raster performs
#' a meaningful mapping from pixel values to fill colour, while annotation_raster is simply adding a picture to your plot. In practice this means that whenever you 
#' need a legend for your raster you should use \code{geom_raster = TRUE}. This also allows you to specify and modify the fill scale manually. 
#' The advantage of using annotation_raster (\code{geom_raster = TRUE}) is that you can still use the scale_fill* for another variable. For example you could add polygons and 
#' map a value to their fill colour. For more details on the theory behind aestetic mapping have a look at the \href{https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf}{ggplot2} manuals.
#' 
#' @export 
#' @examples
#' library(ggplot2)
#' library(raster)
#' data(rlogo)
#' 
#' ## Simple grey scale annotation
#' ggR(rlogo)
#' 
#' ## With linear stretch contrast enhancement
#' ggR(rlogo, stretch = "lin", quantiles = c(0.1,0.9))
#' 
#' ## Don't plot, just return a data.frame
#' df <- ggR(rlogo, ggObj = FALSE)
#' head(df)
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
#' ## Layermode (ggLayer=TRUE)
#' data <- data.frame(x = c(0, 0:100,100), y = c(0,sin(seq(0,2*pi,pi/50))*10+20, 0))
#' ggplot(data, aes(x, y)) +  ggR(rlogo, geom_raster= FALSE, ggLayer = TRUE) +
#'        geom_polygon(aes(x, y), fill = "blue", alpha = 0.4) +
#'        coord_equal(ylim=c(0,75))
#' 
#' ## Categorical data 
#' ## In this case you probably want to use geom_raster=TRUE 
#' ## in order to perform aestetic mapping (i.e. a meaningful legend)
#' rc <- raster(rlogo)
#' rc[] <- cut(rlogo[[1]][], seq(0,300, 50))
#' ggR(rc, geom_raster = TRUE)
#' 
#' ## Legend cusomization etc. ...
#' ggR(rc, geom_raster = TRUE) + scale_fill_discrete(labels=paste("Class", 1:6))
#'  
#' ## Creating a nicely looking DEM with hillshade background
#' data(srtm)
#' terr <- terrain(srtm, c("slope", "aspect"))
#' hill <- hillShade(terr[["slope"]], terr[["aspect"]])
#' ggR(hill)
#' 
#' ggR(hill) + 
#'    ggR(srtm, geom_raster = TRUE, ggLayer = TRUE, alpha = 0.3) +
#'    scale_fill_gradientn(colours = terrain.colors(100), name = "elevation")
ggR <- function(img, layer = 1, maxpixels = 500000,  alpha = 1, hue = 1, sat = 0, stretch = "none", quantiles = c(0.02,0.98), 
        coord_equal = TRUE, ggLayer=FALSE, ggObj = TRUE, geom_raster = FALSE, forceCat = FALSE) {
    
    annotation <- !geom_raster
    layer <- unlist(.numBand(img, layer))
    xfort <- sampleRegular(img[[layer]], maxpixels, asRaster = TRUE)
#    if(is.factor(img[[layer]])) {
#        rat <- levels(xfort)
#        xfort <- stack(xfort,xfort)  ## workaround raster bug #6043    >> apparently solved now in raster    
#        names(xfort)[1] <- names(img)[layer]   
#    }
    df 	  <- as.data.frame(xfort, xy = TRUE)
    layer <- names(img)[layer]
    colnames(df) <- c("x", "y", layer) 
    if(forceCat & !is.factor(df[,layer])) df[,layer] <- as.factor(df[,layer])
    
    if(is.character(df[,layer])) df[,layer] <- factor(df[,layer])
    fac <- is.factor(df[,layer]) 
    
    if(fac & (annotation | !ggObj)) {
        .vMessage("img values are factors but annotation is TRUE. Converting factors as.numeric.")
        levelLUT   <- levels(df[,layer])
        df[,layer] <- as.numeric(df[,layer])
    }
    if(!fac & stretch != "none")  df[,layer] <- .stretch(df[,layer], method = stretch, quantiles = quantiles)    
    
    if(!(ggObj & !annotation)  ){
        ## Annotation processing
        ## Avoid rescaling layers with single values
        normVals    <- suppressWarnings(rescaleImage(df[,layer], ymin = 0, ymax = 1))  ## suppress warnings due to single value bands. rescaleImage returns NA, which is fine.
        uval        <- unique(df[,layer])
        if(sum(is.finite(uval)) == 1)   normVals[is.finite(df[,layer])] <- 1
        nona 		<- !is.na(normVals)
        df$fill  	<- NA
        df[nona, "fill"] <- hsv(h = hue, s = sat, v = normVals[nona], alpha = alpha)
    }
    
    x <- y <- NULL  
    if(ggObj) {       
        ex    <- extent(xfort)
        if(annotation)  {        
            dmat <- matrix(df$fill, nrow=nrow(xfort), ncol=ncol(xfort), byrow = TRUE)  
            ggl  <- annotation_raster(raster = dmat, xmin = ex[1], xmax = ex[2], ymin = ex[3], ymax = ex[4], interpolate = FALSE)
        } else {
            ggl  <- geom_raster(data = df[,c("x","y",layer)], aes_string(x = "x", y = "y", fill = layer), alpha = alpha) 
        }
        
        if(ggLayer) return(ggl)
        
        if(annotation) {   
            dummy <- data.frame(x=ex[1:2],y=ex[3:4])       
            p <- ggplot(dummy, aes(x,y))  + ggl
            if(coord_equal) p <- p + coord_equal()
            return(p)
        } else {
            p <- ggplot() + ggl
            if(coord_equal) p <- p + coord_equal()
            return(p)
        }
        
    } else {
        if(fac & (annotation | !ggObj)) df[,layer] <- factor(levelLUT[df[, layer]], levels=levelLUT)
        return(df)
    }
    
}

