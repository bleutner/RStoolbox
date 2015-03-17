#' Plot RasterLayers in ggplot with greyscale
#' 
#' aimed towards plotting single layer imagery in grey-scale (remote sensing data) but can be 
#' used with any Raster* object.
#' 
#' @param img raster
#' @param layer layername
#' @param maxpixels Integer. Maximal number of pixels to sample
#' @param stretch Character. Either 'lin' or 'hist' for linear or histogram stretch of the data
#' @param quantiles Numeric vector with two elements. Min and max quantiles to stretch to. Defaults to 2\% stretch, i.e. c(0.02,0.98). 
#' @param ggObj Logical. Return a stand-alone ggplot object (TRUE) or just the data.frame with values and colors
#' @param ggLayer Logical. Return only a ggplot layer which must be added to an existing ggplot. If \code{FALSE} s stand-alone ggplot will be returned.
#' @param annotation Logical. Uses annotation_raster by default (good to keep aestetic mappings free). If \code{FALSE} uses geom_raster (and aes(fill)).
#' @param coordEqual Logical. Force addition of coord_equal, i.e. aspect ratio of 1:1. Typically usefull for remote sensing data (depending on your projection), hence it defaults to TRUE.
#'         Note however, that this does not apply if (\code{ggLayer=FALSE}).
#' @param alpha Numeric. Transparency (0-1).
#' @param forceCat Logical. If \code{TRUE} the raster values will be forced to be categorical (will be converted to factor if needed). 
#' @seealso \link{ggRGB}, \link[=fortify.raster]{fortify}
#' @note
#' When \code{img} contains factor values and \code{annotation=TRUE}, the raster values will automatically be converted
#' to numeric in order to proceed with the color calculation. 
#' 
#' The raster package provides a class lookup-table for categorical rasters (e.g. what you get if you run superClass in classification mode). If your raster has one ggR will automatically treat it as categorical (see \link[raster]{factor}). 
#' However, the factor status of Raster objects is easily lost and the values are interpreted as numeric. In such cases you should make use of the \code{forceCat = TRUE} argument, which makes sure
#' that ggplot2 uses a discrete scale, not a continuous one.
#' 
#' 
#' @export 
#' @examples
#' library(ggplot2)
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
#' ggR(rlogo, annotation = FALSE)
#' 
#' ## with different scale
#' ggR(rlogo, annotation=FALSE) + scale_fill_gradientn(name = "mojo", colours = rainbow(10)) +
#'         ggtitle("**Funkadelic**")
#' 
#' ## Layermode (ggLayer=TRUE)
#' data <- data.frame(x = c(0, 0:100,100), y = c(0,sin(seq(0,2*pi,pi/50))*10+20, 0))
#' ggplot(data, aes(x, y)) +  ggR(rlogo, annotation= TRUE, ggLayer = TRUE) +
#'        geom_polygon(aes(x, y), fill = "blue", alpha = 0.4) +
#'        coord_equal(ylim=c(0,75))
#' 
#' ## Categorical data 
#' ## In this case you probably want to use annotation=FALSE 
#' ## in order to perform aestetic mapping (i.e. a meaningful legend)
#' rc <- raster(rlogo)
#' rc[] <- cut(rlogo[[1]][], seq(0,300, 50))
#' ggR(rc, annotation = FALSE)
#' 
#' ## Legend cusomization etc. ...
#' ggR(rc, annotation = FALSE) + scale_fill_discrete(labels=paste("Class", 1:6))
#'  
ggR <- function(img, layer = 1, maxpixels = 500000,  alpha = 1, stretch, quantiles = c(0.02,0.98), coordEqual = TRUE, ggLayer=FALSE, ggObj = TRUE, annotation = TRUE, forceCat = FALSE) {
     
    layer <- unlist(.numBand(img, layer))
    xfort <- sampleRegular(img[[layer]], maxpixels, asRaster = TRUE)
    if(is.factor(img[[layer]])) {
        rat <- levels(xfort)
        xfort <- stack(xfort,xfort)  ## workaround raster bug #6043        
        names(xfort)[1] <- names(img)[layer]   
    }
    df 	  <- as.data.frame(xfort, xy = T)[,-4]
    layer <- names(img)[layer]
    colnames(df) <- c("x", "y", layer) 
    if(forceCat & !is.factor(df[,layer])) df[,layer] <- as.factor(df[,layer])
   
    fac <- is.factor(df[,layer])
    if(fac & (annotation | !ggObj)) {
        .vMessage("img values are factors but annotation is TRUE. Converting factors as.numeric.")
        levelLUT <- levels(df[,layer])
        df[,layer] <- as.numeric(df[,layer])
    }
    if(!fac & !missing("stretch"))  df[,layer] <- .stretch(df[,layer], method = stretch, quantiles = quantiles)    
    
    if(!(ggObj & !annotation)  ){
        normVals 	<- normImage(df[,layer], ymin = 0, ymax = 1)    
        nona 		<- !is.na(normVals)
        df$fill  	<- NA
        df[nona, "fill"] <- hsv(h = 1, s = 0, v = normVals[nona], alpha = alpha)
    }
    x<-y<-NULL
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
            if(coordEqual) p <- p + coord_equal()
            return(p)
        } else {
            p <- ggplot() + ggl
            if(coordEqual) p <- p + coord_equal()
            return(p)
        }
        
    } else {
        if(fac & (annotation | !ggObj)) df[,layer] <- factor(levelLUT[df[, layer]], levels=levelLUT)
        return(df)
    }
    
}
