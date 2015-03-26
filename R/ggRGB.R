#' Create ggplot raster with RGB from 3 rasterLayers
#' 
#' Calculates RGB color composite raster for plotting with ggplot2. Optional value clipping and and stretching can be used to enhance the imagery.
#' 
#' Functionality is based on \code{\link[raster]{plotRGB}} from the raster package.
#' 
#' @param img RasterStack or RasterBrick
#' @param r Integer or character. Red layer in x. Can be set to \code{NULL}, in which case the red channel will be set to zero.
#' @param g Integer or character. Green layer in x. Can be set to \code{NULL}, in which case the green channel will be set to zero.
#' @param b Integer or character. Blue layer in x. Can be set to \code{NULL}, in which case the blue channel will be set to zero.
#' @param scale Numeric. Maximum possible pixel value (optional). Defaults to 255 or to the maximum value of x if that is larger than 255
#' @param maxpixels Integer. Maximal number of pixels used for plotting.
#' @param stretch Character. Either 'none', 'lin', 'hist', 'sqrt' or 'log' for no stretch, linear, histogram, square-root or logarithmic stretch.
#' @param ext extent object to crop the image
#' @param limits Vector or matrix. Can be used to reduce the range of values. Either a vector of two values for all bands (c(min, max))
#'  or a 3x2 matrix with separate min and max values (columns) for each layer (rows).
#' @param quantiles Numeric vector with two elements. Min and max quantiles to stretch to. Defaults to 2\% stretch, i.e. c(0.02,0.98). 
#' @param clipToLimits Logical. If \code{TRUE}, values > scale will be set to NA. if \code{FALSE} they will be set to scale. Defaults to \code{FALSE}.
#' @param ggObj Logical. If \code{TRUE} a ggplot2 object is returned. If \code{FALSE} a data.frame with coordinates and color will be returned.
#' @param ggLayer Logical. If \code{TRUE} a ggplot2 layer is returned. This is usefull if you want to add it to an existing ggplot2 object.
#'  Note that if \code{TRUE} & \code{annotate = FALSE} you have to add a scale_fill_identity() manually in your call to ggplot().
#' @param alpha Numeric. Transparency (0-1).
#' @param coordEqual Logical. Force addition of coord_equal, i.e. aspect ratio of 1:1. Typically usefull for remote sensing data (depending on your projection), hence it defaults to TRUE.
#'         Note howver, that this does not apply if (\code{ggLayer=FALSE}).
#' @param annotation Logical. If \code{TRUE} annotation_raster is used, otherwise geom_raster()+scale_fill_identity is used.
#'  Note that you can't use scale_fill* in addition to the latter, because it already requires scale_fill_identity().
#' @return A ggplot2 object, or a three column data frame with coordinates and fill colour.
#' @export
#' @seealso \link{ggR}, \link[=fortify.raster]{fortify}
#' @examples   
#' library(ggplot2)
#' data(rlogo)
#' 
#' ggRGB(rlogo, r=1, g=2, b=3)
#' 
#' ## Define minMax ranges
#' ggRGB(rlogo, r=1,g=2, b=3, limits = matrix(c(100,150,10,200,50,255),  ncol = 2, by = TRUE))
#' 
#' ## Perform stong linear contrast stretch
#' ggRGB(rlogo, r = 1, g = 2, b = 3,stretch = "lin", quantiles = c(0.2, 0.8))
#' 
#' ## Use only two layers for color calculation
#' ggRGB(rlogo, r = 1, g = 2, b = NULL)
#' 
#' ## Return only data.frame
#' df <- ggRGB(rlogo, ggObj = FALSE)
#' head(df)
#' 
#' ## Use in layer-mode, e.g. to add to another plot
#' wave <- data.frame(x = c(0, 0:100,100), y = c(0,sin(seq(0,2*pi,pi/50))*10+20, 0))
#' p <- ggplot(wave, aes(x, y)) 
#' p + ggRGB(rlogo, ggLayer = TRUE) + 
#'        geom_polygon(aes(x, y), fill = "blue", alpha = 0.4) +
#'        coord_equal(ylim=c(0,75))
ggRGB <- function(img, r = 3, g = 2, b = 1, scale, maxpixels = 500000, stretch = "lin", ext = NULL,  limits = NULL, clipToLimits  = FALSE, quantiles = c(0.02,0.98),
        ggObj = TRUE, ggLayer = FALSE, alpha = 1, coordEqual = TRUE, annotation = TRUE) { 
    # RGB processing riginally forked from raster:::plotRGB
    # Author: Robert J. Hijmans 
    # Date :  April 2010 
    # Version 0.9
    # Licence GPL v3
    # partly based on functions in the pixmap package by Friedrich Leisch
    
    ## Subsample raster		
    rgb <- unlist(.numBand(raster=img,r,g,b))
    if(inherits(img, "RasterLayer")) img <- brick(img)
    rr 	<- sampleRegular(img[[rgb]], maxpixels, ext=ext, asRaster=TRUE, useGDAL=TRUE)
    RGB <- getValues(rr)
    if(!is.matrix(RGB)) RGB <- as.matrix(RGB)
    
   
    ## 
    if (!is.null(limits)) {		
        if (length(limits) == 2) {
			limits <- sort(limits)
			if (!clipToLimits) {
                RGB[ RGB < limits[1] ] <- limits[1]
                RGB[ RGB > limits[2] ] <- limits[2]
            } else { 
                RGB[RGB < limits[1] | RGB > limits[2]] <- NA
            } 
        } else if (NROW(limits) == 3 & NCOL(limits) == 2) {
            for (i in 1:3) {
                zmin <- min(limits[i,])		
                zmax <- max(limits[i,])
                if(zmin < min(RGB[,i], na.rm = T) | zmax > max(RGB[,i], na.rm = T)) warning("The provided minMax values of ", c("red","green","blue")[i]," layer exceed range of actual values in this band.")
                if (!clipToLimits) {
                    RGB[RGB[,i] < zmin, i] <- zmin
                    RGB[RGB[,i] > zmax, i] <- zmax
                } else { 
                    RGB[RGB[,i] < zmin | RGB[,i] > zmax, i] <- NA
                }
            }
        } else {
            stop('zlim should be a vector of two numbers or a 3x2 matrix (one row for each color)')
        }
    }   
	
	rangeRGB <- range(RGB, na.rm = TRUE)
	if(missing(scale)){ scale <- rangeRGB[2] }
	
	if(rangeRGB[1] < 0){
		RGB 	<- RGB - rangeRGB[1]
		scale 	<- scale - rangeRGB[1] 
		rangeRGB <- rangeRGB - rangeRGB[1]
	}   
	
	if(scale < rangeRGB[2]) scale <- rangeRGB[2]
    RGB <- na.omit(RGB)
    
    
	## Perform data stretch
    if (stretch != "none") {
        stretch = tolower(stretch)
        for(i in seq_along(rgb)){
            RGB[,i] <- .stretch(RGB[,i], method = stretch, quantiles=quantiles)
        }
        scale <- 255		
    }
    
    ## Assemble colors
    naind <- as.vector( attr(RGB, "na.action") ) 
    nullbands <- sapply(list(r,g,b), is.null)       
    
    if(any(nullbands)) {
        RGBm <- matrix(0, ncol = 3, nrow = NROW(RGB))
        RGBm[,!nullbands] <- RGB
        RGB <- RGBm      
    }
    
    
    if (!is.null(naind)) {
        z <- rep( NA, times=ncell(rr))
        z[-naind] <- rgb(RGB[,1], RGB[,2], RGB[,3],  max = scale, alpha = alpha*scale)
    } else {
        z <- rgb(RGB[,1], RGB[,2], RGB[,3], max = scale, alpha = alpha*scale)
    }
    df_raster <- data.frame(coordinates(rr), fill = z, stringsAsFactors = FALSE)
    
    x <- y <- fill <- NULL ## workaround for a R CMD check 'note' about non-visible global variable in call to ggplot (variables are column names created earlier within 'data' and hence not visible to check). This does not in any way affect ggRGB,
    if(ggObj){ 
        
        ## We need to set up ggplot with at least the minimum aestetics x and y
        exe <- as.vector(extent(rr))
        df <- data.frame(x=exe[1:2],y=exe[3:4])
        
        ## Set-up plot       
        ## I prefer annotate_raster instead of geom_raster or tile to keep the fill scale free for additional rasters        
        if(annotation) {           
            dz <- matrix(z, nrow=nrow(rr), ncol=ncol(rr), byrow = TRUE)  
            p <- annotation_raster(raster = dz, xmin = exe[1], xmax = exe[2], ymin = exe[3], ymax = exe[4], interpolate = FALSE)
            if(!ggLayer) {
                p <- ggplot(df, aes(x = x,y = y)) + p
            }
        } else {
            p <- geom_raster(data = df_raster, aes(x = x, y = y, fill = fill), alpha = alpha)  
            if(!ggLayer) {
                p <- ggplot() + p + scale_fill_identity() 
            }
        }   
        
        if(coordEqual & !ggLayer) p <- p + coord_equal()
        
        return(p)
        
    } else {
        return(df_raster)
    }
    
    
}


## Perform histogram, log and 98% linear stretching
.stretch <- function (x, method = "lin", quantiles = c(0.02,0.98)) {
    if(!method %in% c("lin", "hist", "log", "sqrt")) stop("Stretch method must be 'lin', 'hist', 'sqrt' or 'log'", call. = FALSE)
    if(method == "lin"){
		if(length(quantiles) == 1) quantiles <- c(0,1) + c(quantiles, -quantiles)/100
        v <- quantile(x, quantiles, na.rm = TRUE)
        temp <- (255 * (x - v[1]))/(v[2] - v[1])
        temp[temp < 0] <- 0
        temp[temp > 255] <- 255 
        return(temp)
    } 
    if(method == "hist"){
        ecdfun <- ecdf(x)
        return(ecdfun(x) * 255)
    } 
    if(method == "log"){
        x <- log(x + 1)
        x <- x - min(x)
        return((x / max(x)) * 255)         
    }
    if(method == "sqrt"){
        x <- sqrt(x)
        x <- x - min(x)
        return((x /max(x)) * 255)
    }
}


