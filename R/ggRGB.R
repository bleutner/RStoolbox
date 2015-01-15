#' Create ggplot raster with RGB from 3 rasterLayers
#' 
#' Calculates RGB color composite raster for plotting with ggplot2. Optional value clipping and and stretching can be used to enhance the imagery.
#' 
#' Functionality is based on \code{\link[raster]{plotRGB}} from the raster package.
#' 
#' @param x RasterStack or RasterBrick
#' @param r Integer or character. Red layer in x. Can be set to \code{NULL}, in which case the red channel will be set to zero.
#' @param g Integer or character. Green layer in x. Can be set to \code{NULL}, in which case the green channel will be set to zero.
#' @param b Integer or character. Blue layer in x. Can be set to \code{NULL}, in which case the blue channel will be set to zero.
#' @param scale Numeric. Maximum possible pixel value (optional). Defaults to 255 or to the maximum value of x if that is larger than 255
#' @param maxpixels Integer. Maximal number of pixels used for plotting.
#' @param stretch Character. Either 'lin' or 'hist' for linear or histogram stretch of the data
#' @param ext extent object tp crop the image
#' @param minMax Vector or matrix. Can be used to reduce the range of values. Either a vector of two values for all bands (c(min, max)) or 
#' a 3x2 matrix with separate min and max values (columns) for each layer (rows).  
#' @param clipToMinMax Logical. If \code{TRUE}, values > scale will be set to NA. if \code{FALSE} they will be set to scale. Defaults to \code{FALSE}.
#' @param ggObj Logical. If \code{TRUE} a ggplot2 object is returned. If \code{FALSE} a data.frame with coordinates and color will be returned.
#' @param ggLayer Logical. If \code{TRUE} a ggplot2 layer is returned. This is usefull if you want to add it to an existing ggplot2 object. Note that if \code{TRUE} & \code{annotate = FALSE} you have to add a scale_fill_identity() manually in your call to ggplot().
#' @param coordEqual Logical. Uses coord_equal, i.e. aspect ratio of 1:1.
#' @param interpolate Logical. Interpolate the raster during plotting. 
#' @param annotation Logical. If \code{TRUE} annotation_raster is used, otherwise geom_raster()+scale_fill_identity is used. Note that you can't use scale_fill* in addition to the latter, because it alread requires scale_fill_identity().
#' @return A ggplot2 object, or a three column data frame with coordinates and fill colour.
#' @export
#' @examples  
#' br <- brick(system.file("external/rlogo.grd", package="raster"))
#' ggRGB(br, 1, 2, 3)
#' ggRGB(br, r=1,g=2,b=3, minMax = matrix(c(100,150,10,200,50,255),  ncol = 2, by = TRUE))
ggRGB <- function(x, r = 3, g = 2, b = 1, scale, maxpixels = 500000, stretch = NULL, ext = NULL,  minMax = NULL, clipToMinMax = FALSE, ggObj = TRUE, ggLayer = FALSE, coordEqual = TRUE, interpolate = TRUE, annotation = TRUE) { 
    # Originally forked from raster:::plotRGB
    # Author: Robert J. Hijmans
    # Date :  April 2010
    # Version 0.9
    # Licence GPL v3
    # partly based on functions in the pixmap package by Friedrich Leisch
    
    ## Subsample raster
    rgb <- c(r,g,b)
    rr 	<- sampleRegular(x[[rgb]], maxpixels, ext=ext, asRaster=TRUE, useGDAL=TRUE)
    RGB <- getValues(rr)
    if(!is.matrix(RGB)) RGB <- as.matrix(RGB)
    
    rangeRGB <- range(RGB, na.rm = TRUE)
    if(missing(scale)){ scale <- max(rangeRGB, 255) }
    if(rangeRGB[1] < 0){
        RGB 	<- RGB - rangeRGB[1]
        scale 	<- scale - rangeRGB[1] 
        if(!missing(minMax)) minMax <- minMax - rangeRGB[1] 
    }   
    
    ## 
    if (!is.null(minMax)) {
        if (length(minMax) == 2) {
            minMax <- sort(minMax)
            if (!clipToMinMax) {
                RGB[ RGB < minMax[1] ] <- minMax[1]
                RGB[ RGB > minMax[2] ] <- minMax[2]
            } else { 
                RGB[RGB < minMax[1] | RGB > minMax[2]] <- NA
            } 
        } else if (NROW(minMax) == 3 & NCOL(minMax) == 2) {
            for (i in 1:3) {
                
                if(zmin < min(RGB[,i], na.rm = T) | zmax > max(RGB[,i], na.rm = T)) warning("The provided minMax values of ", c("red","green","blue")[i]," layer exceed range of actual values in this band.")
                
                zmin <- min(minMax[i,])		
                zmax <- max(minMax[i,])
                if (!clipToMinMax) {
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
    RGB <- na.omit(RGB)
    
    ## Perform data stretch
    if (!is.null(stretch)) {
        stretch = tolower(stretch)
        for(i in seq_along(rgb)){
            RGB[,i] <- .stretch(RGB[,i], method = stretch)
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
        z[-naind] <- rgb(RGB[,1], RGB[,2], RGB[,3],  max = scale)
    } else {
        z <- rgb(RGB[,1], RGB[,2], RGB[,3], max = scale)
    }
    df_raster <- data.frame(coordinates(rr), fill = z)
    if(ggObj){ 
        
        ## We need to set up ggplot with at least the minimum aestetics x and y
        exe <- as.vector(extent(rr))
        df <- data.frame(x=exe[1:2],y=exe[3:4])
        
        ## Set-up plot       
        ## I prefer annotate_raster instead of geom_raster or tile to keep the fill scale free for additional rasters        
        if(annotation) {           
            dz <- matrix(z, nrow=nrow(rr), ncol=ncol(rr), byrow=TRUE)  
            p <- annotation_raster(raster = dz, xmin=exe[1], xmax=exe[2], ymin=exe[3], ymax=exe[4], interpolate = interpolate)
            if(!ggLayer) {
                p <- ggplot(df, aes(x=x,y=y)) + p
            }
        } else {
            p <- geom_raster(data = df_raster, aes(x = x, y = y, fill = fill))  
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


## Perform histogram and 98% linear stretching
.stretch <- function (x, method = "lin") {
    if(method == "lin"){
        v <- quantile(x, c(0.02, 0.98), na.rm = TRUE)
        temp <- (255 * (x - v[1]))/(v[2] - v[1])
        temp[temp < 0] <- 0
        temp[temp > 255] <- 255 
        return(temp)
    } else {
        if(method == "hist"){
            ecdfun <- ecdf(x)
            ecdfun(x) * 255
        } else {
            stop("Stretch method must be 'lin' or 'hist'", call. = FALSE)
        }
    }
}

