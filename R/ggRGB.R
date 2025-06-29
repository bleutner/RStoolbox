#' Create ggplot2 Raster Plots with RGB from 3 RasterLayers
#' 
#' Calculates RGB color composite raster for plotting with ggplot2. Optional values for clipping and and stretching can be used to enhance the imagery.
#' 
#' Functionality is based on \code{\link[terra]{plotRGB}} from the raster package.
#' 
#' @param img SpatRaster
#' @param r Integer or character. Red layer in x. Can be set to \code{NULL}, in which case the red channel will be set to zero.
#' @param g Integer or character. Green layer in x. Can be set to \code{NULL}, in which case the green channel will be set to zero.
#' @param b Integer or character. Blue layer in x. Can be set to \code{NULL}, in which case the blue channel will be set to zero.
#' @param nullValue Numeric. Intensity value used for NULL layers in color compositing. E.g. set g=NULL and fix green value at 0.5 (defaults to 0).
#' @param scale Numeric. Maximum possible pixel value (optional). Defaults to 255 or to the maximum value of x if that is larger than 255
#' @param maxpixels Integer. Maximal number of pixels used for plotting.
#' @param stretch Character. Either 'none', 'lin', 'hist', 'sqrt' or 'log' for no stretch, linear, histogram, square-root or logarithmic stretch.
#' @param ext Extent or SpatExtent object to crop the image
#' @param limits Vector or matrix. Can be used to reduce the range of values. Either a vector of two values for all bands (c(min, max))
#'  or a 3x2 matrix with min and max values (columns) for each layer (rows).
#' @param quantiles Numeric vector with two elements. Min and max quantiles to stretch. Defaults to 2\% stretch, i.e. c(0.02,0.98). 
#' @param clipValues Matrix, numeric vector, string or NA. Values to reset out of range (out of \code{limits}) values to. 
#' By default ('limits') values are reset to \code{limits}. A single value (e.g. NA) will be recycled to all lower/higher clippings,
#' A vector of length two (c(min,max)) can be used to specify lower and higher replace values, applied to all bands. 
#' A two column matrix (typically with three rows) can be used to fully control lower and upper clipping values differently for each band.
#' @param ggObj Logical. If \code{TRUE} a ggplot2 object is returned. If \code{FALSE} a data.frame with coordinates and color will be returned.
#' @param ggLayer Logical. If \code{TRUE} a ggplot2 layer is returned. This is useful if you want to add it to an existing ggplot2 object.
#'  Note that if \code{TRUE} & \code{annotate = FALSE} you have to add a scale_fill_identity() manually in your call to ggplot().
#' @param alpha Numeric. Transparency (0-1).
#' @param coord_equal Logical. Force addition of coord_equal, i.e. aspect ratio of 1:1. Typically useful for remote sensing data (depending on your projection), hence it defaults to TRUE.
#'         Note howver, that this does not apply if (\code{ggLayer=FALSE}).
#' @param geom_raster Logical. If \code{FALSE} annotation_raster is used, otherwise geom_raster()+scale_fill_identity is used.
#'  Note that you can't use scale_fill* in addition to the latter, because it already requires scale_fill_identity().
#' @return 
#' \tabular{ll}{
#'  \code{ggObj = TRUE}:   \tab ggplot2 plot \cr
#'  \code{ggLayer = TRUE}: \tab ggplot2 layer to be combined with an existing ggplot2 \cr
#'  \code{ggObj = FALSE}:  \tab data.frame in long format suitable for plotting with ggplot2, includes the pixel values and the calculated colors \cr  
#' }
#' @export
#' @seealso \link{ggR}, \link[=fortifySpatRaster]{fortifySpatRaster}
#' @examples   
#' library(ggplot2)
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
ggRGB <- function(img, r = 3, g = 2, b = 1, scale, maxpixels = 500000, stretch = "none", ext = NULL,  limits = NULL,
        clipValues  = "limits", quantiles = c(0.02,0.98), ggObj = TRUE, ggLayer = FALSE, 
        alpha = 1, coord_equal = TRUE, geom_raster = FALSE, nullValue = 0) { 
    
    ## TODO: handle single value rasters (e.g. masks)

    img <- .toTerra(img)
    
    # RGB processing originally forked from raster::plotRGB (Author: Robert J. Hijmans) GPL3 
    verbose <- getOption("RStoolbox.verbose")
    annotation <- !geom_raster
    ## Subsample raster        
    rgb <- unlist(.numBand(raster=img,r,g,b))
    nComps <- length(rgb)

    rr <- spatSample(img[[rgb]], maxpixels, ext = ext(img), method = "regular", as.raster = TRUE)
    ex <- as.vector(ext(rr))
    
    RGB    <- as.data.frame(rr, xy = TRUE)
    xy <- RGB[,c("x", "y")]
    RGB <- as.matrix(RGB[,-c(1:2)])
    
    ## Clip to limits
    if (!is.null(limits)) {
        ## Tidy limits
        if (!is.matrix(limits)) {
            limits <- matrix(limits, ncol = 2, nrow = nComps, byrow = TRUE)        
        }         
        ## Tidy clip values
        if(!is.matrix(clipValues)){
            if(!anyNA(clipValues) && clipValues[1] == "limits") {
                clipValues <- limits
            } else {
                clipValues <- matrix(clipValues, ncol = 2, nrow = nComps, byrow = TRUE)                            
            } 
        }
        ## Do clipping
        for (i in 1:nComps) {    
            if(verbose){
                message("Number of pixels clipped in ", c("red", "green", "blue")[i], " band:\n",
                        "below limit: ", sum(RGB[,i] < limits[i,1], na.rm = TRUE), " | above limit: ", sum(RGB[,i] > limits[i,2], na.rm = TRUE))
            }
            RGB[RGB[,i] < limits[i,1], i] <- clipValues[i,1]
            RGB[RGB[,i] > limits[i,2], i] <- clipValues[i,2]            
        }
    }   
    rangeRGB <- range(RGB, na.rm = TRUE)
    
    if(missing('scale')){ scale <- rangeRGB[2] }
    
    if(rangeRGB[1] < 0){
        RGB      <- RGB - rangeRGB[1]
        scale    <- scale - rangeRGB[1] 
        rangeRGB <- rangeRGB - rangeRGB[1]
    }   
    
    if(scale < rangeRGB[2]) {
        warning("Scale < max value. Resetting scale to max.", call.=FALSE)
        scale <- rangeRGB[2]
    }
    RGB <- na.omit(RGB)
    
    
    ## Perform data stretch
    if (stretch != "none") {
        stretch <- tolower(stretch)
        for(i in seq_along(rgb)){
            RGB[,i] <- .stretch(RGB[,i], method = stretch, quantiles=quantiles, band = i)
        }
        scale <- 1        
    }
    
    ## Assemble colors
    naind <- as.vector( attr(RGB, "na.action") ) 
    nullbands <- sapply(list(r,g,b), is.null)       
    
    
    if(any(nullbands)) {
        RGBm <- matrix(nullValue, ncol = 3, nrow = NROW(RGB))
        RGBm[,!nullbands] <- RGB
        RGB <- RGBm      
    }
    
    
    if (!is.null(naind)) {
        z <- rep( NA, times=ncell(rr))
        z[-naind] <- rgb(RGB[,1], RGB[,2], RGB[,3],  max = scale, alpha = alpha*scale)
    } else {
        z <- rgb(RGB[,1], RGB[,2], RGB[,3], max = scale, alpha = alpha*scale)
    }
    df_raster <- data.frame(xy, fill = z, stringsAsFactors = FALSE)
    
    x <- y <- fill <- NULL ## workaround for a R CMD check 'note' about non-visible global variable in call to ggplot (variables are column names created earlier within 'data' and hence not visible to check). This does not in any way affect ggRGB,
    if(ggObj){ 
        
        ## We need to set up ggplot with at least the minimum aestetics x and y
        df <- data.frame(x=ex[1:2],y=ex[3:4])
        
        ## Set-up plot       
        ## I prefer annotate_raster instead of geom_raster or tile to keep the fill scale free for additional rasters        
        if(annotation) {           
            dz <- matrix(z, nrow=nrow(rr), ncol=ncol(rr), byrow = TRUE)  
            p <- annotation_raster(raster = dz, xmin = ex[1], xmax = ex[2], ymin = ex[3], ymax = ex[4], interpolate = FALSE)
            if(!ggLayer) {
                p <- ggplot() + p + geom_blank(data = df, aes(x = x,y = y))
            }
        } else {
            p <- list(geom_raster(data = df_raster, aes(x = x, y = y, fill = fill), alpha = alpha), scale_fill_identity())
            if(!ggLayer) {
                p <- ggplot() + p
            }
        }   
        
        if(coord_equal & !ggLayer) p <- p + coord_equal()
        
        return(p)
        
    } else {
        return(df_raster)
    }
    
    
}


## Perform histogram, sqrt log and 98% linear stretching
.stretch <- function (x, method = "lin", quantiles = c(0.02,0.98), band = NULL) {
    
    if(!method %in% c("lin", "hist", "log", "sqrt")) stop("Stretch method must be 'lin', 'hist', 'sqrt' or 'log'", call. = FALSE)
    if(!length(x)) return(x)
    if(all(is.na(x))) {
        warning("All values are NA. Can't compute color values -> plot will appear empty.", call. = FALSE)
        return(NA)
    } 
    ra <- range(x, na.rm = TRUE)
    if(diff(ra) == 0 & method %in% c("lin", "log", "sqrt")){ 
        if(ra[1] > 1 | ra [1] < 0) {
            warning("Only one unique value in band ", band," (", c("red","green","blue")[band], 
                    "). Stretch not possible -> assigning a value of 1 for rgb color calculation.", call. = FALSE)  
            return( rep( 1, length(x)))
        } else {
            return(x)
        }
    }
    if(method == "lin"){
        if(length(quantiles) == 1) quantiles <- c(0,1) + c(quantiles, -quantiles)/100
        v <- quantile(x, quantiles, na.rm = TRUE)
        if(diff(v)==0) {
            ## sometimes lowr and upr quantile can be identical, which breaks the color calculation --> enforce a minimal distance by adding ~0
            v[2] <- v[2] + 1e-9
        }
        temp <-  (x - v[1])/(v[2] - v[1])
        temp[temp < 0] <- 0
        temp[temp > 1] <- 1 
        return(temp)
    } 
    if(method == "hist"){
        ecdfun <- ecdf(x)
        return(ecdfun(x))
    } 
    if(method == "log"){
        x <- log(x + 1)
        x <- x - min(x)
        return(x / max(x))         
    }
    if(method == "sqrt"){
        x <- sqrt(x)
        x <- x - min(x)
        return(x /max(x))
    }
}


