#' Plot rasters in ggplot with greyscale
#' @param x raster
#' @param layer layername
#' @param maxpixels Integer. Maximal number of pixels to sample
#' @param lowColor Character. Color for lowest value
#' @param highColor Character. Color for highest value
#' @param legendName Character. Layer name
#' @param ggObj Logical. Return a ggplot2 object (TRUE) or just the data.frame
#' 
#'  @export 
ggR <- function(x, layer = 1, maxpixels = 5000000, lowColor = "white", highColor = "black", legendName = "Legend", ggObj = TRUE) {  
    drast <- sampleRegular(x[[layer]], maxpixels, asRaster = TRUE)
    df <- data.frame(coordinates(drast), drast[])
    colnames(df) <- c("x", "y", names(x[[layer]]))
    layer <- colnames(df)[3]
    
    if(ggObj) {p <- ggplot(df) + geom_raster(aes_string(x = "x", y = "y", fill = layer)) +
                scale_fill_gradient(low = lowColor, high = highColor, na.value = NA, name = legendName) +
                coord_equal() 
                
    } else {
        p <- df
    }
    return(p)
}
