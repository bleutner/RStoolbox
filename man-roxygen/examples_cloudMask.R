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
