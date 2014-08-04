#' Estimate image haze for dark object subtraction procedures
#' 
#' @param x raster object from which to estimate haze
#' @param band character. Band or bandname from which to estimate SHV (optinal if x contains only one layer)
#' @param darkProp proportion of pixels estimated to be dark
#' @param plot display histograms and haze values
#' @param returnTable return the table per layer
#' @export 
estimateSHV <- function(x, band, darkProp = 0.001, plot = FALSE, returnTable = FALSE) {
    
    if(missing(band)){
        if(nlayers(x) == 1) {
            band <- names(x)        
        } else {
            stop("Please specify the band from which you want to estimate the haze dn")
        }
    }
    if(is.numeric(band)) band <- names(x)[band]
    
    
    multiple <- if(length(band) > 1) TRUE else FALSE
    
    out   <- lapply(band, function(bi) {
                tf <- freq(x[[bi]]) 
                tf <- tf[tf[,1] > 0,]
                tf[,2] <- tf[,2]/sum(tf[,2])
                dtf <- c(diff(tf[,2]),0) / c(diff(tf[,1]),0)
                
                SHV <- tf[which(dtf > darkProp)[1], 1] 
                
                if(plot){
                    if(multiple) x11()
                    par(mfrow = c(1,2))
                    
                    plot(tf, xlab = "DN", ylab = "Frequency", type = "l", main = bi)
                    abline(v = tf[tf[,1]==SHV,1], col="red")
                    text(SHV, max(tf[,2]), pos=4, label = paste0("SHV_DN = ", SHV), col ="red")
                    
                    plot(dtf, type="l", xlab = "DN", ylab = "diff(Frequency)", main = bi)
                    abline(v = tf[tf[,1]==SHV,1], col="red")
                    abline(h = darkProp, col = "#00000070", lty = 2)
                    text(max(tf[,1]), darkProp, label = paste0("darkProp = ", darkProp), col = "#00000070")
                    text(SHV, max(dtf, na.rm = TRUE), pos=4, label = paste0("SHV_DN = ", SHV), col ="red")
                    
                }
                
                return(list(table = tf, SHV = SHV))
            })
    
    SHV <- unlist(sapply(out, "[", 2))
    names(SHV) <- band
    table <- sapply(out, "[", 1)
    names(table) <- band
    
    return( if(!returnTable) SHV else list(SHV=SHV, table = table))
}