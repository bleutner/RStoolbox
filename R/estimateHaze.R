#' Estimate Image Haze for Dark Object Subtraction (DOS) 
#' 
#' estimates the digital number (DN) pixel value of *dark* objects for the visible wavelength range.
#' 
#' @param x Raster* object or a previous result from \code{estimateHaze(x , returnTables = TRUE} from which to estimate haze
#' @param hazeBands character. Band or bandname from which to estimate atmospheric haze (optional if x contains only one layer)
#' @param darkProp proportion of pixels estimated to be dark
#' @param plot display histograms and haze values
#' @param returnTables return the frequency table per layer. Only takes effect if x is a Raster* object. If x is a result of estimateHaze tables will always be returned.
#' @details 
#' It is assumed that any radiation originating from *dark* pixels is due to atmospheric haze and 
#' not the reflectance of the surface itself (the surface is dark, i.e. it has a reflectance close to zero).
#' Hence, the haze values are estimates of path radiance, which can be subtracted in a dark object subtraction (DOS) procedure (see \code{\link{radCor}})
#' 
#' Atmospheric haze affects almost exclusively the visible wavelength range. Therefore, typically, you'd only want to estimate haze in blue, green and red bands, occasionally also in the nir band.
#' 
#' @return 
#' If returnTables is FALSE (default). Then a vector of length(hazeBands) containing the estimated haze DNs will be returned.
#' If returnTables is TRUE a list with two components will be returned. The list element 'SHV' contains the haze values, while 'table' 
#' contains another list with the sampled frequency tables. The latter can be re-used to try different darkProp thresholds without having to sample 
#' the raster again.
#' @export 
#' @examples
#' mtlFile  <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")
#' lsat <- stackMeta(mtlFile)
#' 
#' ## Estimate haze for blue, green and red band
#' haze <- estimateHaze(lsat, hazeBands = 1:3, plot = TRUE)
#' haze
#' 
#' ## Find threshold interactively
#' #### Return the frequency tables for re-use 
#' #### avoids having to sample the Raster again and again
#' haze <- estimateHaze(lsat, hazeBands = 1:3, returnTables = TRUE)
#' ## Use frequency table instead of lsat and fiddle with 
#' haze <- estimateHaze(haze, hazeBands = 1:3, darkProp = .1, plot = TRUE)
#' haze$SHV
estimateHaze <- function(x, hazeBands, darkProp = 0.02, plot = FALSE, returnTables = FALSE) {
    
    ## Initial or repeated run?
    if(inherits(x, "Raster")) {
        preCalc <- FALSE
    } else {
        if(is.list(x) & "table" %in% names(x)) {
            preCalc <- TRUE
            returnTables <- TRUE
        } else {
            stop("x must be a Raster* object or the result of a previous run of estimateHaze(Raster*, ) with argument 'returnTables = TRUE'", call. = FALSE)
        }	
    }
    
    if(!preCalc){
        if(missing(hazeBands)){ 
            if(nlayers(x) == 1) {
                hazeBands <- names(x)        
            } else {
                stop("Please specify the band from which you want to estimate the haze dn")
            }	
            if(is.numeric(hazeBands)) hazeBands <- names(x)[hazeBands]
        }
        
    } else {
        
        if(is.numeric(hazeBands)) hazeBands <- names(x$table)[hazeBands]
        preCalcAvail <- hazeBands %in% names(x$table)
        if(!any(preCalcAvail)) 	stop("Cannot estimate SHV because tables are missing for all specified bands", call. = FALSE)
        
        if(any(!preCalcAvail)) {
            warning(paste0("Cannot estimate SHV for >> ", hazeBands[!preCalcAvail], " << because tables are missing."), call. = FALSE)
            hazeBands <- hazeBands[preCalcAvail] 				
        }	
    }
    
    ## Prepare plot device
    if(plot){
        olpar <- par(no.readonly = TRUE)
        on.exit(par(olpar))
        if(length(hazeBands) > 1){
            par(mfrow = c(2, ceiling(length(hazeBands)/2))) 
        }
    }
    
    ## Run estimation for each band separately
    out   <- lapply(hazeBands, function(bi) {
                if(!preCalc) {
                    ## TODO: move freq out of loop
                    tf <- freq(x[[bi]], useNA = "no") 
                } else {
                    tf <- x$table[[bi]]
                }
                tf <- tf[tf[,1] > 0,]
                tf[,2] <- tf[,2]/sum(tf[,2])
                dtf <- c(diff(tf[,2]),0) / c(diff(tf[,1]),0)
                
                SHV <- tf[which(dtf > darkProp)[1], 1] 
                if(is.na(SHV)) warning(paste("darkProp for band", bi, "was chosen too high. It exceeds the value range."), call. = FALSE)
                
                if(plot){
                    plot(tf, xlab = "DN", ylab = "Frequency", type = "l", main = bi)
                    abline(v = tf[tf[,1]==SHV,1], col="red")
                    abline(h = darkProp, col = "grey20", lty = 2)                   
                    text(SHV, max(tf[,2]), pos = 4, label = paste0("SHV_DN = ", SHV), col = "red")            
                    text(max(tf[,1]), darkProp+0.001, label = "darkProp", adj=1, col = "grey20")
                }
                
                return(list(table = tf, SHV = SHV))
            })
    
    SHV <- unlist(lapply(out, "[", 2))
    names(SHV) <- hazeBands
    
    if(!preCalc){
        table <- sapply(out, "[", 1)
        names(table) <- hazeBands
    } else {
        table <- x$table
    }
    return( if(!returnTables) SHV else list(SHV=SHV, table = table))
}
