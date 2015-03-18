#' Estimate image haze for dark object subtraction procedures
#' 
#' @param x raster object or a previous result from \code{estimateSHV(x , returnTables = TRUE} from which to estimate haze
#' @param hazeBands character. Band or bandname from which to estimate SHV (optional if x contains only one layer)
#' @param darkProp proportion of pixels estimated to be dark
#' @param plot display histograms and haze values
#' @param returnTables return the frequency table per layer. Only takes effect if x is a Raster* object. If x is a result of estimateSHV tables will always be returned.
#' @export 
estimateHaze <- function(x, hazeBands, darkProp = 0.02, plot = FALSE, returnTables = FALSE) {
    
    ## Initial or repeated run?
    if(inherits(x, "Raster")) {
        preCalc <- FALSE
    } else {
        if(is.list(x) & "table" %in% names(x)) {
            preCalc <- TRUE 
        } else {
            stop("x must be a Raster* object or the result of a previous run of estimateSHV(Raster*, ) with argument 'returnTables = TRUE'", call. = FALSE)
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
