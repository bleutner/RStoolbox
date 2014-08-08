#' Estimate image haze for dark object subtraction procedures
#' 
#' @param x raster object or a previous result from \code{estimateSHV(x , returnTables = TRUE} from which to estimate haze
#' @param band character. Band or bandname from which to estimate SHV (optinal if x contains only one layer)
#' @param darkProp proportion of pixels estimated to be dark
#' @param plot display histograms and haze values
#' @param returnTables return the frequency table per layer. Only takes effect if x is a Raster* object. If x is a result of estimateSHV tables will always be returned.
#' @export 
estimateSHV <- function(x, hazeBand, darkProp = 0.02, plot = FALSE, returnTables = TRUE) {
	
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
		if(missing(hazeBand)){ 
			if(nlayers(x) == 1) {
				hazeBand <- names(x)        
			} else {
				stop("Please specify the band from which you want to estimate the haze dn")
			}	
			if(is.numeric(hazeBand)) hazeBand <- names(x)[hazeBand]
		}
		
	} else {
		
		if(is.numeric(hazeBand)) hazeBand <- names(x$table)[hazeBand]
		preCalcAvail <- hazeBand %in% names(x$table)
		if(!any(preCalcAvail)) 	stop("Cannot estimate SHV because tables are missing for all specified bands", call. = FALSE)
		
		if(any(!preCalcAvail)) {
			warning(paste0("Cannot estimate SHV for >> ", hazeBand[!preCalcAvail], " << because tables are missing."), call. = FALSE)
			hazeBand <- hazeBand[preCalcAvail] 				
		}	
	}
	
	## Decide whether we open multiple devices
	multiple <- if(length(hazeBand) > 1) TRUE else FALSE
	
	## Run estimation for each band separately
	out   <- lapply(hazeBand, function(bi) {
				if(inherits(x, "Raster")) {
					tf <- freq(x[[bi]], useNA = "no") 
				} else {
					if(is.list(x) & "table" %in% names(x)) {
						preCalc <- TRUE
						tf <- x$table[[bi]]
					} else {
						stop("x must be a Raster* object or the result of a previous run of estimateSHV() with argument 'returnTables = TRUE'", call. = FALSE)
					}
				}
				tf <- tf[tf[,1] > 0,]
				tf[,2] <- tf[,2]/sum(tf[,2])
				dtf <- c(diff(tf[,2]),0) / c(diff(tf[,1]),0)
				
				SHV <- tf[which(dtf > darkProp)[1], 1] 
				if(is.na(SHV)) warning(paste("darkProp for band", bi, "was chosen too high. It exceeds the value range."), call. = FALSE)
				
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
	names(SHV) <- hazeBand
	
	if(!preCalc){
		table <- sapply(out, "[", 1)
		names(table) <- hazeBand
	} else {
		table <- x$table
	}
	return( if(!returnTables) SHV else list(SHV=SHV, table = table))
}