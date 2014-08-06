## Function taken from the landsat package: S. Goslee (2012)
## Estimates Earth-Sun distance (in AU) for a given date in format "YYYY-MM-DD"
.ESdist <- function(adate){	
	edist <- julian(as.Date(adate), origin=as.Date(paste(substring(adate, 1, 4), "12", "31", sep="-")))[[1]]
	 1 - 0.016729 * cos((2*pi) * (0.9856 * (edist - 4)/360))
}
