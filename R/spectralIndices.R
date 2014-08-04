#' Spectral indices
#' 
#' @param inputRaster Raster* object. Typically remote sensing imagery, which is to be classified.
#' @param spectral index to apply
#' @param filename path to output file (optional). If \code{NULL}, standard raster handling will apply, i.e. storage either in memory or in the raster temp directory.
#' @param maskRaster Raster layer containing a binary mask to exclude areas from prediction.
#' @param verbose logical. prints progress, statistics and graphics during execution
#' @return the raster holding the index results  
#' @seealso \code{\link{...}} 
#' @export
#' @example
#' 
spectralIndices <- function(inputRaster, indices = "NDVI", sensor, bands = list(), filename = NULL, maskRaster = NULL, verbose = FALSE, overwrite = TRUE, ...) {
	# TODO: add indices
	# TODO: add examples
	# TODO: add formulas to help file
	# TODO: internal sensor db
	# TODO: value checks?
	bands <- lapply(bands, function(x) if(is.character(x)) which(names(inputRaster) == x) else x )
	
	## Internal db
	INDICES <-  list(
			SR 		= function(NIR, RED) {NIR / RED},
			DVI		= function(NIR, RED) NIR-RED,
			NDVI	= function(NIR, RED) (NIR-RED)/(NIR+RED), 
			TVI 	= function(NIR, RED) (((NIR-RED)/(NIR+RED))+0.5)^0.5, 
			MSAVI	= function(NIR, RED) NIR + 0.5 - (0.5 * sqrt((2 * NIR + 1)^2 - 8 * (NIR - (2 * RED)))),
			MSAVI2	= function(NIR, RED) (2 * (NIR + 1) - sqrt((2 * NIR + 1)^2 - 8 * (NIR - RED))) / 2,
			GEMI	= function(NIR, RED) (((NIR^2 - RED^2) * 2 + (NIR * 1.5) + (RED * 0.5) ) / (NIR + RED + 0.5)) * (1 - ((((NIR^2 - RED^2) * 2 + (NIR * 1.5) + (RED * 0.5) ) / (NIR + RED + 0.5)) * 0.25)) - ((RED - 0.125) / (1 - RED)),                   
			SLAVI	= function(RED, MIR) NIR / (RED + MIR),
			EVI		= function(NIR, RED, BLUE) G * ((NIR - RED) / (NIR + C1 * RED - C2 * BLUE + L))# include a G or L specification in command
	)
	
	## Get arguments and check for mising arguments
	args <- lapply(indices, function(index) {
				need <- names(formals(INDICES[[index]]))	
				if(any(!names(bands) %in% need)) stop("Band specification(s) of >> ", paste(names(bands)[!names(bands) %in% need], collapse = ","), 
							" << are missing or do not match layer names in the brick/stack. \nPlease specify the correct layer number or name in a list, e.g. bands = list(RED = 'B4', NIR = 'B5')", call. = FALSE)
				need <- unlist(bands[need])
			})
	names(args) <- indices
	
	## We do this in a separate step, so we can throw an error before if there is one
	inlist <- lapply(indices, function(index) {
				overlay(inputRaster[[args[[index]]]], fun = INDICES[[index]])
			})
	
	## Combine and return
	outlist <- stack(inlist)
	names(outlist) <- indices	
	return(outlist)
}