#' Spectral indices
#' 
#' @param inputRaster Raster* object. Typically remote sensing imagery, which is to be classified.
#' @param indices character. one or more spectral indices 
#' @param sensor if a sensor is specified \code{bands} is populated automatically. Specifying a sensor requires the layernames in inputRaster to match the official band designations formatted as "B1", "B2" etc.
#' @param bands list of band designations. See notes for details
#' @param maskRaster Raster layer containing a binary mask to exclude areas from prediction.
#' @param verbose logical. prints progress, statistics and graphics during execution
#' @param ... further arguments such as filename etc. passed to \link[raster]{writeRaster}
#' @return rasterBrick or rasterStack
#' @seealso \code{\link[raster]{overlay}} 
#' @export
#' @examples
#' r <- raster(ncol=10,nrow=10)
#' r[] <- sample(1:155, 100, TRUE)
#' r <- stack(r, r + 90 + rnorm(100, 10)) 
#' names(r) <- c("red", "nir")
#' SI <- spectralIndices(r, indices = c("SR", "NDVI"), bands = list(NIR = "nir", RED = "red"))
#' plot(SI)
spectralIndices <- function(inputRaster, indices = "NDVI", sensor, bands , maskRaster = NULL, verbose = FALSE, ... ) {
	# TODO: add indices
	# TODO: add examples
	# TODO: add formulas to help file
	# TODO: internal sensor db
	# TODO: value checks?
	# TODO: check sensor list for correctness and extend it 
	
	## Sensor db
	SENSORS <- list(
			LANDSAT5 = list(BLUE = "B1", GREEN = "B2", RED = "B3", NIR = "B4", MIR = "B7"),
			LANDSAT7 = list(BLUE = "B1", GREEN = "B2", RED = "B3", NIR = "B4"),
			LANDSAT8 = list(BLUE = "B2", GREEN = "B3", RED = "B4", NIR = "B5")
	)
	
	if(!missing(sensor)){
		if(!sensor %in% names(SENSORS)) stop(paste0("Unknown sensor. Please provide the 'bands' argument or 'sensor' as one of ", names(SENSORS)))
		bands <- SENSORS[[sensor]]
		if(any(!bands %in% names(inputRaster))) stop("Bandnames of inputRaster do not match the required format or are missing. Please provide 'bands' argument manually or make sure the names(inputRaster) follow the 'B1' 'B2'  ... format if you want to make use of the 'sensor' argument.")
	}
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
				if(any(!need %in% names(bands))) stop("Band specification(s) of >> ", paste(names(bands)[!names(bands) %in% need], collapse = ","), 
							" << are missing or do not match layer names in the brick/stack. \nPlease specify the correct layer number or name in a list, e.g. bands = list(RED = 'B4', NIR = 'B5')", call. = FALSE)
				need <- unlist(bands[need])
			})
	names(args) <- indices 
	
	## We do this in a separate step, so we can throw an error before we start the calculations
	inList <- lapply(indices, function(index) {
				if(verbose) print(paste0("Calculating ", index))
				overlay(inputRaster[[args[[index]]]], fun = INDICES[[index]])
			})
	
	## Combine and return
	outStack <- stack(inList)
		
	## Write file if filename is provided. Doing it this way we write the file twice. We could provide filenames to overlay instead and return a stack so we only write once. 
	## But then we have an output of n single files instead of one multi-layer file containing all indices.
	## Maybe we should make this optional
	if(any(grepl("file", names(list(...))))) outStack <-  writeRaster(outStack, ...)
	
	names(outStack) <- indices	 

	return(outStack)
}