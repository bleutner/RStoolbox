#' Save and Read RStoolbox Classification Results
#' 
#' Saves objects of classes unsuperClass, superClass, rasterPCA and fCover to
#' file. Useful to archive the fitted models.
#'
#' @return
#' The output of writeRSTBX will be at least two files written to disk:
#' a) an .rds file containing the object itself and 
#' b) the raster file (depending on the driver you choose this can be more than two files).
#' 
#' @note All files must be kept in the same directory to read the full object back into R
#' by means of readRSTBX. You can move them to another location but you'll have to move *all* of them 
#' (just like you would with Shapefiles). In case the raster file(s) is missing, readRSTBX will still
#' return the object but the raster will be missing.
#' 
#' writeRSTBX and readRSTBX are convenience wrappers around saveRDS, readRDS. This means 
#' you can read all files created this way also with base functionality as long as you don't move your files.
#' This is because x$map is a Raster* object and hence contains only a static link to the file on disk.
#' 
#' @param x RStoolbox object of classes c("fCover", "rasterPCA", "superClass", "unsuperClass")
#' @param filename Character. Path and filename. Any file extension will be ignored.
#' @param format Character. Driver to use for the raster file
#' @param ... further arguments passed to writeRaster
#' @export 
#' @examples 
#' \dontrun{
#' input <- brick(system.file("external/rlogo.grd", package="raster"))
#' ## Create filename
#' file  <- paste0(tempdir(), "/test", runif(1))
#' ## Run PCA
#' rpc   <- rasterPCA(input, nSample = 100)
#' ## Save object
#' saveRSTBX(rpc, filename=file)
#' ## Which files were written?
#' list.files(tempdir(), pattern = basename(file))
#' ## Re-read files
#' re_rpc <- readRSTBX(file)
#' ## Remove files 
#' file.remove(list.files(tempdir(), pattern = basename(file), full = TRUE))
#' }
#' @name saveRSTBX
NULL

#' @describeIn saveRSTBX Save RStoolbox object to file 
saveRSTBX <- function(x, filename, format = "raster", ...){
    stopifnot(inherits(x, "RStoolbox"))

    if(!inherits(x$map, "SpatRaster")){
        x$map <- .toTerra(x$map)
    }

    rdsFile <- rastFile <- .fullPath(filename)
    rdsFile <- paste0(rdsFile, ".rds")
    rastFile <- paste0(rastFile, .rasterExtension(format))

    f <- terra::sources(x$map)

    if(inMemory(x$map)){
        x$map <- wrap(writeRaster(x$map, filename = rastFile, ...))
    } else {
        if(f != rastFile){
            x$map <- wrap(writeRaster(x$map, filename = rastFile, ...))
        }
    }

    base::saveRDS(x, rdsFile)
}

#' @describeIn saveRSTBX Read files saved with saveRSTBX
#' @export 
readRSTBX <- function(filename){
    rdsFile <- rastFile <- .fullPath(filename)

    x <- readRDS(rdsFile)

    try(x$map <- unwrap(x$map), silent = FALSE)

    if(!inherits(x, "RStoolbox"))
      stop(filename, "is not a RStoolbox object.", call. = FALSE)

    if(inherits(x$map, "SpatRaster")){
        rastFile <- paste0(rastFile, sources(x$map))
        if(!file.exists(rastFile)) {
            warning("Corresponding raster file ", rastFile, " cannot be found.  \nThe *.rds and the raster file must be located in the same directory.")
            x$map <- "Raster map not found"
        }
    }
    x
}

test <- function(){
    devtools::load_all()

    train <- readRDS(system.file("external/trainingPoints.rds", package="RStoolbox"))
    sc <- superClass(rlogo, train, tuneLength = 1, resp="class")

    ## Save and re-import
    outbase <- paste0(tempdir(),"/test-RSTOOLBOX-sc")
    saveRSTBX(sc, outbase , overwrite = TRUE)
    readRSTBX(paste0(outbase, ".rds"))
}
