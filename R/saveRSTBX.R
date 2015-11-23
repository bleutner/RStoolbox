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
#' rpc   <- rasterPCA(input, filename = file, nSample = 100)
#' ## Save object
#' saveRSTBX(rpc, filename=file)
#' ## Which files were written?
#' list.files(tempdir(), pattern = basename(file))
#' ## Re-read files
#' re_rpc <- readRSTBX(file)
#' ## Compare 
#' all.equal(re_rpc, rpc)
#' file.remove(file)
#' }
#' @name saveRSTBX
NULL

#' @describeIn saveRSTBX Save RStoolbox object to file 
saveRSTBX <- function(x, filename, format ="raster", ...){
    
    stopifnot(inherits(x, "RStoolbox"))
    
    rdsFile <- rastFile <- .fullPath(filename)
    extension(rdsFile)  <- ".rds"
    extension(rastFile) <- .rasterExtension(format)          
    f <- raster::filename(x$map)
    if(inherits(x$map, "Raster")){
        if(inMemory(x$map)){
            ## In memory
            x$map <- writeRaster(x$map, filename = rastFile, format=format, ...)
        } else {
            if(f!=rastFile){
                ## File onDisk but not in requested path and/or format
                x$map <- writeRaster(x$map, filename = rastFile, format=format, ...)                           
            }
        }  
    }
    saveRDS(x, rdsFile)
}

#' @describeIn saveRSTBX Read files saved with saveRSTBX
#' @export 
readRSTBX <- function(filename){
    rdsFile <- rastFile <- .fullPath(filename)
    extension(rdsFile)  <- ".rds"
    x 	    <- readRDS(rdsFile)
    if(!inherits(x, "RStoolbox")) stop(filename, "is not a RStoolbox object.", call. = FALSE)
    namesBU <- names(x$map) ## backup names (might get lost between file formats)
    if(inherits(x$map, "Raster")){
        extension(rastFile) <-  extension(filename(x$map))  
        if(!file.exists(rastFile)) {
            warning("Corresponding raster file ", rastFile, " cannot be found.  \nThe *.rds and the raster file must be located in the same directory.")
            x$map <- "Raster map not found"
        } else {
            x$map <- if(inherits(x$map, "RasterLayer")) raster(rastFile) else brick(rastFile) 
        }          
        names(x$map) <- namesBU
    }
    x
}


