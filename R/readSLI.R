#' Read ENVI spectral libraries
#' 
#' read/write support for ENVI spectral libraries
#' 
#' @param path Path to spectral library file with ending .sli.
#' @details
#' ENVI spectral libraries consist of a binary data file (.sli) and a corresponding header (.hdr, or .sli.hdr) file. 
#' @return 
#' The spectral libraries are read into a data.frame. The first column contains the wavelengths and the remaining columns contain the spectra.
#' @seealso \code{\link{writeSLI}}
#' @export 
#' @template examples_SLI
readSLI <- function(path) {
    
    ## Figure out file naming convention of hdr file for either combination of 
    ## (filename.sli + filename.sli.hdr) OR (filename.sli + filename.hdr)
    hdr_path <- paste0(path, ".hdr")
    if(!file.exists(hdr_path)){
        hdr_path <- paste0(strsplit(path,"[.]")[[1]][1], ".hdr")
        if (!file.exists(hdr_path)){
            stop(paste0("Can't find header file of", path), call.= FALSE)
        }
    }
    
    ## Get header info
    hdr   <- readLines(hdr_path, n=-1L)
    bands <- .getNumeric(hdr[grep("samples", hdr)])
    lines <- .getNumeric(hdr[grep("lines", hdr)])
    data_type <- .getNumeric(hdr[grep("data type", hdr)])
    
    ## Extract spectra labels
    id <- .bracketRange(hdr, "spectra names")
    if(id[1]==id[2]) {
        labels <- hdr[(id[1])]
        labels <- strsplit(labels, "[{]")[[1]][2]
    } else {
        labels <- hdr[(id[1]+1):(id[2])]
    }
    
    labels <- gsub( "\\}| ", "", paste(labels, collapse = ","))
    labels <- unlist(strsplit(gsub(",,",",", labels), ","))
    
    ## Extract wavelengths
    id <- .bracketRange(hdr, "wavelength = ")
    wavelengths <- hdr[(id[1]+1):(id[2])]
    wavelengths <- gsub( "\\}| ", "", paste( wavelengths, collapse=","))
    wavelengths <- as.numeric( unlist( strsplit( gsub(",,",",", wavelengths), ",")))
    
    ## Read binary sli file
    if (data_type == 4) bytes <- 4
    if (data_type == 5) bytes <- 8	
    x <- data.frame(matrix(nrow=bands, ncol=lines))
    x[] <- readBin(path, "numeric", n = 1000000, size = bytes)
    colnames(x) <- labels
    x <- cbind(wavelengths,x)
    colnames(x)[1] <- "wavelength"
    return(x)
    
} ## EOF readSLI

#' Write ENVI spectral libraries
#' 
#' Writes binary ENVI spectral library files (sli) with accompanying header (.sli.hdr) files OR ASCII spectral library files in ENVI format. 
#' 
#' ENVI spectral libraries with ending .sli are binary arrays with spectra saved in rows. 
#' 
#' @param path path to spectral library file to be created.
#' @param x data.frame with first column containing wavelengths and all other columns containing spectra.
#' @param wavl.units wavelength units. Defaults to Micrometers. Nanometers is another typical option.
#' @param scaleF optional reflectance scaling factor. Defaults to 1.
#' @param mode character string specifying output file type. Must be one of \code{"bin"} for binary .sli files or \code{"ASCII"} for --guess what-- ASCII spectral library files (still in an ENVI compatible format).
#' @seealso \code{\link{readSLI}}
#' @export
#' @template examples_SLI
writeSLI <- function(x, path, wavl.units="Micrometers", scaleF=1, mode="bin") {
    
    ## Begin write binary mode
    if (mode== "bin") {
        ## Write header file
        sink(paste0(path,".hdr"))
        writeLines(paste0("ENVI\ndescription = {\n   ENVI SpecLib created using RStoolbox for R [", date(), "]}",
                        "\nsamples = ", nrow(x),
                        "\nlines   = ", ncol(x) - 1,
                        "\nbands   = ", 1,
                        "\nheader offset = 0",
                        "\nfile type = ENVI Spectral Library",
                        "\ndata type = 5",
                        "\ninterleave = bsq",
                        "\nsensor type = Unknown",
                        "\nbyte order = 0",
                        "\nwavelength units = ", wavl.units, 
                        "\nreflectance scale factor = ", scaleF,
                        "\nz plot range = {0.00,", ceiling(max(x[,2])*1.2),"}",
                        "\nz plot titles = {Wavelength, Reflectance}",
                        "\nband names = {",
                        "\nSpectral Library}",
                        "\nspectra names = {\n ",
                        paste(colnames(x)[-1],collapse=", "),"}",
                        "\nwavelength = {\n ",
                        paste(x[,1],collapse=", "),"}"))
        sink()
        
        ## Write actual binary file
        x1 <- as.vector(unlist(x[,-1]))
        writeBin(x1, path)
    } ## End write binary mode
    
    ## Begin write ASCII mode
    if (mode == "ASCII") {
        ## Create column descriptions
        collector <- character()
        for(i in 2:ncol(x)){
            collector <- append(collector, paste0("\nColumn ", i, ": ", colnames(x)[i], "~~",i))
        }
        sink(path)
        ## Write txt file header
        writeLines(paste0("ENVI ASCII Plot File [", date(),"]\n",
                        "Column 1: wavelength [!7l!3m]!N", 
                        paste0(collector, collapse="")))
        sink()
        ## Append data
        write.table(data.frame(x=rep("",nrow(x)),x), path, sep="  ", append= TRUE , row.names= FALSE, col.names= FALSE, quote= FALSE)
    } ## End ASCII mode
    
} ## EOF writeSLI


## Helper functions
## Find matching bracket to a matched pattern
.bracketRange <- function(x, pattern) {
    begin   <- which(grepl(pattern, x))
    closers <- which(grepl("}", x))
    if(begin %in% closers){
        # Openeing and closing brackets on the same line
        return(rep(begin, 2))
    } else {
        # Opening and closing brackets on different lines
        end <- closers[(closers - begin) > 0][1]
        return(c(begin, end))
    }
}






