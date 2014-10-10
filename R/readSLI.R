#' Read ENVI spectral libraries
#' 
#' ENVI spectral libraries consist of a binary data file (.sli) and a corresponding header (.hdr, or .sli.hdr) file. Spectra are read into a data.frame with the columns:
#' Wavelength, Spectrum 1, Spectrum 2 ...
#' 
#' @param path path to spectral library file with ending .sli.
#' @references \href{http://geol.hu/data/online_help/ENVI_Header_Format.html}{ENVI header format}
#' @seealso \code{\link{writeSLI}}
#' @export 
#' @examples
#' ## Create fake spectra
#' data <- data.frame(wavelength=350:2500, spectrumA=cumsum(abs(rnorm(2151))), spectrumB=cumsum(abs(rnorm(2151))))
#' pathToFile <- str_c(tempdir(),"/specLib.sli")
#' 
#' ## Write to binary spectral library
#' writeSLI(x = data, path = pathToFile)
#' 
#' ## Read from binary spectral library
#' dataRe <- readSLI(path = pathToFile)
#' 
#' ## Check whether they are the same
#' all.equal(data, dataRe)
#' ## oops? nope, only colnames mismatch. numbers are fine.
#' cat("colnames:\noriginal: ",colnames(data),"\nre-read: ",colnames(dataRe))
readSLI <- function(path) {
    
    ## Figure out file naming convention of hdr file for either combination of 
    ## (filename.sli + filename.sli.hdr) OR (filename.sli + filename.hdr)
    if(file.exists(str_c(path, ".hdr"))){
        hdr_path <- str_c(path, ".hdr")
    } else {
        if (file.exists(str_c(str_split(path,"[.]")[[1]][1], ".hdr"))){
            hdr_path <- str_c(str_split(path,"[.]")[[1]][1], ".hdr")
        } else {
            stop(paste0("Can't find header file of", path), call.= FALSE)
        }
    }
    
    ## Get header info
    hdr <- readLines(hdr_path, n=-1L)
    bands <- as.numeric(tail(.d(hdr, "samples ="), 1))
    lines <- as.numeric(tail(.d(hdr, "lines   ="), 1))
    data_type <- as.numeric(tail(.d(hdr, "data type ="), 1))
    
    ## Extract spectra labels
    id <- .bracketRange(hdr, "spectra names")
    if(id[1]==id[2]) {
        labels <- hdr[(id[1])]
        labels <- str_split(labels, "[{]")[[1]][2]
    } else {
        labels <- hdr[(id[1]+1):(id[2])]
    }
    
    labels <- str_replace_all(paste(labels,collapse=","), "[ }]","")
    labels <- unlist( str_split( str_replace_all( labels, ",,", ","), ","))
    
    ## Extract wavelengths
    id <- .bracketRange(hdr, "wavelength = ")
    wavelengths <- hdr[(id[1]+1):(id[2])]
    wavelengths <- str_replace_all( paste( wavelengths, collapse=","), "[ }]", "")
    wavelengths <- as.numeric( unlist( str_split( str_replace_all( wavelengths, ",,", ","), ",")))
    
    ## Read binary sli file
    if (data_type == 4) bytes <- 4
    if (data_type == 5) bytes <- 8	
    x <- data.frame(matrix(nrow=bands, ncol=lines))
    x[] <- readBin(path, "numeric", n = 1000000, size = bytes)
    colnames(x) <- labels
    x <- cbind(wavelengths,x)
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
#' @references \href{http://geol.hu/data/online_help/ENVI_Header_Format.html}{ENVI header format}
#' @export
#' @examples
#' ## Create fake spectra
#' data <- data.frame(wavelength=350:2500, spectrumA=cumsum(abs(rnorm(2151))), spectrumB=cumsum(abs(rnorm(2151))))
#' pathToFile <- str_c(tempdir(),"/specLib.sli")
#' 
#' ## Write to binary spectral library
#' writeSLI(x = data, path = pathToFile)
#' 
#' ## Read from binary spectral library
#' dataRe <- readSLI(path = pathToFile)
#' 
#' ## Check whether they are the same
#' all.equal(data, dataRe)
#' ## oops? nope, only colnames mismatch. numbers are fine.
#' cat("colnames:\noriginal: ",colnames(data),"\nre-read: ",colnames(dataRe))
writeSLI <- function(x, path, wavl.units="Micrometers", scaleF=1.000000, mode="bin") {
    
    ## Begin write binary mode
    if (mode== "bin") {
        ## Write header file
        sink(str_c(path,".hdr"))
        writeLines(str_c("ENVI\ndescription = {\n   ENVI SpecLib created using RStoolbox for R [", date(), "]}",
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
            collector <- append(collector, str_c("\nColumn ", i, ": ", colnames(x)[i], "~~",i))
        }
        sink(path)
        ## Write txt file header
        writeLines(str_c("ENVI ASCII Plot File [", date(),"]\n",
                        "Column 1: wavelength [!7l!3m]!N", 
                        str_join(collector, collapse="")))
        sink()
        ## Append data
        write.table(data.frame(x=rep("",nrow(x)),x), path, sep="  ", append= TRUE , row.names= FALSE, col.names= FALSE, quote= FALSE)
    } ## End ASCII mode
    
} ## EOF writeSLI


## Helper functions
## Find matching bracket to a matched pattern
.bracketRange <- function(x, pattern) {
    begin <- which(grepl(pattern, x))
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


## Find a pattern an split the resulting string
.d <-function(x,string){unlist(str_split(x[grep(string,x)],"[ \\]"))}





