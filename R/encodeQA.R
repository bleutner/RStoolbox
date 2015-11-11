#' Encode QA Conditions to Integers
#' 
#' Intended for use with the Landsat 8 OLI QA band. Converts pixel quality flags from human readable to integer, which can then be used to 
#' subset the QA image. Please be aware of the default settings which differ for different parameters.
#' 
#' @param fill Designated fill. Options: \code{c("yes", "no", "all")}. 
#' @param droppedFrame Dropped frame. Options: \code{c("yes", "no", "all")}.
#' @param terrainOcclusion Terrain induced occlusion. Options: \code{c("yes", "no", "all")}.
#' @param water Water confidence. Options: \code{c("na", "low", "med", "high", "all")}.
#' @param snow Snow / ice confidence.  Options: \code{c("na", "low", "med", "high", "all")}.
#' @param cirrus Cirrus confidence.  Options: \code{c("na", "low", "med", "high", "all")}.
#' @param cloud Cloud confidence. Options: \code{c("na", "low", "med", "high", "all")}.
#' 
#' @note 
#' Only currently populated bits are available as arguments, i.e. vegetation confidence, cloud shadow and bit nr. 3. are currently useless and hence not available.
#' 
#' @references 
#' \url{http://landsat.usgs.gov/L8QualityAssessmentBand.php} 
#' @export 
#' @return
#' Returns the Integer value for the QA values
#' @examples 
#' encodeQA(snow = "low", cirrus = c("med", "high"), cloud = "high")
encodeQA <- function(fill = "no", droppedFrame = "no", terrainOcclusion = "no", 
        water = "all", snow = "all", cirrus = "all", cloud = "all"){
     
    ## Input checks
    s <- list(fill=fill, droppedFrame=droppedFrame, terrainOcclusion=terrainOcclusion)
    lapply(names(s), function(i) if(any(!s[[i]] %in% c("yes", "no", "all"))) stop(i, " is a single bit parameter. Can digest only values c('yes', 'no', 'all')", call.=FALSE))
    s <- list(water=water, snow=snow, cirrus=cirrus, cloud=cloud)
    lapply(names(s), function(i) if(any(!s[[i]] %in% c("na", "low", "med", "high", "all"))) stop(i, " is a double bit parameter. Can digest only values c('na', 'low', 'med', 'high', 'all')", call.=FALSE))
    
    ## Convert to bit representation
    sing = list(no="0", yes="1", all=c("0", "1"))
    doub = c(na="00", low="01", med="10", high="11")
    doub = c(as.list(doub), list(all=doub))
    
    xfill = sing[fill]
    xdroppedFrame = sing[droppedFrame]
    xterrainOcclusion = sing[terrainOcclusion]
    xreserved = "0"
    xwater = doub[water]
    xcloudShadow = "00"
    xvegetation = "00"
    xsnow = doub[snow]
    xcirrus = doub[cirrus]
    xcloud = doub[cloud]
    
    ## Possible combinations
    li  <- list(xcloud, xcirrus, xsnow, xvegetation, xcloudShadow, xwater, xreserved, xterrainOcclusion, xdroppedFrame, xfill)
    names(li) <- c("cloud", "cir", "snow", "veg", "cs", "water", "res", "ter", "drop", "fill")
    li <- lapply(li, unlist)
    li <- expand.grid(li)
    
    ## Convert to integer
    binWords <-  apply(li, 1, paste, collapse = "")
    
    strtoi(binWords, base = 2)
}

#' Decode QA flags to bit-words
#' 
#' Intended for use with the Landsat 8 OLI QA band. Decodes pixel quality flags from integer to bit-words.
#' @param x Integer (16bit)
#' @export
#' @seealso \link{encodeQA}
#' @examples
#' decodeQA(53248)
decodeQA <- function(x){
    bit <- intToBits(x)
    paste(tail(rev(as.integer(bit)), 16), collapse="")                        
}
