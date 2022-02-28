#' Encode QA Conditions to Integers
#' 
#' Intended for use with Landsat 16-bit QA bands. Converts pixel quality flags from human readable to integer, which can then be used to 
#' subset a QA image. Please be aware of the default settings which differ for different parameters. 
#' Depending on, which \code{sensor} and \code{legacy} is selected, some quality parameters are not used, since the sequences of available bitwise quality designations differ per sensor and collection.
#' 
#' @param fill Designated fill. Options: \code{c("yes", "no", "all")}. 
#' @param terrainOcclusion Terrain induced occlusion. Options: \code{c("yes", "no", "all")}.
#' @param radSaturation Number of bands that contain radiometric saturation. Options: \code{c("na", "low", "med", "high", "all")} for no bands, 1-2 bands, 3-4 bands, 5 or more bands contain saturation.
#' @param cloudMask Cloud mask. Options: \code{c("yes", "no", "all")}.
#' @param cloud Cloud confidence. Options: \code{c("na", "low", "med", "high", "all")}.
#' @param cloudShadow Cloud shadow confidence. Options: \code{c("yes", "no", "all")}.
#' @param snow Snow / ice confidence.  Options: \code{c("na", "low", "med", "high", "all")}.
#' @param cirrus Cirrus confidence.  Options: \code{c("na", "low", "med", "high", "all")}.
#' @param water Water confidence. Options: \code{c("na", "low", "med", "high", "all")}.
#' @param droppedFrame Dropped frame. Options: \code{c("yes", "no", "all")}.
#' @param droppedPixel Dropped pixel. Options: \code{c("yes", "no", "all")}.
#' @param sensor Sensor to encode. Options: \code{c("OLI", "TIRS", "ETM+", "TM", "MSS")}.
#' @param legacy Encoding systematic Options: \code{c("collection1", "pre_collection")}. Default is "collection1" for the Landsat Collection 1 8-bit quality designations. Use "pre_collection" for imagery downloaded before the Collection 1 quality designations were introduced
#' 
#' @note 
#' Only currently populated bits are available as arguments.
#' 
#' @references 
#' \url{https://www.usgs.gov/landsat-missions/landsat-collection-1-level-1-quality-assessment-band} for Collection 1 quality designations (\code{legacy = "collection1"})
#' @export 
#' @return
#' Returns the Integer value for the QA values
#' @examples 
#' encodeQA(snow = "low", cirrus = c("med", "high"), cloud = "high")
encodeQA <- function(fill = "no", terrainOcclusion = "no", radSaturation = "na", cloudMask = "all", cloud = "all", 
                     cloudShadow = "all", snow = "all", cirrus = "all", droppedPixel = "no",
                     water = "all", droppedFrame = "no", sensor = "OLI", legacy = "collection1"){
  
  ## Input checks
  if(legacy == "pre_collection" & !any(sensor %in% c("OLI", "TIRS"))) stop("For argument legacy = 'pre_collection', argument sensor can only be 'OLI' or 'TIRS'.", call.=FALSE)
  
  s <- list(fill = fill, terrainOcclusion = terrainOcclusion, cloudMask = cloudMask, droppedFrame = droppedFrame, droppedPixel = droppedPixel)
  lapply(names(s), function(i) if(any(!s[[i]] %in% c("yes", "no", "all"))) stop(i, " is a single bit parameter. Can digest only values c('yes', 'no', 'all')", call.=FALSE))
  s <- list(water = water, snow = snow, cirrus = cirrus, cloud = cloud, cloudShadow = cloudShadow, radSaturation = radSaturation)
  lapply(names(s), function(i) if(any(!s[[i]] %in% c("na", "low", "med", "high", "all"))) stop(i, " is a double bit parameter. Can digest only values c('na', 'low', 'med', 'high', 'all')", call.=FALSE))
  
  ## Convert to bit representation
  sing <- list(no = "0", yes = "1", all = c("0", "1"))
  doub <- c(na = "00", low = "01", med = "10", high = "11")
  doub <- c(as.list(doub), list(all = doub))
  
  xfill <- sing[fill]
  xterrainOcclusion <- sing[terrainOcclusion]
  xradSaturation <- doub[radSaturation]
  xcloud <- sing[cloudMask]
  xcloudConfidence <- doub[cloud]
  xcloudShadow <- doub[cloudShadow]
  xsnow <- doub[snow]
  xcirrus <- doub[cirrus]
  xdroppedPixel <- sing[droppedPixel]
  xwater <- doub[water]
  xdroppedFrame <- sing[droppedFrame]
  
  xresSing <- "0"
  xresDoub <- "00"
  
  ## Possible combinations
  if(legacy == "pre_collection"){
    li  <- c(xcloudConfidence, xcirrus, xsnow, rep(xresDoub, 2), xwater, xresSing, xterrainOcclusion, xdroppedFrame, xfill)
    names(li) <- c("cc", "cir", "snow", "veg", "cs", "water", "res", "ter", "drop", "fill")
  }
  if(legacy == "collection1"){
    if(any(sensor %in% c("OLI", "TIRS"))) li <- c(rep(xresSing, 3), xcirrus, xsnow, xcloudShadow, xcloudConfidence, xcloud, xradSaturation, xterrainOcclusion, xfill)
    if(any(sensor %in% c("TM", "ETM+"))) li <- c(rep(xresSing, 3), xresDoub, xsnow, xcloudShadow, xcloudConfidence, xcloud, xradSaturation, xterrainOcclusion, xfill)
    if(sensor == "MSS") li <- c(rep(xresSing, 3), rep(xresDoub, 3), xcloudConfidence, xcloud, xradSaturation, xterrainOcclusion, xfill)
    names(li) <- c(rep("res", 3), "cir", "snow", "cs", "cc", "cloudMask", "radSat", "ter", "fill")
  }
  
  li <- lapply(li, unlist)
  li <- expand.grid(li)
  
  ## Convert to integer
  binWords <-  apply(li, 1, paste, collapse = "")
  
  strtoi(binWords, base = 2)
}

#' Decode QA flags to bit-words
#' 
#' Intended for use with Landsat 16-bit QA bands. Decodes pixel quality flags from integer to bit-words.
#' 
#' @param x Integer (16bit)
#' @export
#' @seealso \link{encodeQA}
#' @examples
#' decodeQA(53248)
decodeQA <- function(x){
  bit <- intToBits(x)
  paste(tail(rev(as.integer(bit)), 16), collapse="")                        
}
