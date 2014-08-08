#' Read landsat MTL metadata files
#' 
#' Besides reading metadata, readMeta deals with legacy versions of Landsat metadata files and where possible adds missing information (radiometric gain and offset, earth-sun distance).
#' 
#' @param file path to Landsat MTL file (...MTL.txt)
#' @param unifiedMetadata logical. If \code{TRUE} some relevant etadata of Landsat 5:8 are homogenized into a standard format and appended to the original metadata.
#' @return Returns a list containing the Metadata of the MTL file, structured by the original grouping.
#' 
#' @import landsat
#' @export 
#' 
#' 
#' 
readMeta <- function(file, unifiedMetadata = TRUE){
	if(!grepl("MTL", file) & !grepl("xml", file)) warning("The Landsat metadata file you have specified looks unusual. Typically the filename contains the string 'MTL' or 'xml'. Are you sure you specified the right file? \n I'll try to read it but check the results!")
	
	## Read mtl file
	metaDataFormat <- if(grepl('xml', file)) "XML" else "MTL"
	
	if(metaDataFormat == "MTL") {
		## PROCESS LPS MTL FILES
		
		meta <- read.delim(file, sep = "=", head = FALSE, stringsAsFactors = FALSE, strip.white = TRUE, skip = 1, skipNul = TRUE)
		meta <- meta[-(nrow(meta)-c(1,0)),]
		
		## Retrieve groups
		l <- meta[grep("GROUP",meta[,1]),]
		
		## Assemble metadata list
		meta <- lapply(unique(l[,2]), FUN = function(x){
					w <- which(meta[,2] == x)
					m <- meta[(w[1]+1):(w[2]-1),]
					rownames(m) <- m[,1]
					m <- m[ , 2, drop = FALSE]
					colnames(m) <- "VALUE"
					return(m)
				})
		
		names(meta) <- unique(l[,2])
		
		## Legacy MTL? 
		legacy <- "PROCESSING_SOFTWARE" %in% rownames(meta$PRODUCT_METADATA)
		if(legacy) message("This scene was processed before August 29, 2012. Using MTL legacy format. Some minor infos such as SCENE_ID will be missing")
		
		if(unifiedMetadata){
			
			meta[["UNIFIED_METADATA"]] <- list(
					SPACECRAFT_ID 		= {SAT <- paste0("LANDSAT", .getNumeric(meta$PRODUCT_METADATA["SPACECRAFT_ID",]))},
					SENSOR_ID 			= meta$PRODUCT_METADATA["SENSOR_ID",]	,			
					SCENE_ID 			= meta$METADATA_FILE_INFO["LANDSAT_SCENE_ID",],  ## could assemble name for legacy files: http://landsat.usgs.gov/naming_conventions_scene_identifiers.php
					DATA_TYPE			= if(!legacy) meta$PRODUCT_METADATA["DATA_TYPE",] else meta$PRODUCT_METADATA["PRODUCT_TYPE",],
					ACQUISITION_DATE	= {date <- if(!legacy) meta$PRODUCT_METADATA["DATE_ACQUIRED",] else meta$PRODUCT_METADATA["ACQUISITION_DATE",]},
					PROCESSING_DATE		= if(!legacy) meta$METADATA_FILE_INFO["FILE_DATE",] else meta$METADATA_FILE_INFO["PRODUCT_CREATION_TIME",], 
					PATH				= as.numeric(meta$PRODUCT_METADATA["WRS_PATH",]),
					ROW					= if(!legacy) as.numeric(meta$PRODUCT_METADATA["WRS_ROW",]) else as.numeric(meta$PRODUCT_METADATA["STARTING_ROW",]),
					RADIOMETRIC_RES		= if(SAT == "LANDSAT8") 16 else 8,				
					FILES				= {files <- row.names(meta[["PRODUCT_METADATA"]])[grep("^.*FILE_NAME", row.names(meta$PRODUCT_METADATA))]
						files <- files[grep("^.*BAND",files)]
						files <- meta[["PRODUCT_METADATA"]][files,]	},
					
					BANDS 				= {junk <- unique(sapply(str_split(files, "_B"), "[" ,1 ))
						bds <- str_replace(str_replace(files, paste0(junk,"_"), ""), {if(SAT=="LANDSAT5") "0.TIF" else ".TIF"}, "")
					},
					BAND_TYPE 			= {
						ty <- rep("image", length(bds))
						ty[grepl("QA", bds)] <- "qa"
						ty
					},
					## INSOLATION
					NA_VALUE 			= rep(0, length(ty)),
					SUN_AZIMUTH			= if(!legacy) as.numeric(meta$IMAGE_ATTRIBUTES["SUN_AZIMUTH",]) else as.numeric(meta$PRODUCT_PARAMETERS["SUN_AZIMUTH",]),
					SUN_ELEVATION		= if(!legacy) as.numeric(meta$IMAGE_ATTRIBUTES["SUN_ELEVATION",]) else as.numeric(meta$PRODUCT_PARAMETERS["SUN_ELEVATION",]),
					EARTH_SUN_DISTANCE  = {es <- meta$IMAGE_ATTRIBUTES["EARTH_SUN_DISTANCE",]
						if(is.null(es) || is.na(es)) es <- .ESdist(date)
						as.numeric(es)}
			)
			
			## RADIOMETRIC CORRECTION/RESCALING PARAMETERS
			RADCOR <-  if(!legacy) { list(		
								RAD_OFFSET				= {
									r <- meta$RADIOMETRIC_RESCALING
									r[,1]		<- as.numeric(r[,1])
									bandnames	<- str_c("B", str_replace(rownames(r), "^.*_BAND_", ""))
									go			<- grep("RADIANCE_ADD*", rownames(r))
									ro 			<- r[go,]
									names(ro)	<- bandnames[go]
									ro},
								RAD_GAIN				= {go			<- grep("RADIANCE_MULT*", rownames(r))
									ro 			<- r[go,]
									names(ro)	<- bandnames[go]
									ro},
								REF_OFFSET				= {	go			<- grep("REFLECTANCE_ADD*", rownames(r))
									ro 			<- r[go,]
									names(ro)	<- bandnames[go]
									ro},
								REF_GAIN				= {go			<- grep("REFLECTANCE_MULT*", rownames(r))
									ro 			<- r[go,]
									names(ro)	<- bandnames[go]
									ro})
										
					} else {
						
						bandnames <- paste0("B", .getNumeric(rownames(meta$MIN_MAX_RADIANCE)))
						bandnames <- bandnames[seq(1, length(bandnames), 2)]
						
						L <- diff(as.numeric(meta$MIN_MAX_RADIANCE[,1]))
						L <- L[seq(1, length(L), 2)] 
						
						Q <- diff(as.numeric(meta$MIN_MAX_PIXEL_VALUE[,1]))  
						Q <- Q[seq(1, length(Q), 2)]
						
						G_rescale <- L/Q
						B_rescale <- as.numeric(meta$MIN_MAX_RADIANCE[,1])[seq(2,nrow(meta$MIN_MAX_RADIANCE),2)] - (G_rescale) * 1
						
						RAD_OFFSET 	<- -1 * B_rescale / G_rescale  
						RAD_GAIN	 <- 1 / G_rescale
						
						names(RAD_OFFSET) <- names(RAD_GAIN) <- bandnames
												
						list(RAD_OFFSET = RAD_OFFSET, RAD_GAIN = RAD_GAIN)
						
					}
			
	 if(SAT == "LANDSAT8"){
				RADCOR$K1 ={ r <- meta$TIRS_THERMAL_CONSTANTS
					r[,1]		<- as.numeric(r[,1])
					bandnames	<- str_c("B", str_replace(rownames(r), "^.*_BAND_", ""))
					go			<- grep("K1", rownames(r))
					ro 			<- r[go,]
					names(ro)	<- bandnames[go]
					ro}
				RADCOR$K2 = {go			<- grep("K2", rownames(r))
					ro 			<- r[go,]
					names(ro)	<- bandnames[go]
					ro}				
			} else {
				TAB7 <- list(LANDSAT4 = c(B6=671.62,B6=1284.3), # TAB7 from Chander 2009
						LANDSAT5 = c(B6=607.76,B6=1260.56),
						LANDSAT7 = c(B6=666.09,B6=1282.71))
					
				RADCOR$K1 <- TAB7[[SAT]][1]
				RADCOR$K2 <- TAB7[[SAT]][2]
			}
			
			meta[["UNIFIED_METADATA"]] <- c(meta[["UNIFIED_METADATA"]], RADCOR)
		}
	} else {
		## PROCESS ESPA LEDAPS XML FILES
		meta <- xmlParse(file)
		meta <- xmlToList(meta)
		names(meta$bands) <- str_replace_all(unlist(sapply(meta$bands, "[", "long_name")), " ", "_")
		
		if(unifiedMetadata){
			
			atts <- sapply(meta$bands, "[", ".attrs")
			
			meta[["UNIFIED_METADATA"]] <- list(
					SPACECRAFT_ID 		= {SAT <- paste0("LANDSAT", .getNumeric(meta$global_metadata$satellite))},
					SENSOR_ID 			= meta$global_metadata$instrument,			
					SCENE_ID 			= SID <- str_replace(meta$global_metadata$lpgs_metadata_file, "_MTL.txt", ""),  ## could assemble name for legacy files: http://landsat.usgs.gov/naming_conventions_scene_identifiers.php
					DATA_TYPE			= if(meta$bands[[1]]$.attrs["product"] == "sr_refl") "SR", 
					ACQUISITION_DATE	= {date <- meta$global_metadata$acquisition_date},
					PROCESSING_DATE		= meta$bands[[1]]$production_date, 
					PATH				= as.numeric(meta$global_metadata$wrs["path"]),
					ROW					= as.numeric(meta$global_metadata$wrs["row"]),
					
					FILES				= {files <- sapply(meta$bands, "[[", "file_name")
						names(files) <- NULL
						files},					
					BANDS 				= {	
						bds <- grepl("_band", files)
						toa <- grepl("_toa_", files)
						qas <- grepl("qa", files)	
						bnames				<- toupper(str_replace(nams, paste0(SID, "_"), ""))					
						bnames[bds]			<- paste0("B", .getNumeric(bnames[bds]))
						bnames[bds & qas] 	<- paste0(bnames[bds & qas], "_QA")
						bnames				<- str_replace(str_replace(str_replace(bnames, "\\.TIF", ""), "SR_", ""), "TOA_", "")
						bnames[toa] 		<- paste0(bnames[toa], "_TOA")
						bnames
					},
					BAND_TYPE			= {ty <- sapply(atts, "[" , "category")
						names(ty) <- NULL
						ty
					},
					NA_VALUE 			= as.numeric(sapply(atts, "[" , "fill_value")),
					SATURATE_VALUE 		= as.numeric(sapply(atts, "[" , "saturate_value")),
					SCALE_FACTOR 		= as.numeric(sapply(atts, "[" , "scale_factor")),
					
					SUN_AZIMUTH			= as.numeric(meta$global_metadata$solar_angles["azimuth"]),
					SUN_ELEVATION		= 90 - as.numeric(meta$global_metadata$solar_angles["zenith"]),
					EARTH_SUN_DISTANCE  = {.ESdist(date)}
			)
			
		}
		
	}
	return(meta)
}





#' Import separate Landsat files into single stack
#' 
#' Reads Landsat MTL or XML metadata files and loads single Landsat Tiffs into a rasterStack.
#' Be aware that by default stackLS() does NOT import panchromatic bands nor thermal bands with resolutions != 30m.
#' 
#' @param file character. Path to Landsat MTL metadata file.
#' @param allResolutions logical. if \code{TRUE} a list will be returned with length = unique spatial resolutions.
#' @param resampleTIR logical. As of  the USGS resamples TIR bands to 30m. Use this option if you use data processed prior to February 25, 2010 which has not been resampled.
#' @param resamplingMethod character. Method to use for TUR resampling ('ngb' or 'bilinear'). Defaults to 'ngb' (nearest neighbor).
#' @param products character vector. Which products should be returned in the stack? (only relevant for LS8 and LEDAPS processed products). 'image': image data, 'index': multiband indices, 'qa' quality flag bands. 
#' @return Either a list of rasterStacks comprising all resolutions or only one rasterStack comprising only 30m resolution imagery
#' @note 
#' Be aware that by default stackLS() does NOT import panchromatic bands nor thermal bands with resolutions != 30m. Use the allResolutions argument to import all layers.
#' 
#' The USGS uses cubic convolution to resample TIR bands to 30m resolution. In the opinion of the author this may not be the best choice for supersampling. 
#' Therefore the default method in this implementation is nearest neighbor. Keep this in mind if you plan to compare TIR bands created by differing resampling routines.
#' Typically, however, you will already have the USGS 30m TIR products, so no need to worry...
#' @export
stackMeta <- function(file, allResolutions = FALSE,  resampleTIR = FALSE, resamplingMethod = "ngb", products = c("image", "index", "qa")){
	
	## Read metadata and extract layer file names
	meta  <- readMeta(file)
	files <- meta$UNIFIED_METADATA$FILES
	
	## Load layers
	path  <- if(basename(file) != file)  str_replace(file, basename(file), "") else NULL
	
	## Import rasters
	rl <- lapply(paste0(path, files), raster)
	resL <- lapply(lapply(rl, res),"[", 1)
	
	if(any(resL > 30)) {
		message("Your Landsat data includes TIR band(s) which were not resampled to 30m.
						\nYou can set resampleTIR = TRUE to resample TIR bands to 30m if you want a single stack")
		
		## Resample TIR to 30m
		if(resampleTIR){
			for(i in which(resL > 30))
				rl[[i]] <- resample(rl[[i]], rl[[which(resL == 30)[1]]], method = resamplingMethod)		
		}
	}
	
	## Stack
	returnRes <- if(allResolutions) unlist(unique(resL)) else 30
	
	LS 	<- lapply(returnRes, function(x){
				s			<- stack(rl[resL == x])
				names(s) 	<- meta$UNIFIED_METADATA$BANDS[resL == x]
				NAvalue(s)	<- meta$UNIFIED_METADATA$NA_VALUE[resL == x]	
				s <- s[[ which(names(s) %in% meta$UNIFIED_METADATA$BANDS[meta$UNIFIED_METADATA$BAND_TYPE %in% products])	]]
				s
			})
	
	if(!allResolutions) LS <- LS[[1]]
	
	return(LS)
}