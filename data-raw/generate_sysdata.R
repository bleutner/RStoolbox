## *******************************************************************************************************************
## ESdistance (Observer range) obtained from NASA JPL HORIZONS Web-Interface
## http://ssd.jpl.nasa.gov/horizons.cgi 
## for the time period 1970-01-01 : 2069-12-31 deltaT = 1d
## The 100year distances (in AU) were then averaged per DOY 
## resulting in this lookup vector ranging from DOY 1 : 336
.ESdistance <- readRDS("data-raw/sun_earth_dists.rds")


## *******************************************************************************************************************
## Landsat auxilliary data. Taken from Chander et al 2009
## spatRes resampling: http://landsat.usgs.gov/band_designations_landsat_satellites.php
.LANDSATdb <- list(
        LANDSAT5 = list (
                TM = data.frame(band = paste0("B", 1:7, "_DN"),
                        bandtype = c(rep("REF", 5), "TIR", "REF"),
                        centerWavl = c(0.485, 0.569, 0.66, 0.840, 1.676, 11.435, 2.223),
                        spatRes1 = rep(30, 7),
                        spatRes2 = c(rep(30,5), 60, 30), ## TM Band 6 was acquired at 120-meter resolution, but products processed before February 25, 2010 are resampled to 60-meter pixels. Products processed after February 25, 2010 are resampled to 30-meter pixels.
                        esun = c(1983, 1796, 1536, 1031, 220, NA, 83.44))
        ),
        LANDSAT7 = list(
                ETM = data.frame(band = paste0("B",1:8, "_DN"),
                        bandtype = c(rep("REF", 5), "TIR", "REF", "PAN"),
                        spatRes1 = c(rep(30, 7), 15),
                        spatRes2 = c(rep(30,5), 60, 30, 15),  ## ETM+ Band 6 is acquired at 60-meter resolution. Products processed after February 25, 2010 are resampled to 30-meter pixels.
                        centerWavl = c(0.485, 0.560, 0.660, 0.835, 1.650,11.335,2.220,0.706),
                        esun = c(1997,1812,1533,1039,230.8,NA,84.9,1362)
                )
        ),
        LANDSAT8 = list(
                OLI_TIRS = data.frame(band = c(paste0("B",1:11, "_DN"), "BQA"),
                        bandtype = c(rep("REF", 7), "PAN", "REF", "TIR", "TIR", "QA"),
                        spatRes1 = c(rep(30, 7), 15, rep(30,4)),
                        spatRes2 = c(rep(30, 7), 15, rep(30,4)),  ## ETM+ Band 6 is acquired at 60-meter resolution. Products processed after February 25, 2010 are resampled to 30-meter pixels.
                        centerWavl = c(0.44,0.48,0.56,0.655,0.865,1.61,2.2,0.59,1.37,10.6,11.5, NA), 
                        esun = c(NA, 2067, 1893, 1603, 972.6, 245, 79.72, NA, 399.7, NA, NA, NA ) ## http://www.gisagmaps.com/landsat-8-atco/ ##http://landsat.usgs.gov/Landsat8_Using_Product.php
                )
        )
) 

exponents <- c(-4, -2, -1, -.7, -.5)
for(s in names(.LANDSATdb)){
    bandType		<- .LANDSATdb[[s]][[1]][,"bandtype"] == "REF"
    centerWavl		<- .LANDSATdb[[s]][[1]][bandType, "centerWavl"] 
    bands 			<- .LANDSATdb[[s]][[1]][bandType, "band"]
    
    ## Calc Chavez Tab 1
    TAB1			<- sapply(exponents, function(x) centerWavl ^ x)
    rownames(TAB1)  <- bands
    colnames(TAB1)	<- c("veryClear", "clear", "moderate", "hazy", "veryHazy")
    
    ## Calc Chavez Tab 2, but only until SHVB = B4, larger wavelengths don't make sense to estimate haze
    TAB2 <- lapply(paste0("B", 1:4, "_DN"), function(SHVB){ sweep(TAB1, 2, TAB1[SHVB,], "/")})
    TAB2 <- do.call("cbind", TAB2)
    colnames(TAB2) <- paste0(rep(paste0("B", 1:4, "_DN"), each = 5),"_", colnames(TAB2))
    
    .LANDSATdb[[s]][[1]] <-  merge(.LANDSATdb[[s]][[1]] , TAB2, by.x = "band", by.y = "row.names", all.x = TRUE, sort = FALSE)
}



## *******************************************************************************************************************
## Save internal data
save(.ESdistance, .LANDSATdb, file = "R/sysdata.rda", compress = "gzip")
