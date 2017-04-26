## *******************************************************************************************************************
## ESdistance (Observer range) obtained from NASA JPL HORIZONS Web-Interface
## http://ssd.jpl.nasa.gov/horizons.cgi 
## for the time period 1970-01-01 : 2069-12-31 deltaT = 1d
## The 100year distances (in AU) were then averaged per DOY 
## resulting in this lookup vector ranging from DOY 1 : 336
.ESdistance <- readRDS("data-raw/sun_earth_dists.rds")

## Raster data type lookup table
.DATATYPEdb <- read.table(text="datatype min max
				LOG1S 0 1
				INT1S -127 127
				INT1U 0 255
				INT2S 32767 32767
				INT2U 0 65534
				INT4S -2147483647 2147483647
				INT4U 0 4294967296
				FLT4S -3.4e+38 3.4e+38
				FLT8S -1.7e+308 1.7e+308", sep=" ", head=TRUE, row.names="datatype")


## *******************************************************************************************************************
## Landsat auxilliary data. Taken from Chander et al 2009
## spatRes resampling: http://landsat.usgs.gov/band_designations_landsat_satellites.php
## ESUN values according to: https://landsat.usgs.gov/esun
.LANDSATdb <- list(
		LANDSAT1 = list (
				MSS =  data.frame(band = paste0("B", 4:7, "_dn"),
						bandtype   = rep("REF", 4),
						centerWavl = c(0.548, 0.652, 0.747, 0.900),
						spatRes1   = rep(60, 4), ## resampled
						esun = c(1848, 1588, 1235, 856.6) ,  
						stringsAsFactors = FALSE
				)
		),
		
		LANDSAT2 = list (
				MSS =  data.frame(band = paste0("B", 4:7, "_dn"),
						bandtype = rep("REF", 4),
						centerWavl = c(0.548, 0.659, 0.750, 0.899),
						spatRes1 = rep(60, 4), ##resampled
                        esun = c(1848, 1588, 1235, 856.6) ,  
                        stringsAsFactors = FALSE
				)
		),
		
		LANDSAT3 = list (
				MSS =  data.frame(band = paste0("B", 4:7, "_dn"),
						bandtype = rep("REF", 4),
						centerWavl = c(0.545, 0.656, 0.743,	0.896),
						spatRes1 = rep(60, 4), ##resampled
                        esun = c(1848, 1588, 1235, 856.6) ,  
						stringsAsFactors = FALSE
				)
		
		),
		
		LANDSAT4 = list (
                TM = data.frame(band = paste0("B", 1:7, "_dn"),
                        bandtype = c(rep("REF", 5), "TIR", "REF"),
                        centerWavl = c(0.486, 0.569, 0.659, 0.841, 1.676, 11.040, 2.222), ## Chander2009 Tab3
                        spatRes1 = rep(30, 7),
                        spatRes2 = c(rep(30,5), 60, 30), ## TM Band 6 was acquired at 120-meter resolution, but products processed before February 25, 2010 are resampled to 60-meter pixels. Products processed after February 25, 2010 are resampled to 30-meter pixels.
                        esun = c(1958, 1826, 1554, 1033, 214.7, NA, 80.70) ,	
                        stringsAsFactors = FALSE
                ),
				MSS =  data.frame(band = paste0("B", 1:4, "_dn"),
						bandtype = rep("REF", 4),
						centerWavl = c(0.550, 0.650, 0.757,	0.916),
						spatRes1 = rep(60, 4), ##resampled
                        esun = c(1848, 1588, 1235, 856.6) ,  
						stringsAsFactors = FALSE
				)
		
		),
		
		LANDSAT5 = list (
				TM = data.frame(band = paste0("B", 1:7, "_dn"),
						bandtype = c(rep("REF", 5), "TIR", "REF"),
						centerWavl = c(0.485, 0.569, 0.66, 0.840, 1.676, 11.435, 2.223),
						spatRes1 = rep(30, 7),
						spatRes2 = c(rep(30,5), 60, 30), ## TM Band 6 was acquired at 120-meter resolution, but products processed before February 25, 2010 are resampled to 60-meter pixels. Products processed after February 25, 2010 are resampled to 30-meter pixels.
						esun = c(1958, 1827, 1551, 1036, 214.9, NA, 80.65) ,
						stringsAsFactors = FALSE
				),
				MSS =  data.frame(band = paste0("B", 1:4, "_dn"),
						bandtype = rep("REF", 4),
						centerWavl = c(0.552, 0.650,0.759,0.923),
						spatRes1 = rep(60, 4), ##resampled
                        esun = c(1848, 1588, 1235, 856.6) ,  
                        stringsAsFactors = FALSE
				)
		),
		
		LANDSAT7 = list(
				ETM = data.frame(band = paste0("B",c(1:6, "6_VCID_1", "6_VCID_2", 7:8), "_dn"),
						bandtype = c(rep("REF", 5), rep("TIR",3), "REF", "PAN"),
						spatRes1 = c(rep(30, 9), 15),
						spatRes2 = c(rep(30,5), rep(60,3), 30, 15),  ## ETM+ Band 6 is acquired at 60-meter resolution. Products processed after February 25, 2010 are resampled to 30-meter pixels.
						centerWavl = c(0.485, 0.560, 0.660, 0.835, 1.650, rep(11.335,3),2.220,0.706),
						esun = c(1970, 1842, 1547, 1044, 225.7, rep(NA,3), 82.06, 1369),
						stringsAsFactors = FALSE
				)
		),
		
		LANDSAT8 = list(
				OLI_TIRS = data.frame(band = c(paste0("B",1:11, "_dn"), "BQA"),
						bandtype = c(rep("REF", 7), "PAN", "REF", "TIR", "TIR", "QA"),
						spatRes1 = c(rep(30, 7), 15, rep(30,4)),
						spatRes2 = c(rep(30, 7), 15, rep(30,4)),  ## ETM+ Band 6 is acquired at 60-meter resolution. Products processed after February 25, 2010 are resampled to 30-meter pixels.
						centerWavl = c(0.44,0.48,0.56,0.655,0.865,1.61,2.2,0.59,1.37,10.6,11.5, NA), 
						esun = c(1895.57139, 2005.40760, 1820.55317 , 1549.31890,  951.28307, 
								247.93538, 85.87907 ,  367.00629, NA, NA, 1718.38626, NA ),
						stringsAsFactors = FALSE
				)
		)
) 

exponents <- c(-4, -2, -1, -.7, -.5)
for(s in names(.LANDSATdb)){
	for(sen in names(.LANDSATdb[[s]])) {
		bandType		<- .LANDSATdb[[s]][[sen]][,"bandtype"] == "REF"
		centerWavl		<- .LANDSATdb[[s]][[sen]][bandType, "centerWavl"] 
		bands 			<- .LANDSATdb[[s]][[sen]][bandType, "band"]
		
		## Calc Chavez Tab 1
		TAB1			<- sapply(exponents, function(x) centerWavl ^ x)
		rownames(TAB1)  <- bands
		colnames(TAB1)	<- c("veryClear", "clear", "moderate", "hazy", "veryHazy")
		
		## Calc Chavez Tab 2, but only until SHVB = B4, larger wavelengths don't make sense to estimate haze
		hazeBands <- if(s %in% paste0("LANDSAT",1:3)) paste0("B", 4:7, "_dn") else  paste0("B", 1:4, "_dn")
		TAB2 <- lapply(hazeBands, function(SHVB){ sweep(TAB1, 2, TAB1[SHVB,], "/")})
		TAB2 <- do.call("cbind", TAB2)
		colnames(TAB2) <- paste0(rep(paste0("B", 1:4, "_dn"), each = 5),"_", colnames(TAB2))
		
		.LANDSATdb[[s]][[sen]] <-  merge(.LANDSATdb[[s]][[sen]] , TAB2, by.x = "band", by.y = "row.names", all.x = TRUE, sort = FALSE)
		rownames(.LANDSATdb[[s]][[sen]]) <- .LANDSATdb[[s]][[sen]]$band
	}
}



## Tasselled Cap Coefficients
d <- list(NULL, c("brightness", "greenness", "wetness"))
.TCcoefs <- list(
        landsat4tm = matrix(c(
                        # Crist 1985
                        0.2043,  0.4158,  0.5524, 0.5741,  0.3124,  0.2303, 
                        -0.1063, -0.2819, -0.4934, 0.7940, -0.0002, -0.1446,
                        0.0315,  0.2021,  0.3102, 0.1594, -0.6806, -0.6109),
                ncol=3, dimnames = d),
        landsat5tm = matrix( c(
                        # Crist 1985
                        0.2043,  0.4158,  0.5524, 0.5741,  0.3124,  0.2303, 
                        -0.1063, -0.2819, -0.4934, 0.7940, -0.0002, -0.1446,
                        0.0315,  0.2021,  0.3102, 0.1594, -0.6806, -0.6109)
                , ncol = 3, dimnames = d),
        landsat7etm= matrix(c( 
                        # Huang 2002
                        0.3561,  0.3972, 0.3904, 0.6966, 0.2286, 0.1596, 
                        -0.3344, -0.3544,-0.4556, 0.6966,-0.0242,-0.2630,
                        0.2626,  0.2141, 0.0926, 0.0656,-0.7629,-0.5388)
                #       0.0805, -0.0498 ,0.1950,-0.1327, 0.5752,-0.7775, 
                #      -0.7252, -0.0202, 0.6683, 0.0631,-0.1494,-0.0274, 
                #       0.4000, -0.8172, 0.3832, 0.0602,-0.1095, 0.0985 
                , ncol = 3, dimnames = d),          
        landsat8oli= matrix(c(
                        # Baig et al (2014)
                        0.3029, 0.2786, 0.4733, 0.5599, 0.5080, 0.1872,
                        -0.2941,-0.2430,-0.5424, 0.7276, 0.0713,-0.1608,
                        0.1511, 0.1973, 0.3283, 0.3407,-0.7117,-0.4559), ncol = 3, dimnames = d),    
        modis = matrix(c(
                        #Lobser & Cohen (2007) 
                        0.4395, 0.5945, 0.2460, 0.3918, 0.3506, 0.2136, 0.2678, 
                        -0.4064, 0.5129,-0.2744,-0.2893, 0.4882,-0.0036,-0.4169,
                        0.1147, 0.2489, 0.2408, 0.3132,-0.3122,-0.6416,-0.5087), ncol = 3, dimnames = d)
#ikonos = matrix(c(NA)),
#spot = matrix(c(NA)),
#rapideye = matrix(c(NA)),
)





## *******************************************************************************************************************
## Save internal data
save(.ESdistance, .LANDSATdb, .DATATYPEdb,.TCcoefs, file = "R/sysdata.rda", compress = "gzip")
