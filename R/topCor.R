#' Topographic Illumination Correction
#' 
#' account and correct for changes in illumination due to terrain elevation.
#' 
#' @param img Raster* object or SpatRaster. Imagery to correct
#' @param dem Raster* object or SpatRaster. Either a digital elevation model as a RasterLayer or a RasterStack/Brick with pre-calculated slope and aspect (see \link[raster]{terrain}) in which case the layers must be named 'slope' and 'aspect'.
#' Must have the same dimensions as \code{img}.
#' @param metaData Character, ImageMetaData. Either a path to a Landsat meta-data file (MTL) or an ImageMetaData object (see \link{readMeta}) 
#' @param solarAngles Numeric vector containing sun azimuth and sun zenith (in radians and in that order). Not needed if metaData is provided   
#' @param method Character. One of c("cos", "avgcos", "minnaert", "C", "stat", "illu"). Choosing 'illu' will return only the local illumination map.
#' @param stratImg RasterLayer or SpatRaster to define strata, e.g. NDVI. Or the string 'slope' in which case stratification will be on \code{nStrat} slope classes. Only relevant if \code{method = 'minnaert'}.
#' @param nStrat Integer. Number of bins or quantiles to stratify by. If a bin has less than 50 samples it will be merged with the next bin. Only relevant if \code{method = 'minnaert'}.
#' @param illu Raster* object or SpatRaster. Optional pre-calculated ilumination map. Run topCor with method="illu" to calculate an ilumination map
#' @param ... arguments passed to \code{\link[raster]{writeRaster}}
#' @details
#' For detailed discussion of the various approaches please see Riano et al. (2003).
#' 
#' The minnaert correction can be stratified for different landcover characteristics. If \code{stratImg = 'slope'} the analysis is stratified by the slope, 
#' i.e. the slope values are divided into \code{nStrat} classes and the correction coefficient k is calculated and applied separately for each slope class.
#' An alternative could be to stratify by a vegetation index in which case an additional raster layer has to be provided via the \code{stratImg} argument.
#' @export
#' @returns SpatRaster
#' @references 
#' Riano et al. (2003) Assessment of different topographic correction in Landsat-TM data for mapping vegetation types. IEEE Transactions on Geoscience and Remote Sensing.
#' @examples 
#' ## Load example data
#' metaData <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")
#' metaData <- readMeta(metaData)
#' lsat     <- stackMeta(metaData)
#' 
#' ## Minnaert correction, solar angles from metaData
#' lsat_minnaert <- topCor(lsat, dem = srtm, metaData = metaData, method = "minnaert")
#' 
#' ## C correction, solar angles provided manually
#' lsat_C <- topCor(lsat, dem = srtm, solarAngles = c(1.081533, 0.7023922), method = "C")
#' 
topCor <- function(img, dem, metaData, solarAngles = c(), method = "C", stratImg = NULL, nStrat = 5, illu, ...){
    img_t <- .toTerra(img)
    if(!missing("dem")) dem_t <- .toTerra(dem)
	if(!missing("illu")) illu_t <- .toTerra(illu)
	
	
    stopifnot(method %in% c("cos", "avgcos", "minnaert", "C", "stat", "illu"))
    ## TODO: improve performance
    ## Metadata 
    if(!missing("solarAngles")) {
        if(length(solarAngles)!=2) stop ("If metaData is used to provide solar azimuth and solar zenith it must be a numeric vector of length 2: c(azimuth, zenith)")
        sa <- solarAngles[1]
        sz <- solarAngles[2]
    } else { 
        if(missing("metaData")) stop("You must specify either solarAngles or metaData")
        if(inherits(metaData, "character"))   metaData <- readMeta(metaData)
        sz <- (90-metaData$SOLAR_PARAMETERS[2])*pi/180
        sa <-  metaData$SOLAR_PARAMETERS[1]*pi/180        
    }  
    
    ## Terrain
    if(any(!names(dem) %in% c("slope", "aspect"))) {
        .vMessage("Calculate slope and aspect")
        topo_t <- terrain(dem_t, c("slope", "aspect"), unit = "radians")
    } else {
        topo_t <- dem_t
        .vMessage("Using pre-calculated slope and aspect")
    }

    slope_t <- topo_t[["slope"]]

    ## Illumination
    if(missing(illu)){
        .vMessage("Calculate illumination map")
        illu_t_func <- function(slope, aspect, sazimuth = sa, szenith = sz){
            cos(szenith) * cos(slope) + sin(szenith) * sin(slope) * cos(sazimuth - aspect)
        }
        illu_t <- illu_t_func(topo_t$slope, topo_t$aspect)

        names(illu_t) <- "illu"
    } else {
        .vMessage("Using pre-calculated illumination map")
    }
    if(method=="illu")
      return(illu_t)
    
    .vMessage("Correct imagery")
    if (method == "cos") {
        ## valid range: <55 degree
        ## Eq 3 in Riano2003
        ## Lambertian assumption
        Lh_t_func <- function(x,y){x * (cos(sz) / y)}
        Lh_t <- Lh_t_func(img_t, illu_t)
    }
    if (method == "avgcos") {
        ## Eq 4 in Riano2003
        ## Lambertian assumption
        avgillu_t <- as.numeric(t(terra::global(illu_t, "mean", na.rm = TRUE)))

        Lh_t_func <- function(x,y){ x + x * (avgillu_t-y) / avgillu_t}
        Lh_t <- Lh_t_func(img_t, illu_t)

    }
    if(method =="minnaert") {
        ## Eq 5 in Riano2003
        ## Lambertian assumption if k == 1
        ## Non-lambertian if 0 <= k < 1   
        stratMethod <- if(is.null(stratImg)) {stratImg <- "slope"; "noStrat"} else "stratEqualBins"
        ks_t <- .kestimate_t(img_t, illu_t, slope_t, method = stratMethod, stratImg = stratImg, n = nStrat, sz=sz)

        ks_t$k <- lapply(ks_t$k, function(x){
            x[x[,2] < 0, 2] <- 0
            x[x[,2] > 1, 2] <- 1
            x
        })

        Lh_t <- lapply(1:nlyr(img_t), function(i){
            Lh_t_func <- function(img, illu, strat, groups = ks_t$groups, k = ks_t$k) {
                sc <- base::cut(as.numeric(values(strat)), breaks = groups, labels = FALSE)
                k <- k[[i]][sc,2]
                return(as.numeric(values(img)) * cos(sz)/ as.numeric(values(illu))^k)
            }
            Lh_t_vals <- Lh_t_func(img_t[[i]], illu_t, slope_t)
            Lh_t_img <- img_t[[i]]
            values(Lh_t_img) <- Lh_t_vals
            return(Lh_t_img)
        })

        ellip <- list(...)
        if ('filename' %in% names(ellip) && !is.null(ellip[["filename"]])) {
            names(Lh_t) <- names(img_t)
            Lh_t <- writeRaster(Lh_t, ...)
        }
    }    
    if(method ==  "stat") {
        ## Eq 8 in Riano2003
        ks_t <- .kestimate_t(img_t, illu_t, slope_t, method = "stat")
        sub_t <- rast(lapply(ks_t$k, function(x){
            x[,2] * illu_t
        }))

        Lh_t <- img_t - sub_t

    }
    if(method == "C") {
        ks_t <- .kestimate_t(img_t, illu_t, slope_t, method = "stat")
        mult_t <- rast(lapply(ks_t$k, function(x){
            ck <- x[,1]/x[,2]
            (cos(sz) + ck) /  (illu_t + ck)
        }))
        Lh_t <- img_t * mult_t

    }

    names(Lh_t) <- names(img_t)
    return(Lh_t)
#    if(FALSE && method == "minnaMod"){
#        ## Richter 2009
#        if(sz < 45*pi/180) {
#            beta_t <- sz + 20*pi/180
#        } else if(sz > 55*pi/180) {
#            beta_t <- sz + 10*pi/180        
#        } else {
#            beta_t <- sz + 15*pi/180
#        }
#        
#        ## Vegetation classes: 1 = non-veg, 2 = veg
#        bvis = c(0.5,  ## non-vegetation
#                3/4)  ## vegetation (wavelength < 720nm)
#        bir = c(0.5,  ## non-vegetation
#                1/3)  ## vegetation (wavelength > 720nm)
#   
#        minnaMod <- function(x, beta_tx, b_lut) {
#            b      <- b_lut[x[,2]]
#            mult <- (x[,1]/beta_tx)^b
#            mult[mult > 0.25] <- 0.25
#            mult
#        }
#        
#     multvis <- calc(stack(illu, stratImg), fun = function(x) minnaMod(x, b_lut = bvis, beta_tx = beta_t))
#     multir  <- calc(stack(illu, stratImg), fun = function(x) minnaMod(x, b_lut = bir, beta_tx = beta_t))
#      
#     select <- illu > beta_t 
#     visBands <- 1:4    
#     visCo <- img[visBands]
#     visCo[select] <- img[[visBands]][select]  * multvis[select]
#    }
        
}

#' Parameter estimation
#' @noRd 
#' @keywords internal
.kestimate_t <- function(img, illu, slope, stratImg = "slope", method = "noStrat", n = 5, minN = 50, sz) {
    suppressWarnings({
        stopifnot(method %in% c("stat", "noStrat", "stratEqualBins", "stratQuantiles"))
        ## Following Lu 2008 sample pre selection
        set.seed(10)
        strat <- if(inherits(stratImg, "character")) NULL else {names(stratImg) <- "strat"; stratImg}
        sr       <- as.data.frame(spatSample(c(img, illu, slope, strat), size = min(ncell(img), 10000), na.rm=TRUE))

        if(method != "stat") sr  <- sr[sr$slope > 2*pi/180 & sr$illu >= 0,]
        if(method != "noStrat" & inherits(stratImg, "character")) {
            sr$strat <- sr[,stratImg]
            stratImg <- slope
        }

        if(method %in% c("stat","noStrat")){
            groups <- 0:1
            assoc <- rep(1, length(nrow(sr)))
        } else {
            .vMessage("Begin strafification")
            if(method == "stratQuantiles") {
                ## Quantile method
                groups <- quantile(stratImg, probs = 0:n/n)
            }
            if(method == "stratEqualBins") {
                ## Equal method
                mi <- min(values(stratImg), na.rm = TRUE)
                ma <- max(values(stratImg), na.rm = TRUE)
                groups <- seq(mi, ma, by = (ma - mi)/n)
            }
            assoc  <- cut(sr$strat, breaks = groups, labels = FALSE, include.lowest = TRUE)
            gMax   <- tail(groups, 1)
            gMin   <- groups[1]
            tab    <- tabulate(assoc, nbins = (length(groups)-1))
            tooFew <- which(tab < minN) + 1
            while(length(tooFew)){
                tooFew[tooFew == 1] <- 2
                tooFew[tooFew == length(groups)] <- length(groups) -1
                groups <- groups[-tooFew]
                groups <- unique(c(gMin, groups, gMax))
                assoc  <- cut(sr$strat, breaks = groups, labels = FALSE, include.lowest = TRUE)
                tab    <- tabulate(assoc, nbins = (length(groups)-1))
                tooFew <- which(tab < minN) + 1
            }
        }
        .vMessage("Estimate coefficients")
        x     <- if(method == "stat") sr$illu else log(sr$illu/cos(sz))
        kl <- lapply(1:nlyr(img), function(i){
                    if(method == "stat") {
                        y <- sr[,i]
                    } else {
                        stz <- sr[,i] < 0

                        if(any(stz)) {
                            warning("Resetting negative reflectances to zero!", call.=FALSE)
                            sr[stz,i] <- 1e-32
                        }
                        sr[sr[,i] == 0,i] <- 1e-32
                        y <- log(sr[,i])
                    }
                    k <- lapply(1:(length(groups)-1), function(g){
                                select <- assoc == g
                                mod <- lm(y[select] ~ x[select])
                                k <- coefficients(mod)
                            })
                    do.call("rbind", k)
                })
            return(list(groups = groups, k = kl))
    })
}
