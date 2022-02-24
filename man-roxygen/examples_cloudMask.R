#' @examples   
#' library(raster)
#' library(ggplot2)
#' ## Import Landsat example subset
#' data(lsat) 
#' ## We have two tiny clouds in the east
#' \donttest{ggRGB(lsat, stretch = "lin")}
#' 
#' ## Calculate cloud index
#' cldmsk    <- cloudMask(lsat, blue = 1, tir = 6)
#' \donttest{ggR(cldmsk, 2, geom_raster = TRUE) }
#' 
#' ## Define threshold (re-use the previously calculated index)
#' ## Everything above the threshold is masked
#' ## In addition we apply a region-growing around the core cloud pixels
#' cldmsk_final <- cloudMask(cldmsk, threshold = 0.1, buffer = 5) 
#' 
#' ## Plot cloudmask 
#' \donttest{ggRGB(lsat, stretch = "lin") +
#'    ggR(cldmsk_final[[1]], ggLayer = TRUE, forceCat = TRUE, geom_raster = TRUE) +
#'    scale_fill_manual(values = c("red"), na.value = NA)
#' }
#' #' ## Estimate cloud shadow displacement
#' ## Interactively (click on cloud pixels and the corresponding shadow pixels)
#' \dontrun{ shadow <- cloudShadowMask(lsat, cldmsk_final, nc = 2) }
#' 
#' ## Non-interactively. Pre-defined shadow displacement estimate (shiftEstimate)
#' \donttest{shadow <- cloudShadowMask(lsat, cldmsk_final, shiftEstimate = c(-16,-6))}
#'
#' ## Plot
#' \donttest{csmask <- raster::merge(cldmsk_final[[1]], shadow)
#' ggRGB(lsat, stretch = "lin") +
#'         ggR(csmask, ggLayer = TRUE, forceCat = TRUE, geom_raster = TRUE) +
#'         scale_fill_manual(values = c("blue", "yellow"), 
#'         labels = c("shadow", "cloud"), na.value = NA)
#' }
