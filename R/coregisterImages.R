#' Image to Image Co-Registration based on Mutual Information
#' 
#' Shifts an image to match a reference image. Matching is based on maximum
#' mutual information. 
#' 
#' @param img Raster* object or SpatRaster. Image to shift to match reference image. \code{img} and \code{ref} must have equal numbers of bands.
#' @param ref Raster* object or SpatRaster. Reference image. \code{img} and \code{ref} must have equal numbers of bands.
#' @param shift Numeric or matrix. If numeric, then shift is the maximal absolute radius (in pixels of \code{img} resolution) which \code{img} is shifted (\code{seq(-shift, shift, by=shiftInc)}). 
#'  If shift is a matrix it must have two columns (x shift and y shift), then only these shift values will be tested.
#' @param shiftInc Numeric. Shift increment (in pixels, but not restricted to integer). Ignored if \code{shift} is a matrix.
#' @param nSamples Integer. Number of samples to calculate mutual information. 
#' @param nBins Integer. Number of bins to calculate joint histogram.
#' @param reportStats Logical. If \code{FALSE} it will return only the shifted images. Otherwise it will return the shifted image in a list containing stats such as mutual information per shift and joint histograms.
#' @param verbose Logical. Print status messages. Overrides global RStoolbox.verbose option.
#' @param ... further arguments passed to \code{\link[raster]{writeRaster}}.
#' @param slave DEPRECATED! Argument was renamed. Please use \code{img} from now on.
#' @param master DEPRECATED! Argument was renamed. Please use \code{ref} from now on.
#' @details 
#' Currently only a simple linear x - y shift is considered and tested. No higher order shifts (e.g. rotation, non-linear transformation) are performed. This means that your imagery
#' should already be properly geometrically corrected.
#' 
#' \href{https://en.wikipedia.org/wiki/Mutual_information}{Mutual information} is a similarity metric originating from information theory.
#' Roughly speaking, the higher the mutual information of two data-sets, the higher is their shared information content, i.e. their similarity.
#' When two images are exactly co-registered their mutual information is maximal. By trying different image shifts, we aim to find the best overlap which maximises the mutual information.
#' @return 
#' \code{reportStats=FALSE} returns a SpatRaster (x-y shifted image).
#' \code{reportStats=TRUE} returns a list containing a data.frame with mutual information per shift ($MI), the shift of maximum MI ($bestShift),
#' the joint histograms per shift in a list ($jointHist) and the shifted image ($coregImg). 
#' @export 
#' @examples 
#' library(terra)
#' library(ggplot2)
#' library(reshape2)
#' reference <- rlogo_rs
#' ## Shift reference 2 pixels to the right and 3 up
#' missreg <- shift(reference,  2,  3)
#'
#'## Compare shift
#'p <- ggR(reference, sat = 1, alpha = .5) 
#'p + ggR(missreg, sat = 1, hue = .5, alpha = 0.5, ggLayer=TRUE) 
#'
#'## Coregister images (and report statistics)
#'coreg <- coregisterImages(missreg, ref = reference,
#'                          nSamples = 500, reportStats = TRUE)
#'
#'## Plot mutual information per shift
#'ggplot(coreg$MI) + geom_raster(aes(x,y,fill=mi))
#'
#'## Plot joint histograms per shift (x/y shift in facet labels)
#'\donttest{ 
#'df <- melt(coreg$jointHist)   
#'df$L1 <- factor(df$L1, levels = names(coreg$jointHist))
#'df[df$value == 0, "value"] <- NA ## don't display p = 0
#'ggplot(df) + geom_raster(aes(x = Var2, y = Var1,fill=value)) + facet_wrap(~L1) + 
#'        scale_fill_gradientn(name = "p", colours =  heat.colors(10), na.value = NA)
#'}
#'## Compare correction
#'ggR(reference, sat = 1, alpha = .5) +
#'   ggR(coreg$coregImg, sat = 1, hue = .5, alpha = 0.5, ggLayer=TRUE) 
coregisterImages <- function(img, ref, shift = 3, shiftInc = 1, nSamples = 100,
        reportStats = FALSE, verbose, nBins = 100, 
        master = deprecated(), slave = deprecated(),
        ...) {
    
    if(is_present(slave)){
        deprecate_warn("0.3.0", "coregisterImages(slave)", "coregisterImages(img)")
    }
    if(is_present(master)){
        deprecate_warn("0.3.0", "coregisterImages(master)", "coregisterImages(ref)")
    }

    img <- .toTerra(img)
	ref <- .toTerra(ref)
	
    ## TODO: allow user selected pseudo control points
    ## TODO: add computation of MI to docu
    #if(!swin%%2 | !mwin%%2) stop("swin and mwin must be odd numbers")
    if(!missing("verbose")) .initVerbose(verbose)
    if(!crs(img) == crs(ref)) stop("Projection must be the same for ref and img")
    nSamples <- min(nSamples, ncell(img))
    
    if(inherits(shift, "matrix") && ncol(shift) == 2) {
        shifts <- shift * res(img)
    } else {
        shift <- seq(0, shift, shiftInc)
        shift <- c(-rev(shift), shift[-1]) ## always include zero shift
        shifts <- expand.grid(shift * res(img)[1], shift * res(img)[2])
    }
    names(shifts) <- c("x", "y")

    ran   <- apply(shifts, 2, range)
    minex <- ext(shift(img, ran[1,1], ran[1,2]))
    maxex <- ext(shift(img, ran[2,1], ran[2,2]))

    XYimgs <- data.matrix(
      terra::spatSample(ref, size = nSamples, ext = .getExtentOverlap(minex, maxex) * .9, xy = TRUE)
    )

    mmin <- min(values(ref))
    mmax <- max(values(ref))
    smin <- min(values(img))
    smax <- max(values(img))

    xy <- XYimgs[,c(1,2)]
    me <- XYimgs[,-c(1,2)]

    mbreax <- seq(mmin, mmax, by = (mmax - mmin)/nBins)
    sbreax <- seq(smin, smax, by = (smax - smin)/nBins)

    me <- cut(me, breaks = mbreax, labels = FALSE, include.lowest = TRUE)

    nsl <- ncol(img)
    nml <- ncol(ref)

    if(nsl !=  nml)  stop("Currently img and ref must have the same number of layers")         
    
    shiftPts <- function(o, x, y) {
        o[,"x"] <- o[,"x"] + x
        o[,"y"] <- o[,"y"] + y
        o
    }
    
    spts <- .parXapply(X = 1:nrow(shifts), XFUN = "lapply", FUN = function(i){
                xt <- shiftPts(xy, x = -shifts[i,1], y = -shifts[i,2])
                cellFromXY(img, xt)
            }, envir=environment())

    ucells <- sort(unique(unlist(spts)))
    lut <- as.matrix(img[ucells])
    rownames(lut) <- ucells
    spts <- lapply(spts, as.character)

    ## Shift and calculate mutual information
    sh <- .parXapply(X = 1:nrow(shifts), XFUN = "lapply", FUN = function(i){
                se <- lut[spts[[i]], ]
                se  <- cut(as.numeric(se), breaks = sbreax, labels = FALSE, include.lowest = TRUE)
                
                pab  <- table(me,se)
                pab  <- pab/sum(pab)
                
                pa   <- colSums(pab)
                pb   <- rowSums(pab)
                
                pabx <- pab[pab>0]    ## we can do this because lim(0 * log(0)) = 0.
                pb   <- pb[pb>0] 
                pa   <- pa[pa>0]
                
                hab  <- sum(-pabx * log(pabx))
                ha   <- sum(-pa * log(pa))
                hb   <- sum(-pb * log(pb))
                mi   <- ha + hb -hab
                list(mi = mi, joint = pab)
                
            }, envir = environment() )

    ## Aggregate stats
    mi <- vapply(sh,"[[", i = 1, numeric(1))

    if(reportStats){
        jh <- lapply(sh, function(x) matrix(as.vector(x$joint), nrow=nrow(x$joint), ncol=ncol(x$joint)))   
        names(jh) <- paste(shifts[,"x"], shifts[,"y"], sep = "/")
    }
    ## Find best shift and shift if doShift
    moveIt <- shifts[which.max(mi),]
    .vMessage("Identified shift in map units (x/y): ", paste(moveIt, collapse="/"))

    moved <- shift(img, as.numeric(moveIt[1]), as.numeric(moveIt[2]), ...)

    if(reportStats) {
        return(list(MI = data.frame(shifts, mi=mi), jointHist = jh, bestShift = moveIt, coregImg = moved))
    } else {
        return(moved)
    }

}

#    if(method == "areaCor"){
#        mwin = 11, swin = 3, regbands = 3, 
#    
#    if(length(regbands) == 1) regbands <- rep(regbands,2)
#    mstr <- master[[regbands[1]]]  
#    slv  <- slave[[regbands[2]]]
#    XYslaves <- sampleRegular(slv, size = n, xy = TRUE, cells =T)
#    nbrs <- matrix(c(rep(1,0.5*swin^2), 0, rep(1,0.5*swin^2)), ncol = swin, nrow = swin)  
#    sVals <- lapply(XYslaves[,"cell"], function(x) {
#                slv[adjacent(slv, x, directions = nbrs, pairs = FALSE, include = TRUE)]
#            }) 
#    nbrs[] <- 1   
#    #nbrs <- matrix(c(rep(1,0.5*mwin^2), 0, rep(1,0.5*mwin^2)), ncol = mwin, nrow = mwin) 
#    XYmaster <- lapply(1:nrow(XYslaves), function(i) {
#                d <-  XYslaves[i, c("x", "y")]  
#                r <- res(mstr)[1]
#                e <- extent(c(d - r * mwin/2, d + r*mwin/2)[c(1,3,2,4)])
#                masterc <- crop(mstr, e)
#                #  masterf <- focal(masterc, w=nbrs, fun = function(x) {sum((x - sVals[[i]])^2)})
#                #  masterf <- focal(masterc, w=nbrs, fun = function(x) {cor(x,sVals[[i]])})
#                masterf <- focal(masterc, w=nbrs, fun=function(x){sum(x*sVals[[i]])/sum(abs(x))})
#                mcell <- which.max(masterf)
#                cbind(xyFromCell(masterf, mcell), cor = masterf[mcell])
#                
#            })
#    minCor <- 0.9
#    XYmaster <- do.call("rbind", XYmaster)
#    da <- data.frame(s=XYslaves[,c("x","y")], m=XYmaster)
#    da <- da[da[,"m.cor"] > minCor,] 
#    ggplot()+geom_segment(data = da, aes(x = s.x, xend = m.x, y=s.y, yend=m.y), arrow = arrow(length = unit(0.4,"cm")))
#}

