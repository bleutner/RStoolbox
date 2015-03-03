#' Image to Image Registration based on Mutual Information
#' 
#' Shifts a slave image to match the reference image (master). Match is based on maximum
#' mutual information.
#' 
#' @param slave Raster* object. Slave image to shift to master.
#' @param master Raster* object. Reference image.
#' @param shift Numeric or matrix. If numeric, then shift is the maximal absolute radius (in pixels) which \code{slave} is shifted (\code{seq(-shift, shift, by=shiftInc)}). 
#'  If shift is a matrix it must have two columns (x shift an y shift), then only these shift values will be tested.
#' @param shiftInc Numeric. Shift increment (in pixels, but not restricted to integer). Ignored if \code{shift} is a matrix.
#' @param n Integer. Number of samples to calculate mutual information.
#' @export 
registerImages <- function(slave, master, shift = 3, shiftInc = 1, n = 500) {
    #if(!swin%%2 | !mwin%%2) stop("swin and mwin must be odd numbers")
    method <- "mi" ## method choice disabled, currently only mi is ready
    
    if(method == "mi") {
        
        if(inherits(shift, "matrix") && ncol(shift)  == 2) { 
            shifts <- shift * res(master) 
        } else {
            shift <- seq(-shift, shift, shiftInc)  
            shifts <- expand.grid(shift * res(master)[1], shift * res(master)[2])
        } 
        names(shifts) <- c("x", "y")        
                
        XYslaves <- sampleRandom(master, size = n, xy = TRUE)
        xy <- XYslaves[,c(1,2)]
        me <- XYslaves[,-c(1,2)]
        ta <- table(me)
        p  <- ta/sum(ta)
        HA <- -sum(p*log(p))
        

        sh <- lapply(1:nrow(shifts), function(i){
                    se  <- extract(shift(slave, shifts[i,1], shifts[i,2]), xy)
                    tb  <- table(se)
                    p   <- tb/sum(tb)
                    HB  <- - sum(p*log(p))
                    tab <- table(c(me,se))
                    p   <- tab/sum(tab)
                    HAB <-  - sum(p*log(p))     
                    if(i == 1) j <<- HA
                    MI  <- HA + HB - HAB              
                } )
        .vMessage("Corrected shift in map units (x/y): ", paste(shifts[which.max(sh),], collapse="/"))
        d<-data.frame(shifts, mi=unlist(sh))
        print(ggplot(d) + geom_raster(aes(x,y,fill = mi)))
        slaveShifted <- shift(slave, shifts[which.max(sh),])
        return(slaveShifted)
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


}
