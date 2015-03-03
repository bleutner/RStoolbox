
panSharp <- function(img, pan, method = "pca", norm=TRUE) {
    if(method == "simple") {
        imgHiRes <- raster::resample(img, pan, method = "ngb")
        out <- .paraRasterFun(imgHiRes, rasterFun=overlay, args = list(y = pan, fun ="/"))
    }
    if(method == "pca") {
        imgpca <- rasterPCA(img)
        
        imgpcaHiRes <- raster::resample(imgpca$map, pan, method = "ngb")
        
        if(norm) {
            panMa <- normImage(pan, imgpca$map[[1]])
        }else{
            panMa <- histMatch(pan, imgpca$map[[1]])
        }
        imgpcaHiRes[[1]] <- panMa
        eigen <- t(loadings(imgpca$model))
        cents <- imgpca$model$center
        panimg <- .paraRasterFun(imgpcaHiRes, rasterFun = calc, args = list(fun = function(x) { x %*%  eigen  +  cents}))
        
        names(panimg) <- paste0(names(img), "_pan")
        return(panimg)
    } else {
        img <- normImage(img, xmin = 0, xmax = 2^16/2, ymin=0, ymax=1)
        imgSum <- .paraRasterFun(img[[c(r,g,b)]],rasterFun = calc, args = list(fun = sum))
        imgNorm <- .paraRasterFun(img[[c(r,g,b)]], rasterFun = overlay, args = list(y = imgSum, fun = "/"))
        .H <- function(r, g, b) { 
            out <- acos((r - g/2 - b/2) /  sqrt((r-g)^2 + (r-b)*(g-b))) 
            m1 <- b > g
            out[m1] <- 2*pi - out[m1] 
            out           
        }
        H <- .paraRasterFun(imgNorm, rasterFun = overlay, args = list(fun =.H))
        # H <- H * 180/pi
        S <- 1 - 3 * .paraRasterFun(imgNorm, rasterFun = calc, args=list(fun=min))
        I <- imgSum / 3    
        
        ## reverse
        hsi2rgb <- function(H, S, I){
            rr1 <- numeric(length(H))
            case1 <- H < 2*pi/3
            case3 <- H >= 4*pi/3
            case2 <- !(case1 | case3)
            
            rr1[case1] <-  1/3 * (1 + (S[case1] * cos(H[case1]))/cos(pi/3-H[case1]))
            rr1[case2] <-  1/3 * (1 + (S[case2] * cos(H[case2] - 2*pi/3))/cos(pi-H[case2]))
            rr1[case3] <-  1/3 * (1 + (S[case3] * cos(H[case3] - 4*pi/3))/cos(5*pi/3-H[case3]))
            
            rr2 <- (1 - S) / 3
            rr3 <-  1 - rr2 - rr1
            
            out <- matrix(ncol=3, nrow=length(H))
            colnames(out) <- c("r", "g", "b")
            
            out[case1, ] <- cbind(rr1[case1], rr3[case1], rr2[case1])
            out[case2, ] <- cbind(rr2[case2], rr1[case2], rr3[case2])
            out[case3, ] <- cbind(rr3[case3], rr2[case3], rr1[case3])
            
            return(out)         
        }
        
        
        rgb <- raster::overlay(stack(H,S,I), fun = hsi2rgb)
        
        hsv <- overlay(img[[c(r,g,b)]], fun = rgb2hsv)
        rgbb <- overlay(hsv, fun=function(x,y,z) t(col2rgb(hsv(x,y,z))))    
        
        
        HSV <- overlay(imgNorm, fun=rgb2hsv, max = 32767)
        
        M <-  matrix(c(rep(1/3,3), rep(sqrt(6)^-1,2), -2/sqrt(6), 1/sqrt(2), -1/sqrt(2), 0), ncol=3, byrow = T)
        Mr <- t(M)
        Mr[,1] <- 1 
        
        Ivv <- M %*% t(img[[c(r,g,b)]][1:10])
        Ivv <- calc(img[[c(r,g,b)]], fun = function(x) t(M %*% t(x)))
        
        Ivvr <- raster::resample(Ivv[[2:3]], pan, method = "bilinear")
        panMa <- histMatch(pan, Ivv[[1]])
        RGB <- calc(stack(panMa, Ivvr), fun = function(x) t(Mr %*% t(x)))
        
        RGB <- t(Mr %*% Ivv)
        plot(imgSum)
    }
    
}            


normImage <- function(x, y, xmin, xmax, ymin, ymax, forceMinMax = FALSE) {
    if(forceMinMax) {
        x <- setMinMax(x)
        if(!missing(y))  y <- setMinMax(y)
    }
    if(missing("ymin")) ymin <- y@data@min
    if(missing("ymax")) ymax <- y@data@max
    if(missing("xmin"))	xmin <- x@data@min 
    if(missing("xmax")) xmax <- x@data@max
    scal <- (ymax - ymin)/(xmax-xmin) 
    .paraRasterFun(x, rasterFun = calc,  args = list(fun = function(x) {(x - xmin) * scal + ymin}))      
}              