library(testthat)
library(RStoolbox)

test_check("Parallel functions")


r <- raster(ncol=10,nrow=10)
r[] <- sample(1:155, 100, TRUE)
r <- stack(r, r + 90 + rnorm(100, 10))
names(r) <- c("red", "nir")

rd <- as.data.frame(r)
m <- lm(red~nir, data = rd)
f <- function(a,b){a-b}

## Check 
callList <- list(
        predict = quote(.paraRasterFun(r, rasterFun = predict, model=m, na.rm=T)),
        calc	= quote(.paraRasterFun(r, rasterFun = calc, na.rm=T, fun=sum)),
        overlay = quote(.paraRasterFun(r, rasterFun = overlay, na.rm=T, fun=f))
)

lapply(callList, function(x){
            print(x)
            single <- eval(x)
            beginCluster(4, type ="SOCK")
            multi <- eval(x)
            endCluster() 
            cat("all.equal: ")
            all.equal(single, multi)
        })

