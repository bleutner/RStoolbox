context("Multicore/Singlecore")
library(terra)


test_that(".paraRasterFun is equal to predict, calc, overlay, Both single and multicore.", {
    skip_on_cran()
    for (clusterType in c('PSOCK', 'FORK')){
        if(Sys.info()[["sysname"]] != "Linux" && clusterType == "FORK") next
        r <- rast(ncol=10,nrow=10, vals=1:100)
        r <- c(r, r^2)
        names(r) <- c("red", "nir")
        m <- lm(red~nir, data = as.data.frame(r))
        f <- function(a,b){a-b}

        for(i in 1:2){
          expect_equal(.paraRasterFun(r, rasterFun = terra::predict, args = list(model = m)), terra::predict(r, m), label = paste("predict:", "singlecore"))
          expect_equal(.paraRasterFun(r, rasterFun = app, args = list(fun = sum)), app(r, fun = sum), label = paste("calc:", "singlecore"))
        }
    }
})


test_that(".parXapply family returns identical results to ?pply family. Both single and multicore.", {
    skip_on_cran()
    for (clusterType in c('PSOCK', 'FORK')){
      if(Sys.info()[["sysname"]] != "Linux" && clusterType == "FORK") next
      lis <- lapply(1:5, rnorm)
      mat <- matrix(1:100,10,10)
      for(i in 1:2){
          expect_equal(.parXapply(X = lis, XFUN ="lapply", FUN=sum, envir = environment()), lapply(lis, sum), label = paste("lapply:", "singlecore"))
          expect_equal(.parXapply(X = lis, XFUN ="sapply", FUN=sum, envir = environment()), sapply(lis, sum), label = paste("sapply:", "singlecore"))
          expect_equal(.parXapply(X = mat, XFUN ="apply", 1, FUN=sum, envir = environment()), apply(mat, MAR =1, sum), label = paste("aapply:", "singlecore"))
      }
    }
})




