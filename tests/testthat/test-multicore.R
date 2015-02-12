context("Multicore/Singlecore")


test_that(".paraRasterFun is equal to predict, calc, overlay, Both single and multicore.", {
            skip_on_cran() # hadley says its risky to test paralell code on cran :-)
            
            r <- raster(ncol=10,nrow=10, vals=1:100)
            r <- stack(r, r^2)
            names(r) <- c("red", "nir")  
            m <- lm(red~nir, data = as.data.frame(r))
            f <- function(a,b){a-b}            
            
            beginCluster(2, type = "SOCK")
            cluster <- "multicore"
            for(i in 1:2){
                expect_identical(.paraRasterFun(r, rasterFun = predict, model = m), predict(r, m), label = paste("predict:", cluster))
                expect_identical(.paraRasterFun(r, rasterFun = calc, fun = sum), calc(r, fun = sum), label = paste("calc:", cluster))
                expect_identical(.paraRasterFun(r, rasterFun = overlay, fun = f), overlay(r, fun = f), label = paste("overlay:", cluster))
                endCluster()
                cluster <- "singlecore"
            }
            
        })


test_that(".parXapply family returns identical results to ?pply family. Both single and multicore.", {
            skip_on_cran()
            lis <- lapply(1:5, rnorm)
            mat <- matrix(1:100,10,10)
            beginCluster(2, type = "SOCK")
            cluster <- "multicore"
            for(i in 1:2){
                expect_identical(.parXapply(X = lis, XFUN ="lapply", FUN=sum), lapply(lis, sum), label = paste("lapply:", cluster))
                expect_identical(.parXapply(X = lis, XFUN ="sapply", FUN=sum), sapply(lis, sum), label = paste("sapply:", cluster))
                expect_identical(.parXapply(X = mat, XFUN ="apply", 1, FUN=sum), apply(mat, MAR =1, sum), label = paste("aapply:", cluster))        
                endCluster()
                cluster <- "singlecore"
            }
        })  





