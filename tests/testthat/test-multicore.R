context("Multicore/Singlecore")


test_that("single and multicore rasterFuns return equal results", {
    r <- raster(ncol=10,nrow=10, vals=1:100)
    r <- stack(r, r^2)
    names(r) <- c("red", "nir")  
    m <- lm(red~nir, data = as.data.frame(r))
    f <- function(a,b){a-b}
    
    calls <- list(
            predict = quote(.paraRasterFun(r, rasterFun = predict, model=m, na.rm=TRUE)),
            calc	= quote(.paraRasterFun(r, rasterFun = calc, na.rm=TRUE, fun=sum)),
            overlay = quote(.paraRasterFun(r, rasterFun = overlay, na.rm=TRUE, fun=f))
    )
     
    multEval <-  function(x){
                beginCluster(2, type ="SOCK")
                mult <- eval(x)
                endCluster() 
                mult             
            }
    
    expect_equal(eval(calls[["predict"]]), multEval(calls[["predict"]]))
    expect_equal(eval(calls[["calc"]]), multEval(calls[["calc"]]))
    expect_equal(eval(calls[["overlay"]]), multEval(calls[["overlay"]]))
    
    ## If tests before passed, then make sure its also equal to the regular raster calls
    expect_equal(eval(calls[["predict"]]), predict(r, m, na.rm=TRUE))
    expect_equal(eval(calls[["calc"]]), calc(r, sum, na.rm=TRUE))
    expect_equal(eval(calls[["overlay"]]), overlay(r, fun=f, na.rm=TRUE))
    
})



