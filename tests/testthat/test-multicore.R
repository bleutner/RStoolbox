context("Multicore/Singlecore")


test_that("single and multicore rasterFuns return equal results", {
    skip_on_cran() #because hadley says its risky to test paralell code on cran :-)
    r <- raster(ncol=10,nrow=10, vals=1:100)
    r <- stack(r, r^2)
    names(r) <- c("red", "nir")  
    m <- lm(red~nir, data = as.data.frame(r))
    f <- function(a,b){a-b}
    
    calls <- list(
            predict = quote(.paraRasterFun(r, rasterFun = predict, model=m)),
            calc	= quote(.paraRasterFun(r, rasterFun = calc, fun=sum)),
            overlay = quote(.paraRasterFun(r, rasterFun = overlay, fun=f))
    )
    
    ## Run actual calculations and test after that. May not be ideal for reporting but its faster since makCluster happens only once
    sing <- lapply(calls, eval)  
    beginCluster(2, type ="SOCK")
    mult <- lapply(calls, eval)
    endCluster() 
                   
    expect_equal(sing[["predict"]], mult[["predict"]])
    expect_equal(sing[["calc"]], mult[["calc"]])
    expect_equal(sing[["overlay"]], mult[["overlay"]])
    
    ## If tests before passed, then make sure its also equal to the regular raster calls
    expect_equal(sing[["predict"]], predict(r, m))
    expect_equal(sing[["calc"]], calc(r, fun=sum))
    expect_equal(sing[["overlay"]], overlay(r, fun=f))
    
})



