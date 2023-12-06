context("topCor")
suppressPackageStartupMessages(library(terra))
metaData <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")
metaData <- readMeta(metaData)
lsat     <- stackMeta(metaData) 
srtm <- srtm_rs


## Minnaert correction, solar angles from metaData
test_that("basic functioning", { 
    suppressWarnings({
      mths <-  if (identical(Sys.getenv("NOT_CRAN"), "true")) c("cos", "avgcos", "C", "stat", "illu")  else "cos"
      for(method in mths){
          expect_is(tc <- topCor(lsat, dem = srtm, metaData = metaData, method = method), "SpatRaster")
          expect_equal(names(tc),  if(method!="illu") names(lsat) else "illu")
      }
      skip_on_cran()
      for(method in  mths){
          expect_is(tc <- topCor(lsat, dem = srtm, metaData = metaData, method = method, filename = rasterTmpFile()), "SpatRaster")
          expect_equal(names(tc),  if(method!="illu") names(lsat) else "illu")
      }
      expect_is(tc2 <- topCor(lsat, dem = srtm, metaData = metaData, method = "minnaert", stratImg='slope', nStrat = 5), "list")
      expect_is(tc3 <- topCor(lsat, dem = srtm, solarAngles = c(1.081533, 0.7023922), method = "C"), "SpatRaster")
    })

})
