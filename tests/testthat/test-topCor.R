context("topCor")
suppressPackageStartupMessages(library(raster))
metaData <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")
metaData <- readMeta(metaData)
lsat     <- stackMeta(metaData) 
data(srtm)


## Minnaert correction, solar angles from metaData
test_that("basic functioning", {
            for(method in c( c("cos", "avgcos", "C", "stat", "illu"))){
                expect_s4_class(tc <- topCor(lsat, dem = srtm, metaData = metaData, method = method), "Raster")
                expect_equal(names(tc),  if(method!="illu") names(lsat) else "illu")
            }
            for(method in c( c("cos", "avgcos", "C", "stat", "illu"))){
                expect_s4_class(tc <- topCor(lsat, dem = srtm, metaData = metaData, method = method, filename = rasterTmpFile()), "Raster")
                expect_equal(names(tc),  if(method!="illu") names(lsat) else "illu")
            }
            expect_s4_class(topCor(lsat, dem = srtm, metaData = metaData, method = "minnaert", stratImg='slope', nStrat = 5), "Raster")
            expect_s4_class(topCor(lsat, dem = srtm, solarAngles = c(1.081533, 0.7023922), method = "C"), "Raster")
            
        })
