context("tasseledCap")

data(lsat)

test_that("basic function",{
           for(sat in c("Landsat4TM", "Landsat5TM", "Landsat7ETM", "Landsat8OLI"))  {
              expect_is(lsat_tc <- tasseledCap(lsat[[c(1:5,7)]], sat = sat), "SpatRaster")
           }
           expect_is(lsat_tc <- tasseledCap(lsat, sat = "MODIS"), "SpatRaster")
           expect_error(tc <- tasseledCap(lsat[[1:4]], sat = "MODIS"), "Number of layers does not match")
           expect_error(tc <- tasseledCap(lsat[[1:4]], sat = "xkcd"),"Sensor not implemented")
        })

