context("cloudMask and cloudShadowMask" )

library(raster)
data(lsat) 

test_that("cloud and shadow masking works", {
			expect_is(cldmsk  <- cloudMask(lsat, blue = 1, tir = 6), "RasterStack")
			expect_is(cldmsk_final <- cloudMask(cldmsk, threshold = 0.1, buffer = 5), "RasterStack") 
			expect_equivalent(names(cldmsk), c("CMASK", "NDTCI"))
			expect_equivalent(names(cldmsk_final), c("CMASK", "NDTCI"))
			expect_is(shadow <- cloudShadowMask(lsat, cldmsk_final, shiftEstimate = c(-16,-6)), "RasterLayer")
			expect_is(stack(lsat,cldmsk_final, shadow), "RasterStack", label = "img, cloud and shadow rasters do not fit to each other")
		})



## TODO: How to unit test interactive components?