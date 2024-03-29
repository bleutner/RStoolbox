context("histMatch")

library(terra)
singLay <- histMatch(lsat[[1]], lsat[[2]])
multLay <- histMatch(lsat, sqrt(lsat))

test_that("histMatch RasterLayers and Stack/Bricks", {
 	expect_is(singLay, "SpatRaster")
	expect_is(multLay, "SpatRaster")
	expect_equal(names(multLay), names(lsat))
	expect_equal(names(singLay), names(lsat)[1])
})
