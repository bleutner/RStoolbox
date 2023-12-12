context("coregisterImages")

loff <- shift(lsat_rs, 30, 60)

test_that("works and finds correct shift", {
	expect_is(lcor <- coregisterImages(loff, lsat_rs, nSamples = 100, reportStats= TRUE), "list")
	expect_equivalent(lcor$bestShift, c(x=-30, y=-60))
	expect_equivalent(ext(lcor$coregImg) , ext(lsat_rs))
	expect_equal(vapply(lcor, class, character(1)), c(MI="data.frame", jointHist="list", bestShift = "data.frame", coregImg = "SpatRaster"))
})

