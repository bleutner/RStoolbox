context("coregisterImages")

data(lsat)
loff <- shift(lsat, 30, 60)

test_that("works and finds correct shift", {
	expect_is(lcor <- coregisterImages(loff, lsat, nSamples = 100, reportStats= TRUE), "list")
	expect_equivalent(lcor$bestShift, c(x=-30, y=-60))
	expect_equivalent(extent(lcor$coregImg) , extent(lsat))
	expect_equal(vapply(lcor, class, character(1)), c(MI="data.frame", jointHist="list", bestShift = "data.frame", coregImg = "RasterBrick"))
})

