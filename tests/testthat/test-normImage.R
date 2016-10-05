context("normImage")

library(raster)
data(lsat)

test_that("normImage", {
		expect_is(nlsat <- normImage(lsat, norm = TRUE), "RasterBrick")
		expect_true(all(round(colMeans(nlsat[]), 10)==0))
	}
)

lsat[1,1] <- NA
lsat[[2]][2] <- NA
test_that("normImage with NAs",{
		expect_is(nlsat <- normImage(lsat, norm = TRUE), "RasterBrick")
		expect_true(all(is.na(nlsat[1])))
		expect_equal(as.vector(is.na(nlsat[2])), c(F,T,rep(F,5)))
	}
)
