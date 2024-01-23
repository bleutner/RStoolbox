context("oneHotEncode")
library(terra)
r <- rast(vals = c(1,2,0,1,NA,-1), ncol = 2, nrow = 3)

result_with_na <- structure(c(1L, 0L, 0L, 1L, NA, 0L, 0L, 1L, 0L, 0L, NA, 0L), .Dim = c(6L, 2L), .Dimnames = list(NULL, c("c_1", "c_2")))
result_without_na <- result_without_na_bgNA <- structure(c(1L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L), .Dim = c(6L, 2L), .Dimnames = list(NULL, c("c_1", "c_2")))
result_without_na_bgNA[result_without_na_bgNA == 0] <- NA

test_that("oneHotEncode raster input", {
    expect_is(oh <- oneHotEncode(r, classes = c(1,2), na.rm = TRUE), "SpatRaster", info = "check class is SpatRaster")
    expect_equal(nlyr(oh), 2, info = "check number of layers")
    expect_equal(oh[], result_without_na, info = "check return values")
    expect_equal(oneHotEncode(r[], classes = c(1,2), na.rm = FALSE), result_with_na, info = "with na.rm=TRUE")
    expect_equal(as.numeric(oneHotEncode(r[], classes = 1, na.rm = TRUE)), result_without_na[,1], info = "for one class only")
    expect_equal(oneHotEncode(r, 1:2, background = NA, na.rm = TRUE)[], result_without_na_bgNA)
    expect_equal(oneHotEncode(r, 1:2, foreground = 7, na.rm = TRUE)[], result_without_na*7)
})

test_that("oneHotEncode vector input", {
    expect_is(oh <- oneHotEncode(r[], classes = c(1,2), na.rm = TRUE), "matrix")
    expect_equal(oneHotEncode(r[], classes = c(1,2), na.rm = FALSE), result_with_na)
    expect_equal(oneHotEncode(r[], classes = 1, na.rm  = TRUE), result_without_na[,1,drop = FALSE])
})

                
                
            