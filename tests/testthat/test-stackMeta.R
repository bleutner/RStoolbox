context("stackMeta")

mtlFile  <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")
test_that("stackMeta with exampleData", {
  expect_s4_class(st <- stackMeta(mtlFile, returnTerra = FALSE), "RasterStack")
  expect_s4_class(st <- stackMeta(mtlFile, returnTerra = TRUE), "SpatRaster")
  expect_s4_class(stackMeta(readMeta(mtlFile)), "RasterStack")
  expect_true(all(grepl("B[1-7]_dn", names(st))))
})
