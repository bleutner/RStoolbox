context("internal functions")

ra <- raster(ncol = 5, nrow=5, val = 1)
terra <- rast(ra)

test_that(".toRaster conversion", {
    expect_identical(stack(ra),.toRaster(terra))
})

sf <- readRDS(system.file("external/trainingPolygons.rds", package="RStoolbox"))

test_that("Loaded as sf", {
    expect_is(sf, c("sf", "data.frame"))
})


test_that(".canProcInMem says no", {
  expect_true(.canProcInMem(terra,1))
  expect_false(.canProcInMem(terra,1e20))
  expect_true(.canProcInMem(ra,1))
  expect_false(.canProcInMem(ra,1e20))
})