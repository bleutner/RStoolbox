context("internal functions")

terra <- rast(ncol = 5, nrow=5, vals = 1)

sf <- readRDS(system.file("external/trainingPolygons_lsat.rds", package="RStoolbox"))

test_that("Loaded as sf", {
    expect_is(sf, c("sf", "data.frame"))
})


test_that(".canProcInMem says no", {
  expect_true(.canProcInMem(terra,1))
  expect_false(.canProcInMem(terra,1e20))
})