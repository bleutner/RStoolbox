context("readEE")

exfile <- system.file("external/EarthExplorer_LS8.txt", package = "RStoolbox")
files <- list.files("testdata/earthexplorer", full = TRUE)

test_that("returned classes", {
    skip_on_cran()
    expect_is(ee <- readEE(files[1]), "data.frame")
    expect_is(ee <- readEE(files), "data.frame")
    expect_true(all(is.na(ee$Download.Link)))
    expect_is(ee <- readEE(exfile), "data.frame")
    expect_true(all(is.na(ee$Browse.Link)))
})
