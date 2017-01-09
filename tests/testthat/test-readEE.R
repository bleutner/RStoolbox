context("readEE")

file <- system.file("external/EarthExplorer_LS8.txt", package = "RStoolbox")

test_that("returned classes", {
			expect_is(ee <- readEE(file), "data.frame")
		})