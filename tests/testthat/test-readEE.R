context("readEE")

exfile <- system.file("external/EarthExplorer_LS8.txt", package = "RStoolbox")
tdir <- system.file("tests/testdata/earthexplorer", package="RStoolbox")
files <- list.files(tdir, full = TRUE)

test_that("returned classes", {
			expect_is(ee <- readEE(files[1]), "data.frame")
            expect_is(ee <- readEE(files), "data.frame")
            expect_true(all(is.na(ee$Download.Link))) 
            expect_is(ee <- readEE(exfile), "data.frame")
            expect_true(all(is.na(ee$Browse.Link))) 
            
		})