context("rescaleImage")
library(raster)
data(lsat)
lsat2 <- lsat - 1000
lsat2r <- rescaleImage(lsat2, lsat)
lsat2u <- rescaleImage(lsat2, ymin = 0.5, ymax = 1.6)

test_that("rescales to proper limits", {
            expect_true(compareRaster(lsat, lsat2r, values = TRUE))
            expect_equal(minValue(lsat2u), rep(0.5, nlayers(lsat)))
            expect_equal(maxValue(lsat2u), rep(1.6, nlayers(lsat)))
            expect_true(compareRaster(lsat2u[[1]], rescaleImage(lsat[[1]], ymin = 0.5, ymax = 1.6), values = TRUE)) ## RasterLayer vs. RasterBrick/Stack            
        })


test_that("deals with missing values and single valued layers and returns NAs", {
            lsat2[[1]][] <- 1
            suppressWarnings(lsat2[[2]][] <- NA)
            suppressWarnings(lsat2[[3]][] <- Inf)
            lsat2[[4]][,1:100] <- NA
            lsat2[[5]][,1:100] <- Inf                     
            expect_warning(lsaResc <- rescaleImage(lsat2, ymin = 0, ymax = 1), "no value range.*B1_dn*")
            expect_equal(cellStats(is.na(lsaResc[[1]]), "sum"), ncell(lsat)) # single values
            expect_equal(cellStats(is.na(lsaResc[[2]]), "sum"), ncell(lsat)) # NAs
            expect_equal(cellStats(is.na(lsaResc[[3]]), "sum"), ncell(lsat)) # Infinites
            expect_equal(minValue(lsaResc[[4]]), 0)  ## Ignores NAs min
            expect_equal(maxValue(lsaResc[[4]]), 1)  ## Ignores NAs max
            expect_false(any(!is.na(lsaResc[[4]][,1:100])))
            expect_equal(cellStats(is.na(lsaResc[[5]]), "sum"), ncell(lsat)) # Partial Infinites -> NA
                     
        })