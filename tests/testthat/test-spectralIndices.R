context("spectralIndices")

library(raster)
suppressPackageStartupMessages(library(pls))

## Create test data-sets
vals <- c(-1, 0, 0.5, 1, 2, NA)
vals <- expand.grid(vals,vals)
r    <- raster(ncol= 6, nrow = 6)
r[]  <- vals[,1]
r    <- stack(r,r)
names(r) <- c("L1", "L2")
r[[2]]<-vals[,2]

test_that("gives proper errors and warnings", {
            expect_error(spectralIndices(r, red = 1, indices = "NDVI"), "you must specify \\*all\\* required bands")
            expect_error(spectralIndices(r, red = 1), "you must specify \\*all\\* required bands")
            expect_warning(spectralIndices(r, red = 1, nir = 2, indices = c("NDVI", "EVI")), "not specified: blue")
            expect_warning(spectralIndices(r, red = 1, nir = 2, blue = 1, index = c("NDVI", "EVI")), "Skipping EVI")
        })


test_that("returns", {
            vi <- list(
                    spectralIndices(r, red = 1, nir = 2, indices = "NDVI"),
                    spectralIndices(r, red = "L1", nir = 2, indices = "NDVI"),
                    spectralIndices(r, red = "L1", nir = "L2", indices = "NDVI"),         
					spectralIndices(r, red = 1, nir = 2, indices = c("NDVI", "DVI", "MSAVI2"))
            )
            ## Check numeric, mixed and character band indices
            expect_identical(vi[[1]], vi[[2]], info = "numeric vs. mixed band indexes")
            expect_identical(vi[[1]], vi[[3]], info = "numeric vs. character band indexes")
            
            ## Check layer numbers and names
            expect_identical(nlayers(vi[[1]]), 1)
            expect_identical(names(vi[[1]]), "NDVI")
            expect_identical(nlayers(suppressWarnings(spectralIndices(r, red = 1, nir = 2, indices = c("NDVI", "EVI")))), 1)
			expect_identical(nlayers(vi[[4]]), 3L, info = "nlayers: 3 indices NDVI, MSAVI2, DVI")
			expect_identical(names(vi[[4]]), c("NDVI", "DVI", "MSAVI2"), info = "names: 3 indices NDVI, MSAVI2, DVI")
			
			## Check index values
			## NDVI like
			expect_identical(range(vi[[1]][], na.rm = TRUE), c(-1, 1))
			
        })


