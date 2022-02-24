context("internal functions")

ra <- raster(ncol = 5, nrow=5, val = 1)
terra <- rast(ra)

test_that(".toRaster conversion", {
            expect_identical(stack(ra),.toRaster(terra))            
        })

sp <- readRDS(system.file("external/trainingPolygons.rds", package="RStoolbox"))
sf <- st_as_sf(sp)
test_that(".toSp conversion", {
			expect_is(.toSp(sf), c("SpatialPolygonsDataFrame"))            
		})
