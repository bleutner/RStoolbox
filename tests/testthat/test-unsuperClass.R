context("unsuperClass")

library(raster)

## Set-up test data
data(lsat)
lsatNA <- lsat
lsatNA[20:40, ] <- NA
							
lsatNA2 <- lsat
lsatNA2 <- writeRaster(lsatNA2, rasterTmpFile())
NAvalue(lsatNA2) <- 20

## Tiny raster bug caused superClass to fail when predictions were written to .grd file 
test_that("unsuperClass and NA",{
			expect_is(sc <- unsuperClass(lsat,  nClasses = 3), "unsuperClass")
			expect_is(scNA <- unsuperClass(lsatNA,  nClasses = 3), "unsuperClass")
			expect_true(all(is.na(scNA$map[20:40,])))			
			expect_is(scNA <- unsuperClass(lsatNA2,  nClasses = 3, filename = rasterTmpFile()), "unsuperClass")
			expect_equal(minValue(scNA$map), 1)
		}) 


## kmeans prediction function only
mat <- matrix(1:20, by = TRUE, nrow = 5, ncol=4)
cents <- mat[c(1,3),]
test_that("kmeans predictions",{
			expect_equal(predKmeansCpp(mat, cents), c(1,1,2,2,2))
			mat[1] <- NA
			expect_equal(predKmeansCpp(mat, cents), c(NA,1,2,2,2))
		})





