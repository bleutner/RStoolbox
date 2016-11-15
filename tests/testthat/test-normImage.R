context("normImage")

library(raster)
data(lsat)

rasterOptions(todisk = TRUE)

for(mem in 1:2){
	test_that("normImage for single or multiple layers", {
				## Multiple layers 	
				expect_is(nlsat <- normImage(lsat, norm = TRUE), "RasterBrick")		
				expect_true(all(round(colMeans(nlsat[]), 10)==0))
				
				## Single layer
				expect_is(nlsat <- normImage(lsat[[1]], norm = TRUE), "RasterLayer")
				expect_equal(round(mean(nlsat[])),0)
			}
	)
	rasterOptions(todisk = FALSE)
}


lsat[1,1] <- NA
lsat[[2]][2] <- NA
test_that("normImage with NAs",{
			expect_is(nlsat <- normImage(lsat, norm = TRUE), "RasterBrick")
			expect_true(all(is.na(nlsat[1])))
			expect_equal(as.vector(is.na(nlsat[2])), c(F,T,rep(F,5)))
		}
)



test_that("normImage.cpp works", {
			
			m  <- lsat[1:5]
			cm <- colMeans(m, na.rm = TRUE)
			cs <- apply(m, 2, sd, na.rm = TRUE)
			
			expect_is(cmat <- RStoolbox:::normImageCpp(m, cm, cs), "matrix")
			expect_true(all(round(colMeans(cmat, na.rm = T), 10)==0))
			expect_true(all(round(apply(cmat, 2, sd, na.rm = T), 10)==1))
			expect_equal(sum(is.na(cmat[1,])), 7)
			expect_equal(sum(is.na(cmat[2,])), 1)	
			expect_equivalent( RStoolbox:::normImageCpp(m, cm, cs), scale(m, T, T))
				
		}
)
