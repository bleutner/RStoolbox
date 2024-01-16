context("rasterPCA")

library(terra)

lsat_t <- crop(lsat, ext(lsat)*.2)
ld   <- as.data.frame(lsat_t)

for(spc in c(FALSE, TRUE)) {
	test_that(paste("stats::princomp(covMat(raster)) == stats::princomp(sample) with spca=",spc), {
		expect_s3_class(r   <- rasterPCA(lsat_t, nSamples = NULL, spca = spc), c("RStoolbox", "rasterPCA"))
		expect_s3_class(rs  <- rasterPCA(lsat_t, nSamples = ncell(lsat_t), spca = spc), c("RStoolbox", "rasterPCA"))
		expect_equal(abs(unclass(rs$model$loadings)), abs(unclass(r$model$loadings)))
		expect_equal(abs(r$map[]), abs(rs$map[]), tolerance = 1e-03)
	})
	
}

lsat_t[[1]][100:200] <- NA
lsat_t[400:500] 	   <- NA
G <- expand.grid(smpl=c(TRUE,FALSE), spc = c(TRUE, FALSE))
for(i in seq_len(nrow(G))){
	spc  <- G[i,"spc"]
	smpl <- if(G[i,"smpl"]) ncell(lsat_t) else NULL
	test_that(paste("rasterPCA NA handling; spca =",spc, "; nSamples =", deparse(smpl)), {
		suppressWarnings({
			expect_s3_class(r   <- rasterPCA(lsat_t, nSamples = smpl, spca = spc), c("RStoolbox", "rasterPCA"))
			expect_true(all(is.na(r$map[c(100:200,400:500)])))
			expect_false(any(is.na(r$map[1:99])))
		})
	})
}