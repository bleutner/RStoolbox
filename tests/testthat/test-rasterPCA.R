context("rasterPCA")

library(raster)

data(lsat)
lsat <- crop(lsat,extent(lsat)*.2)
ld  <- as.data.frame(lsat)

for(spc in c(FALSE, TRUE)) {
	test_that(paste("princomp(covMat(raster)) == princomp(sample) with spca=",spc), {
				expect_s3_class(r   <- rasterPCA(lsat, nSamples = NULL, spca = spc), c("RStoolbox","rasterPCA"))
				expect_s3_class(rs  <- rasterPCA(lsat, nSamples = ncell(lsat), spca = spc), c("RStoolbox","rasterPCA"))
				expect_equal(abs(unclass(rs$model$loadings)), abs(unclass(r$model$loadings)))
				if(!spc) expect_equivalent(abs(r$map[]), abs(rs$map[])) ## TODO: activate for spca=T once raster::cellStats(sd,asSample=F) is fixed 
			})
	
}

lsat[[1]][100:200] <- NA
lsat[400:500] 	   <- NA
G <- expand.grid(smpl=c(TRUE,FALSE), spc = c(TRUE, FALSE))
for(i in 1:nrow(G)){
	spc  <- G[i,"spc"]
	smpl <- if(G[i,"smpl"]) ncell(lsat) else NULL
	test_that(paste("rasterPCA NA handling; spca =",spc, "; nSamples =", deparse(smpl)), {
				expect_s3_class(r   <- rasterPCA(lsat, nSamples = smpl, spca = spc), c("RStoolbox","rasterPCA"))
				expect_true(all(is.na(r$map[c(100:200,400:500)])))
				expect_false(any(is.na(r$map[1:99])))
			})	
	
}