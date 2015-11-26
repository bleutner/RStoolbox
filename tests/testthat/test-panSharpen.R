context("panSharpen")

data(lsat)
library(raster)

agg     <- aggregate(lsat,10)
pan     <- sum(lsat[[1:3]])
meth    <- c("brovey", "ihs", "pca")
panList <- lapply(meth, function(m) panSharpen(img = agg, pan = pan, r = 3, g = 2, b = 1, method = m))
nlayers <- c(3,3,7)
names(nlayers) <- names(panList) <- meth

test_that("panSharpen methods",	{
			for(m in meth) expect_is(panList[[m]], "RasterBrick", info = m)
			for(m in meth) expect_equal(names(panList[[m]]), paste0(names(agg)[1:nlayers[m]], "_pan"), info = m)  # tests inmplicitly for correct number of layers
			for(m in meth) expect_equal(res(panList[[m]]), res(pan), info = m)
		})