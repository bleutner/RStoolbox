context("readMeta & stackMeta")

tdir <- system.file("tests/testdata/metadata", package="RStoolbox")
mfil <- list.files(tdir, full = TRUE)

for(f in mfil) {
	test_that(paste("readMeta ", basename(f)), {
				expect_s3_class(m <- readMeta(f), c("RStoolbox", "ImageMetaData"))
				expect_s3_class(m$ACQUISITION_DATE, c("POSIXlt", "POSIXt")) 
			})
	
}


mtlFile  <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")
test_that("stackMeta with exampleData", {
			expect_s4_class(stackMeta(mtlFile), "RasterStack")
			expect_s4_class(stackMeta(readMeta(mtlFile)), "RasterStack")
		})
