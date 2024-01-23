context("readMeta")
mfil <- list.files("testdata/metadata", full = TRUE)
for(f in mfil) {
	test_that(paste("readMeta and summary(readMeta)", basename(f)), {
				expect_s3_class(m <- readMeta(f), c("RStoolbox", "ImageMetaData"))
				expect_s3_class(m$ACQUISITION_DATE, c("POSIXlt", "POSIXt")) 
                expect_true(all(grepl("^B1?[0-9]_(dn|toa|sre){1}$", m$DATA$BANDS[1:4])))
				expect_output( summary(m), "Scene:")
				expect_is(m <- readMeta(f, raw = TRUE), "list")
				expect_gte(length(names(m)), 2)
			})
}
