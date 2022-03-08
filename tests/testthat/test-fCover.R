context("fCover")

set.seed(42)
suppressPackageStartupMessages(library(terra))

data(lsat)
lc	  <- unsuperClass(lsat, nSamples = 50, nClass=3)$map
modis <- aggregate(lsat, 9)

for(cl in 1:2) {
    if (!identical(Sys.getenv("NOT_CRAN"), "true") && cl == 2) next
    
	test_that(sprintf("works for %s classes(s)",cl), {
				expect_is(fc <- fCover(
								classImage = lc ,
								predImage = modis,
								classes=1:cl,
								model="lm",
								trControl = trainControl(method = "cv", number = 2),
								nSample = 30,
								tuneLength=1
						), c("RStoolbox", "fCover"))
				expect_equal(.nlyr(fc$map), cl)
			})

}

test_that("errors and warnings are thrown",{
expect_error(fCover(
		classImage = lc ,
		predImage = modis,
		classes = 4,
		model="rf",
		nSample = 50,
		tuneLength=1
),"One or more classes are not represented in the sampled pixels")
})