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
            for(cm in c(TRUE, FALSE)) {
                expect_is(sc <- unsuperClass(lsat,  nClasses = 2, clusterMap = cm), "unsuperClass")
                expect_is(scNA <- unsuperClass(lsatNA,  nClasses = 2, clusterMap = cm), "unsuperClass")
                expect_true(all(is.na(scNA$map[20:40,])))			
                expect_is(scNA <- unsuperClass(lsatNA2,  nClasses = 2, filename = rasterTmpFile(), clusterMap = cm), "unsuperClass")
                expect_equal(minValue(scNA$map), 1)
            }
        }) 

test_that("terra inputs",{
	expect_is(sc <- unsuperClass(rast(lsat),  nClasses = 2), "unsuperClass")
})

## kmeans prediction function only
mat <- matrix(1:20, by = TRUE, nrow = 5, ncol=4)
cents <- mat[c(1,3),]
dists <- apply(cents, 1, function(ce) { apply(mat, 1, function(x) {
    sqrt(sum((x - ce)^2))
} ) })


test_that("kmeans predictions",{
    expect_equal(predKmeansCpp(mat, cents), matrix(c(1,1,2,2,2), ncol = 1))
    expect_equal(predKmeansCpp(mat, cents,TRUE), dists)
    mat[1] <- NA
    dists[1,] <- NA
    expect_equal(predKmeansCpp(mat, cents), matrix(c(NA,1,2,2,2), ncol = 1))
    expect_equal(predKmeansCpp(mat, cents, TRUE), dists)
})


## pretty print
test_that("printing method", {
    skip_on_cran()
    expect_output(print(unsuperClass(lsat,  nClasses = 2)), "unsuperClass results")
})


## algortithm warning
test_that("kmeans fail detection", {
            skip_on_cran()
            set.seed(1)
            expect_warning(unsuperClass(lsat, nSamples = ncell(lsat), nStarts = 1, nClasses = 20), "doesn't converge properly")
        })

## Helper for symlink-proof filename checking
## Added to fix CI on gh-actions
slp_bn <- function(path, tmp = basename(tempdir())) {
    tail(strsplit(path, tmp)[[1]],1)
}

## Predict S3 method
test_that("predict.unSuperClass", {
    skip_on_cran()
    uc <- unsuperClass(lsat, nSamples = ncell(lsat), nClasses = 2)
    expect_s4_class(pred <- predict(uc, lsat), "RasterLayer")
    expect_equal(unique(uc$map - pred), 0)
    
    tmpFile <- tempfile(fileext = ".grd")    
    pred <- predict(uc, lsat, filename = tmpFile )
    expect_false(inMemory(pred))
    expect_equal(slp_bn(filename(pred)), slp_bn(tmpFile))
    file.remove(tmpFile, gsub("grd", "gri", tmpFile))
  })
