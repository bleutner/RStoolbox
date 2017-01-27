context("spectralIndices")

library(raster)
suppressPackageStartupMessages(library(pls))

## Create test data-sets
vals <- c(-1, 0, 0.5, 1, 2, NA)
vals <- expand.grid(vals,vals)
r  <-  ml <- raster(ncol= 6, nrow = 6)
r[]  <- vals[,1]
r    <- stack(r,r)
names(r) <- c("L1", "L2")
r[[2]]<-vals[,2]
ml[] <- 1
ml[,2] <- 10
ml[,3] <- NA
names(ml) <- "henryMaske"

test_that("errors and warnings", {
            expect_error(spectralIndices(r, red = 1, indices = "NDVI"), "you must specify \\*all\\* required bands")
            expect_error(spectralIndices(r, red = 1), "you must specify \\*all\\* required bands")
            expect_warning(spectralIndices(r, red = 1, nir = 2, indices = c("NDVI", "EVI")), "not specified: blue")
            expect_warning(spectralIndices(r, red = 1, nir = 2, blue = 1, index = c("NDVI", "EVI")), "Skipping EVI")
            expect_is(spectralIndices(r, red = 1, nir = 2, blue = 1, index = c( "EVI"), skipRefCheck = TRUE), "RasterLayer")
            expect_error(spectralIndices(r, red = 1, nir = 2, blue = 1, maskLayer = FALSE, index = c( "ndvi"), skipRefCheck = TRUE), "maskLayer must be")
            expect_error(spectralIndices(r, red = 1, nir = 2, blue = 1, maskLayer = "reginaHalmich", index = c( "ndvi"), skipRefCheck = TRUE), "is not a layer")
        })

m_cfg <- list(ml, 3, "henryMaske")
test_that("maskLayer",  {
            for(i in 1:3){
                expect_s4_class(nd <- spectralIndices(stack(r,ml), red = 1, nir=2, indices = "NDVI", maskLayer = m_cfg[[i]], maskValue = 10), "RasterLayer")
                expect_equal(nd[,2], rep(NA_real_, 6))
                expect_s4_class(nd <- spectralIndices(stack(r,ml), red = 1, nir=2, indices = "NDVI",  maskLayer = m_cfg[[i]], maskValue = NA), "RasterLayer")
                expect_equal(nd[,3], rep(NA_real_, 6))            
            }          
        })



test_that("returned classes", {
            vi <- list(
                    spectralIndices(r, red = 1, nir = 2, indices = "NDVI"),
                    spectralIndices(r, red = "L1", nir = 2, indices = "NDVI"),
                    spectralIndices(r, red = "L1", nir = "L2", indices = "NDVI"),         
                    spectralIndices(r, red = 1, nir = 2, indices = c("NDVI", "DVI", "MSAVI2"))
            )
            ## Check numeric, mixed and character band indices
            expect_identical(vi[[1]], vi[[2]], info = "numeric vs. mixed band indexes")
            expect_identical(vi[[1]], vi[[3]], info = "numeric vs. character band indexes")
            
            ## Check layer numbers and names
            expect_identical(nlayers(vi[[1]]), 1)
            expect_identical(names(vi[[1]]), "NDVI")
            expect_identical(nlayers(suppressWarnings(spectralIndices(r, red = 1, nir = 2, indices = c("NDVI", "EVI")))), 1)
            expect_identical(nlayers(vi[[4]]), 3L, info = "nlayers: 3 indices NDVI, MSAVI2, DVI")
            expect_identical(names(vi[[4]]), c("NDVI", "DVI", "MSAVI2"), info = "names: 3 indices NDVI, MSAVI2, DVI")
            
            ## Check index values
            ## NDVI like
            expect_identical(range(vi[[1]][], na.rm = TRUE), c(-1, 1))
            
        })

data(lsat)
test_that("excercise all indices", {
            expect_is(sp <- spectralIndices(lsat, blue = 1, green=2, red=3, nir=4, swir2=5, swir3=7,
                            coefs = list(L=0.4,s=0.3,swir2ccc=30,swir2coc=140), scaleFactor=255), "RasterBrick")
            expect_equal(nlayers(sp), 22)
        })






## Check for duplicate indices
#tmat <- do.call(rbind, lapply(1:ncol(k), function(i){colSums(k[,i]-k, na.rm = T)==0}))
#colnames(tmat) <- rownames(tmat) <- colnames(k)

