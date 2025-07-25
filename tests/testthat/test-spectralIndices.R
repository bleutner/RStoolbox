context("spectralIndices")

library(terra)

## Create test data-sets
vals <- c(-1, 0, 0.5, 1, 2, NA)
vals <- expand.grid(vals,vals)
r  <-  ml <- rast(ncol= 6, nrow = 6)
r[]  <- vals[,1]
r    <- c(r,r)
r[[2]]<-vals[,2]
names(r) <- c("L1", "L2")
ml[] <- 1
ml[,2] <- 10
ml[,3] <- NA
names(ml) <- "henryMaske"

r <- rast(r)
ml <- rast(ml)
test_that("errors and warnings are emitted", {
  expect_error(spectralIndices(r, red = 1, indices = "NDVI"), "you must specify \\*all\\* required bands")
  expect_warning(spectralIndices(r, red = 1, nir = 2, indices = c("NDVI", "EVI")), "not specified: blue")
  expect_warning(spectralIndices(r, red = 1, nir = 2, blue = 1, index = c("NDVI", "EVI")), "Skipping EVI")
  expect_warning(spectralIndices(r, red = 1, nir = 2, blue = 1, index = "EVI", skipRefCheck = TRUE), "raster has no values")
  expect_error(spectralIndices(r, red = 1, nir = 2, blue = 1, maskLayer = FALSE, index = "ndvi", skipRefCheck = TRUE), "maskLayer must be")
  expect_error(spectralIndices(r, red = 1, nir = 2, blue = 1, maskLayer = "reginaHalmich", index = "ndvi", skipRefCheck = TRUE), "is not a layer")
})

m_cfg <- list(ml, 3, "henryMaske")
nadf  <- data.frame("NDVI" = rep(NA_real_, 6))

# Suppresswarnings in the next section, we are still dealing with na data, guess raster does not throw a warning
# with a valid raster even if every value is na
test_that("maskLayer is considered regardless of input class",  {
  for(i in 1:3){
    expect_s4_class(nd <- suppressWarnings({spectralIndices(c(r,ml), red = 1, nir=2, indices = "NDVI", maskLayer = m_cfg[[i]], maskValue = 10)}), "SpatRaster")
    expect_equal(nd[,2], nadf)
    expect_s4_class(nd <- suppressWarnings({spectralIndices(c(r,ml), red = 1, nir=2, indices = "NDVI", maskLayer = m_cfg[[i]], maskValue = NA)}), "SpatRaster")
    expect_equal(nd[,3], nadf)
  }
})

# Rebuild those test for landsat data
test_that("returned classes", {
  vi <- list(
    spectralIndices(lsat, red = 4, nir = 5, indices = "NDVI"),
    spectralIndices(lsat, red = "B4_dn", nir = 5, indices = "NDVI"),
    spectralIndices(lsat, red = "B4_dn", nir = "B5_dn", indices = "NDVI"),
    spectralIndices(lsat, red = 4, nir = 5, indices = c("NDVI", "DVI", "MSAVI2"))
  )
  ## Check numeric, mixed and character band indices
  expect_equal(vi[[1]], vi[[2]], info = "numeric vs. mixed band indexes")
  expect_equal(vi[[1]], vi[[3]], info = "numeric vs. character band indexes")

  ## Check layer numbers and names
  expect_equal(nlyr(vi[[1]]), 1)
  expect_identical(names(vi[[1]]), "NDVI")
  expect_identical(nlyr(suppressWarnings(spectralIndices(r, red = 1, nir = 2, indices = c("NDVI", "EVI")))), 1)
  expect_identical(nlyr(vi[[4]]), 3, info = "nlayers: 3 indices NDVI, MSAVI2, DVI")
  expect_identical(names(vi[[4]]), c("NDVI", "DVI", "MSAVI2"), info = "names: 3 indices NDVI, MSAVI2, DVI")

  ## Check index values
  ## NDVI like
  expect_lte(max(range(vi[[1]][], na.rm = TRUE)), 1)
  expect_gte(min(range(vi[[1]][], na.rm = TRUE)), -1)
})

test_that("excercise all indices", {
  expect_is(sp <- spectralIndices(lsat, blue = 1, green=2, redEdge1=1, redEdge2=2, redEdge3=3, red=3, nir=4, swir2=5, swir3=7,
                                  coefs = list(L=0.4,s=0.3,swir2ccc=30,swir2coc=140), scaleFactor=255), "SpatRaster")
})
  


idxdb <- getOption("RStoolbox.idxdb")
cdb <- c(
  idxdb,
  CUSTOM = list(list(c("Mueller2024", "Super custom index"), function(red) {red * 0})),
  CUSTOM2 = list(list(c("Mueller2024", "Super custom index"), function(swir1) {swir1 - swir1}))
)
rsOpts(idxdb = cdb)

test_that("custom spectral index",{
  expect_is(spectralIndices(lsat, red = 3, indices = "CUSTOM"), "SpatRaster")
  expect_is(spectralIndices(lsat, swir1 = 5, indices = "CUSTOM2"), "SpatRaster")
  expect_equal(
    unique(values(spectralIndices(lsat, red = 3, indices = "CUSTOM"))),
    as.matrix(data.frame(CUSTOM=0)))
  expect_equal(
    unique(values(spectralIndices(lsat, swir1 = 5, indices = "CUSTOM2"))),
    as.matrix(data.frame(CUSTOM2=0))
  )
})



## Check for duplicate indices
#tmat <- do.call(rbind, lapply(1:ncol(k), function(i){colSums(k[,i]-k, na.rm = T)==0}))
#colnames(tmat) <- rownames(tmat) <- colnames(k)

