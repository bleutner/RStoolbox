context("getMeta")
mtlFile  <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")
meta <- readMeta(mtlFile)
lsat <- stackMeta(mtlFile)

test_that("Get integer scale factors",{
    ## Vectors        
    expect_is(gm <- getMeta(lsat, metaData = meta, what = "SCALE_FACTOR"), "numeric")
    expect_equal(length(gm), nlyr(lsat))
    expect_is(gm <- getMeta(lsat, metaData = meta, what = "FILES"), "character")
    expect_is(gm <- getMeta(lsat, metaData = meta, what = "QUANTITY"), "character")
    expect_true(all(gm == "dn"))
    expect_is(gm <- getMeta(lsat, metaData = meta, what = "CATEGORY"), "character")
    expect_true(all(gm == "image"))
    
    ## Data.frames
    expect_is(gm <- getMeta("B6_dn", metaData = meta, what = "CALBT"), "data.frame")
    expect_is(gm <- getMeta(c("B1_dn", "B2_dn"), metaData = meta, what = "CALRAD"), "data.frame")
    ## Ordered?
    expect_is(gm <- getMeta(lsat[[5:1]], metaData = meta, what = "CALRAD"), "data.frame")
    expect_equal(rownames(gm), names(lsat[[5:1]]))
    ## NA
    expect_error(gm <- getMeta(lsat, metaData = meta, what = "CALREF"), "not populated")
    
})

