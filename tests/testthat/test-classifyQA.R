context("classifyQA")

set.seed(1)
qa <- rast(ncol = 128, nrow=128, val = 1:2^14)
# TODO: add real LS8 example data

test_that("returned classes and number of layers", {
    ## QA classes
    expect_is(qacs <- classifyQA(img = qa), "SpatRaster")
    expect_equal(names(qacs), "QAclass")
    expect_true(all(c(unique(qacs) %in% 1:5)))

    ## Single category
    expect_is(qacs <- classifyQA(img = qa, type = "cirrus"), "RasterLayer")
    expect_equal(unique(qacs) , 3)

    ## Confidence levels
    ## All categories
    expect_is(qacs_conf <- classifyQA(img = qa, confLayers = TRUE), "RasterStack")
    expect_equal(names(qacs_conf), c("cloud", "cirrus", "snow", "water"))
    expect_true(all(unique(as.vector(qacs_conf[])) %in% c(NA,1:3)))

    ## Single category
    expect_is(qacs_conf <- classifyQA(img = qa, type = "water", confLayers = TRUE), "RasterStack")
    expect_equal(names(qacs_conf), c("water"))
    expect_true(all(unique(as.vector(qacs_conf[])) %in% c(NA,1:3)))

})
