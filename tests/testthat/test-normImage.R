context("normImage")

library(terra)
lsat_t <- lsat

for(mem in c(TRUE, FALSE)){
  test_that("normImage for single or multiple layers", {
    ## Multiple layers 	
    expect_is(nlsat <- normImage(lsat, norm = TRUE), "SpatRaster")
    expect_true(all(round(colMeans(nlsat[]), 5)==0))
    
    ## Single layer
    expect_is(nlsat <- normImage(lsat[[1]], norm = TRUE), "SpatRaster")
    expect_equal(round(mean(nlsat[]), 5),0)
  }
  )
}

test_that("terra inputs work", {
  expect_is(nlsat <- normImage(lsat, norm = TRUE), "SpatRaster")
  expect_true(all(round(colMeans(nlsat[]), 5)==0))
})

lsat_t[1, 1] <- NA
lsat_t[[2]][2] <- NA
test_that("normImage with NAs",{
  expect_is(nlsat <- normImage(lsat_t, norm = TRUE), "SpatRaster")
  expect_true(all(is.na(nlsat[1])))
  expect_equal(as.vector(is.na(nlsat[2])), c(F,T,rep(F,5)))
})



test_that("normImage.cpp works", {
  m  <- as.matrix(lsat_t[1:5])
  cm <- colMeans(m, na.rm = TRUE)
  cs <- apply(m, 2, sd, na.rm = TRUE)

  expect_is(cmat <- normImageCpp(m, cm, cs), "matrix")
  expect_true(all(round(colMeans(cmat, na.rm = T), 10)==0))
  expect_true(all(round(apply(cmat, 2, sd, na.rm = T), 10)==1))
  expect_equal(sum(is.na(cmat[1,])), 7)
  expect_equal(sum(is.na(cmat[2,])), 1)
  expect_equivalent(normImageCpp(m, cm, cs), scale(m, T, T))

})
