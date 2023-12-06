context("multiple endmember spectral mixture analysis")

lsat <- lsat_rs
pts <- data.frame(class=c("water", "land"), cell = c(47916,5294))
em <- as.matrix(lsat[pts$cell])
props <- matrix(c(seq(0,1,.1), seq(1,0,-.1)),ncol=2)
mat <- props %*% em

test_that("nnls_solver returns correct solutions",{
  expect_equal(props, round(nnls_solver(x = mat, A = em)[,c(1,2)], digits = 2))
}
)

test_that("solver output class", {
  expect_is(solved <- nnls_solver(x = mat, A = em)[,c(1,2)], "matrix")
})

test_that("mesma call using NNLS", {
  expect_is(solved <- mesma(lsat, em, method = "NNLS"), "SpatRaster")
  expect_is(solved <- mesma(lsat, data.frame(em), method = "NNLS"), "SpatRaster")
})

test_that("mesma method error", {
  expect_error(mesma(lsat, em, method = "no-valid-method"))
})

values(lsat)[c(1,10,100,400,200),c(3,4,5,2,7)] <- NA

test_that("mesma img NA handling", {
  expect_is(solved <- mesma(lsat, em), "SpatRaster")
})

emNA <- em
emNA[1,6] <- NA

test_that("mesma img NA handling", {
  expect_error(mesma(lsat, emNA))
})
