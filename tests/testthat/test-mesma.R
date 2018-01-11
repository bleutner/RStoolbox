context("multiple endmember spectral mixture analysis")

data(lsat)
pts <- data.frame(class=c("water", "land"), cell = c(47916,5294))
spec <- lsat[pts$cell]
props <- matrix(c(seq(0,1,.1), seq(1,0,-.1)),ncol=2)
mat <- props %*% spec

test_that("nnls_mat returns correct solutions",{
  expect_equal(props, round(nnls_solver(x = mat, A = spec)[,c(1,2)], digits = 2))
}
)

test_that("solver output class", {
  expect_is(solved <- nnls_solver(x = mat, A = spec)[,c(1,2)], "matrix")
})