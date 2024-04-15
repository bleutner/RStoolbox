context("multiple endmember spectral mixture analysis")

# sets of endmembers
em_sma <- as.matrix(data.frame(lsat[c(5294, 47916)]))
rownames(em_sma) <- c("forest", "water")

em_mesma_2 <- rbind(
   data.frame(lsat[c(4155, 17018, 53134, 69487, 83704)], class = "forest"),
   data.frame(lsat[c(22742, 25946, 38617, 59632, 67313)], class = "water")
)
em_mesma_3 <- rbind(
  data.frame(lsat[c(4155, 17018, 53134, 69487, 83704)], class = "forest"),
  data.frame(lsat[c(22742, 25946, 38617, 59632, 67313)], class = "water"),
  data.frame(lsat[c(4330, 1762, 1278, 1357, 17414)], class = "shortgrown")
)

props <- matrix(c(seq(0,1,.1), seq(1,0,-.1)),ncol=2)
mat <- props %*% em_sma

test_that("nnls_solver returns correct solutions",{
  expect_equal(props, round(nnls_solver(x = mat, A = em_sma)[,c(1,2)], digits = 2))
}
)

test_that("solver output class", {
  expect_is(solved <- nnls_solver(x = mat, A = em_sma)[,c(1,2)], "matrix")
})

test_that("sma call using NNLS", {
  expect_is(solved <- mesma(lsat, em_sma, method = "NNLS"), "SpatRaster")
  expect_is(solved <- mesma(lsat, data.frame(em_sma), method = "NNLS"), "SpatRaster")
})

test_that("method error", {
  expect_error(mesma(lsat, em_sma, method = "no-valid-method"))
})

lsat_t <- lsat
values(lsat_t)[c(1, 10, 100, 400, 200), c(3, 4, 5, 2, 7)] <- NA

test_that("img NA handling", {
  expect_is(solved <- mesma(lsat_t, em_sma), "SpatRaster")
})

emNA <- em_sma
emNA[1,6] <- NA

test_that("img NA handling", {
  expect_error(mesma(lsat_t, emNA))
})

test_that("mesma two classes", {
  probs <- expect_is(mesma(lsat, em_mesma_2), "SpatRaster")
  expect_equal(nlyr(probs), 3)
  expect_equal(names(probs), c("forest", "water", "RMSE"))
  expect_equal(sapply(c(1000, 2000, 3000), function(x) sum(probs[[1:2]][x])), c(1,1,1))
})

test_that("mesma n_models", {
  expect_warning(mesma(lsat, em_mesma_2, n_models = 10))
})

test_that("mesma sum_to_one", {
  probs <- expect_is(mesma(lsat, em_mesma_2, sum_to_one = F), "SpatRaster")
  expect_equal(round(sapply(c(1000, 2000, 3000), function(x) sum(probs[[1:2]][x])), 5), c(0.99841, 1.09785, 1.00944))
})

test_that("mesma three classes", {
  probs <- expect_is(mesma(lsat, em_mesma_3), "SpatRaster")
  expect_equal(nlyr(probs), 4)
  expect_equal(names(probs), c("forest", "water", "shortgrown", "RMSE"))
})

