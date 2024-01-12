context("pifMatch")
library(terra)

lsat_b <- log(lsat)

for(m in c("cor", "sam", "ed")) {
   test_that("pifMatch return classes", {
      expect_is(lb <- pifMatch(lsat_b, lsat, method = m, returnPifMap = TRUE, returnSimMap = TRUE, returnModels = TRUE), "list", info = sprintf("method=%s", m))
      expect_equal(names(lb), c("img", "simMap", "pifMap", "models"))
      expect_is(lb$models$B1_dn, "lm")
      expect_true(all(vapply(lb[2:3],inherits, logical(1), "SpatRaster")))
      expect_is(lb$img, "list")

   })
}

test_that("error messages", {
    expect_error(lb <- pifMatch(lsat_b, lsat, method = "ok", returnPifMap = TRUE, returnSimMap = TRUE, returnModels = TRUE), "method must be one of")
})
