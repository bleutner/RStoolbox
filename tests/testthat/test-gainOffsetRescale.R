context("gainOffsetRescaleCpp")
suppressPackageStartupMessages(library(terra))
r <- rast(vals = 1, ncol = 2, nrow = 2)
r <- c(r,r*2)
r[[1]][1:4] <- c(NA, -100, 100, 0)
gain <- c(.1,.2)
offset <- c(.1,.4)
clamp <- list(c(TRUE,FALSE), c(FALSE,TRUE), c(TRUE, TRUE), c(FALSE, FALSE))
expected <-list(c(NA, .8, 0, .8, 10.1, .8, 0.1, .8),
        c(NA, .8, -9.9, .8, 1, .8, 0.1, .8),
        c(NA, .8, 0, .8, 1, .8, 0.1, .8),
        c(NA, .8, -9.9, .8, 10.1, .8, 0.1, .8)
)

test_that("NA, clamping, general",{
            for(i in seq_along(clamp)){
                out <- app(r, function(x) gainOffsetRescale(x, gain, offset, clamp[[i]]))
                expect_equal(as.vector(out[]), expected[[i]])
            }
        })