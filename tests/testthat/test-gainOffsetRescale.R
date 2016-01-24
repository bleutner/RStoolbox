context("gainOffsetRescaleCpp")
suppressPackageStartupMessages(library(raster))
r <- raster(vals = 1, ncol = 2, nrow = 2)
r <- stack(r,r*2)
r[[1]][1:4] <- c(NA, -100, 100, 0)
gain <- c(.1,.2)
offset <- c(.1,.4)
clamp <- list(c(TRUE,FALSE), c(FALSE,TRUE), c(TRUE, TRUE), c(FALSE, FALSE))
expected <-list(c(NA, 0, 10.1, 0.1,rep(.8,4)),
        c(NA, -9.9, 1, 0.1, rep(.8,4)),
        c(NA, 0, 1, 0.1, rep(.8,4)),
        c(NA, -9.9, 10.1, 0.1,rep(.8,4)))

test_that("NA, clamping, general",{
            for(i in seq_along(clamp)){
                out <- calc(r, function(x) gainOffsetRescale(x, gain, offset, clamp[[i]]), forcefun=T)
                expect_equal(as.vector(out[]), expected[[i]])
            }
        })