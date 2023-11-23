context("rescaleImage")
library(terra)
data(lsat)
lsat <- rast(lsat)
lsat2 <- lsat - 1000
lsat2r <- rescaleImage(lsat2, lsat)
lsat2u <- rescaleImage(lsat2, ymin = 0.5, ymax = 1.6)

mm <- matrix(c(0.5, 1.6), ncol= nlyr(lsat), nrow = 2)
colnames(mm) <- names(lsat)
rownames(mm) <- c("min", "max")

test_that("rescales to proper limits", {
            skip_on_cran()
            expect_equal(lsat[], lsat2r[], values = TRUE)
            expect_equal(minmax(lsat2u), mm)
            expect_equal(lsat2u[[1]][], rescaleImage(lsat[[1]], ymin = 0.5, ymax = 1.6)[], values = TRUE)
        })


test_that("deals with missing values and single valued layers and returns NAs", {
            skip_on_cran()
            lsat2[[1]][] <- 1
            suppressWarnings(lsat2[[2]][] <- NA)
            suppressWarnings(lsat2[[3]][] <- Inf)
            lsat2[[4]][,1:100] <- NA
            lsat2[[5]][,1:100] <- Inf                     
            expect_warning(lsaResc <- rescaleImage(lsat2, ymin = 0, ymax = 1), "no value range.*B1_dn*")
            expect_equal(.th_naCount(lsaResc[[1]]), ncell(lsat)) # single values
            expect_equal(.th_naCount(lsaResc[[2]]), ncell(lsat)) # NAs
            expect_equal(.th_naCount(lsaResc[[3]]), ncell(lsat)) # Infinites
            expect_equal(.th_minmax(lsaResc[[4]]), matrix(0:1.,ncol=1))  ## Ignores NAs 
            expect_false(any(!is.na(lsaResc[[4]][,1:100])))
            expect_equal(.th_naCount(lsaResc[[5]]), ncell(lsat)) # Partial Infinites -> NA
                     
        })
