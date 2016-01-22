context("estimateHaze")

mtlFile  <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")
data(lsat)

test_that("all hazeBand specifications work", {
            hb <- list(single = 2, contiguous = c(1:3), noncontiguous = c(1,3), noncontiguous2 = c(2,4))
            for(i in seq_along(hb)){
                expect_is(hdn <- estimateHaze(lsat, hazeBands = hb[[i]], plot = FALSE),  "numeric")
                expect_identical(length(hdn), length(hb[[i]]))
                expect_is(hdn2 <- estimateHaze(lsat, hazeBands = names(lsat)[hb[[i]]], plot = FALSE),  "numeric")
                expect_identical(hdn, hdn2)
                expect_equal(names(hdn), names(lsat)[hb[[i]]])          
                expect_is(hdn <- estimateHaze(lsat, hazeBands = hb[[i]], maxSlope = FALSE, plot = FALSE),  "numeric")               
            }       
            expect_error(estimateHaze(lsat), "specify the band")          
        })


vals <- unlist(Map(rep, 1:20, c(1:20)^2))
tera <- raster(vals = vals, ncol = 1, nrow = length(vals))
vals[1]<-NA

test_that("correct haze values are found (and deals with NA)", {
            expect_is(hdn <- estimateHaze(tera, hazeBands = 1, darkProp = .02, maxSlope = FALSE, plot = FALSE),  "numeric")
            expect_equal(hdn, c(layer = 5))
            expect_is(hdn <- estimateHaze(tera, hazeBands = 1, darkProp = .02, maxSlope = TRUE, plot = FALSE),  "numeric")
            expect_equal(hdn, c(layer = 4))
        })

