context("rasterCVA")

r <- raster(val = 0, ncol = 2, nrow = 10)
r1 <- r2 <- stack(r,r)
s = 4
x <- c(0,s,s,s,0,-s,-s,-s, NA, 0)
y <- c(s,s,0,-s,-s,-s,0,s, 0, NA)
r2[[1]][] <- c(x, x + sign(x)*2)
r2[[2]][] <- c(y, y + sign(y)*2)

expectedDf <- as.matrix(data.frame(angle = c(0,45,90,135,180,225,270,315,NA,NA),
                magnitude = c(rep(c(s, sqrt(2*s^2)), 4), NA,NA, rep(c((2+s), sqrt(2*(2+s)^2)), 4), NA,NA)))

test_that("angles and magnitudes are correct (incl. NA treatment)", {
        expect_is(cva <- rasterCVA(r1,r2, tmf = 0), "RasterBrick")
        expect_equal(cva[], expectedDf)
    })