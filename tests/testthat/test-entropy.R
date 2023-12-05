context("Entropy")

data(lsat_t)

data(rlogo)

data(lsat)
writeRaster(rast(lsat), "data/lsat_t.tif")
library(RStoolbox)
data(lsat_t, package = "RStoolbox")

entro <- function(x){
    p <- table(x)/length(x)
    -sum(p*log(p))
}

test_that("entropyCpp is correct",
    expect_equal(rasterEntropy(rlogo), {
        x <- app(rlogo, entro)
        names(x)<-"entropy";
        x
    })
)
   

