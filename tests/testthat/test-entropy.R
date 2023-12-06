context("Entropy")

rlogo <- rlogo_rs
lsat <- lsat_rs

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
   

