context("Entropy")

data(rlogo)

entro <- function(x){
    p <- table(x)/length(x)
    -sum(p*log(p))
}

test_that("entropyCpp is correct",
        expect_equal(rasterEntropy(rlogo), {x <- calc(rlogo, entro);names(x)<-"entropy";x})
)
   

