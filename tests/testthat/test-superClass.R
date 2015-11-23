context("superClass")

data(rlogo)
train <- readRDS(system.file("external/training.rds", package="RStoolbox"))

## No prediction
set.seed(1)
sc <- superClass(rlogo, trainData = train, responseCol = "class", model = "rf", tuneLength = 1, trainPartition = 0.7, predict = FALSE)
test_that("superClass without prediction",
        expect_is(sc$map, "character")
)

## With prediction
set.seed(1)
sc2 <- superClass(rlogo, trainData = train, responseCol = "class", model = "rf", tuneLength = 1, trainPartition = 0.7, predict = TRUE)
test_that("validation is identical between sample pixels approach and spatial predict approach",
        expect_equal(sc$validation, sc2$validation)
)

## predict.superClass
test_that("predict.superClass map is identical to superClass$map",{
            compareRaster(sc2$map, predict(sc, rlogo), values = TRUE)  
            expect_equal(nlayers(predict(sc, rlogo, predType = "prob")), 3L)
        }
)
