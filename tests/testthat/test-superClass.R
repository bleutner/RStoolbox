context("superClass")
library(raster)
data(lsat)

train <- readRDS(system.file("external/trainingPolygons.rds", package="RStoolbox"))
train$res <- as.numeric(train$class)

## No prediction
set.seed(1)
sc <- superClass(lsat, trainData = train, responseCol = "class", model = "pls", tuneLength = 3, trainPartition = 0.7, predict = FALSE)
test_that("superClass without prediction",
		expect_is(sc$map, "character")
) 

## With prediction
set.seed(1)
sc2 <- superClass(lsat, trainData = train, responseCol = "class", model = "pls",  tuneLength = 3, trainPartition = 0.7, predict = TRUE)
test_that("validation is identical between sample pixels approach and spatial predict approach",
		expect_equal(sc$validation[[1]], sc2$validation[[1]]))

set.seed(1)
sc3 <- superClass(lsat, trainData = train, responseCol = "res", model = "pls",  tuneLength = 3, trainPartition = 0.7, predict = TRUE)
test_that("classification with numeric vs. factor predictors should return identical results",
expect_equal(cellStats(sc2$map - sc3$map, sum), 0, info = "numeric vs. factor predictors"))


## predict.superClass
test_that("predict.superClass map is identical to superClass$map",{
			compareRaster(sc2$map, predict(sc2, lsat), values = TRUE)  
			expect_equal(nlayers(predict(sc, lsat, predType = "prob")), 4L)
		}
)

## Maximum likelihood custom model
set.seed(1)
sc <- superClass(lsat, trainData = train, responseCol = "class", model = "mlc", trainPartition = 0.7, predict = FALSE)
test_that("superClass without maximum likelihood model",
		expect_is(sc, "superClass")
) 

