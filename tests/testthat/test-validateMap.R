context("validateMap")

library(raster)
suppressPackageStartupMessages(library(pls))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(caret))

data(lsat)
lsat <- lsat[[1:4]]

## Set-up test data
set.seed(1)
poly     <- readRDS(system.file("external/trainingPolygons.rds", package="RStoolbox"))
poly$classNum <- as.numeric(poly$class)

sc <- superClass(lsat, trainData = poly, nSample = 100, responseCol = "class", model = "mlc", trainPartition = 0.7, predict = TRUE)

test_that("classification, without class mapping",{
val <- validateMap(sc$map, valData = poly, nSample = 100, responseCol = "classNum", classMapping = NULL)
expect_is(val, "mapValidation")
expect_equal(lapply(val, "class"), list(performance="confusionMatrix",validationSet = "data.frame"))
expect_equal(colnames(val$validationSet), c("reference", "prediction", "cell"))
})

test_that("classification, with class mapping",{
            val <- validateMap(sc$map, valData = poly, nSample = 100, responseCol = "class", classMapping = sc$classMapping)
            expect_is(val, "mapValidation")
            expect_equal(lapply(val, "class"), list(performance="confusionMatrix",validationSet = "data.frame"))
            expect_equal(colnames(val$validationSet), c("reference", "prediction", "cell"))
        })

test_that("regression",{
            val <- validateMap(sc$map, valData = poly, nSample = 100, mode = "regression", responseCol = "classNum", classMapping)
            expect_is(val, "mapValidation")
            expect_equal(lapply(val, "class"), list(performance="numeric",validationSet = "data.frame"))
            expect_equal(colnames(val$validationSet), c("reference", "prediction", "cell"))
            expect_equal(names(val$performance), c("RMSE", "Rsquared"))
            
        }) 