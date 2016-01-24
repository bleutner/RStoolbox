context("getValidation")
suppressPackageStartupMessages(library("randomForest"))
suppressPackageStartupMessages(library("sp"))

train <- readRDS(system.file("external/trainingPoints.rds", package="RStoolbox"))
data(rlogo)
train$num <- rnorm(nrow(train))
class   <- superClass(rlogo, trainData = train, responseCol = "class", tuneLength = 1, trainPartition = 0.7, predict = FALSE)
reg     <- superClass(rlogo, trainData = train, responseCol = "num", tuneLength = 1, trainPartition = 0.7, predict = FALSE, mode = "regression")


test_that("getValidation returns correct objects", {
			for(f in c("testset", "cv")){
				expect_is(getValidation(class, from = f), "data.frame")
				expect_equal(nrow(getValidation(class, from = f)), 1L)
				expect_is(getValidation(class, metrics = "classwise", from = f), "data.frame")
				expect_equal(nrow(getValidation(class, metrics = "classwise", from = f)), 3)
				expect_is(getValidation(class, metrics = "confmat", from = f), "table")
				expect_is(getValidation(class, metrics = "caret", from = f), "confusionMatrix")
			}
			for(f in c("testset", "cv")){
				expect_is(getValidation(reg, from = f), "data.frame")
				expect_equal(nrow(getValidation(reg, from = f)), 1L)
			}	
		}
)