context("mlc")
suppressPackageStartupMessages(library(caret))

set.seed(1)
mat <- matrix(rnorm(300), ncol = 3, nrow = 100)
y <- sample(factor(c("a", "b")), 100, replace = TRUE)


test_that("fit mlc",{
          expect_is( mr <- mlc(mat,y), "list")
          expect_equal(names(mr), c("a", "b", "levels")) 
          expect_equal(vapply(mr$a, length, numeric(1)), c(m=3,D=1,I=9))   
      })

test_that("predict mlc",{
                      mod <- train( mat, y, method = mlcCaret, trControl = trainControl(method = "none"))
                      expect_is(pred <- predict.mlc(mod, mat), "factor")
                      expect_equal(length(pred), nrow(mat))
                      expect_equal(levels(pred), c("a", "b"))
                      expect_is(prob <- predict.mlc.prob(mod, mat), "matrix")
                      expect_equal(nrow(prob), nrow(mat))
                      expect_equal(ncol(prob), 2)
                  })  
          
          