library(testthat)
library(RStoolbox) 


Sys.setenv("R_TESTS" = "") ## needed to pass R CMD check: https://github.com/hadley/testthat/issues/144

test_check("RStoolbox")


