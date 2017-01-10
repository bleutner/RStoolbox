context("rsOpts & .vMessage")

test_that("setting global options",{
           expect_false(options("RStoolbox.verbose")[[1]]) 
           expect_silent(.vMessage("MIC CHECK 12"))
           expect_silent(rsOpts(verbose = TRUE))
           expect_true(options("RStoolbox.verbose")[[1]]) 
           expect_message(.vMessage("MIC CHECK 12"), "MIC CHECK 12")
           expect_silent(rsOpts(verbose = FALSE))
        })

