context("save and read RSTBX objects")
library(raster)
data(rlogo)

## Create RSTBX object
train <- readRDS(system.file("external/trainingPoints.rds", package="RStoolbox"))
sc <- superClass(rlogo, train, tuneLength = 1, resp="class")

## Save and re-import
outbase <- paste0(tempdir(),"/test-RSTOOLBOX-sc")
saveRSTBX(sc, outbase , overwrite = TRUE)
sc_re <- readRSTBX(paste0(outbase, ".rds"))
womap <- setdiff(names(sc), "map")

test_that("export and import works",{
        expect_is(sc_re, c("RStoolbox", "superClass"))
        expect_equal(sc[womap], sc_re[womap]) ## need to test $map separately because things like @fromdisk can change
        expect_true(compareRaster(sc_re$map, sc$map, values = TRUE))  
    })