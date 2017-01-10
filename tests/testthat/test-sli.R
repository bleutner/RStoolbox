context("readSLI & writeSLI")
sliFile <- system.file("external/vegSpec.sli", package="RStoolbox")
sliTmpFile <- paste0(tempdir(),"/spectralLibraryFile", Sys.getpid(),".sli")

test_that("read and write are compatible", {
            expect_is(sli <- readSLI(sliFile), "data.frame") 
            
            expect_equal(names(sli), c("wavelength", "veg_stressed", "veg_vital"))
            expect_equal(nrow(sli), 2151)
            
            for(mode in c("bin", "ASCII")) {
                expect_silent(writeSLI(sli, sliTmpFile, wavl.units = "Nanometers", mode = mode))
                sliR <- readSLI(sliTmpFile)
                expect_equal(names(sli), c("wavelength", "veg_stressed", "veg_vital"))
                expect_equal(nrow(sli), 2151)
                expect_equal(sli, sliR)
                file.remove(list.files(tempdir(), basename(sliTmpFile), full = TRUE))
            }
            
            
            
        })


