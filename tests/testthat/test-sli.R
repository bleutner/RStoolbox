context("readSLI & writeSLI")
sliFile <- system.file("external/vegSpec.sli", package="RStoolbox")
sliTmpFile <- paste0(tempdir(),"/spectralLibraryFile", Sys.getpid(),".sli")

test_that("read and write are compatible", {
  expect_is(sli <- readSLI(sliFile), "data.frame") 
  
  expect_equal(names(sli), c("wavelength", "veg_stressed", "veg_vital"))
  expect_equal(nrow(sli), 2151)
  
  for(mode in c("bin", "ASCII")) {
    for(endian in c("little", "big")) {
      if(mode=="ASCII" && endian=="big") next
      expect_silent(writeSLI(sli, sliTmpFile, wavl.units = "Nanometers", mode = mode, endian = endian))
      
      sliR <- readSLI(sliTmpFile) 
      expect_equal(names(sli), c("wavelength", "veg_stressed", "veg_vital"))
      expect_equal(nrow(sli), 2151)
      expect_equal(sli, sliR)
      
      if(mode=="bin") {
        file.rename(paste0(sliTmpFile, ".hdr"), gsub(".sli", ".hdr", sliTmpFile))
        expect_is(readSLI(sliTmpFile) , "data.frame")
      }
      
      file.remove(list.files(tempdir(), basename(sliTmpFile), full = TRUE))
    }
  }
})


test_that("spectra labels are parsed correctly", {
    skip_on_cran()
    for(s in letters[1:4]) {
        expect_is(sli <- readSLI(paste0("testdata/sli/", s, ".sli")), "data.frame", info = paste0("failed: ", s, ".sli"))
        expect_equal(colnames(sli), c("wavelength", "spectrum_a", "spectrum_b"))
        expect_equal(unlist(sli[1,]), c(wavelength = 350, spectrum_a = 0.008958003153785, spectrum_b = 0.00883699393591312  ))
    }
})
