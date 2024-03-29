context("radCor")

mtlFile <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package = "RStoolbox")

test_that("radiance conversion", {
  expect_is(rc <- radCor(lsat, metaData = mtlFile, method = "rad"), "SpatRaster")
  expect_equal(names(rc), gsub("dn", "tra", names(lsat)))
  rcm <- rc[]
  rra <- apply(rcm, 2, range, na.rm = TRUE)
  expect_equivalent(colSums(is.na(rcm)) < ncell(rc), rep(TRUE, nlyr(rc)), info = "one or more layers are filled with NA")
  expect_false(any(rra < 0))
  expect_false(any(rra > 125))
})

test_that("reflectance conversion", {
  expect_is(rc <- radCor(lsat, metaData = mtlFile, method = "apref"), "SpatRaster")
  expect_equal(names(rc), paste0(gsub("dn", "", names(lsat)), c(rep("tre", 5), "bt", "tre")))
  rcm <- rc[]
  rra <- apply(rcm, 2, range, na.rm = TRUE)
  expect_equivalent(colSums(is.na(rcm)) < ncell(rc), rep(TRUE, nlyr(rc)), info = "one or more layers are filled with NA")
  expect_false(any(rra < 0))
  expect_false(any(rra[, c(1:5, 7)] > 1))
  expect_false(any(rra[, 6] > 300))

  ## Process only subsets of layers
  expect_is(rc <- radCor(lsat, metaData = mtlFile, bandSet = c("B1_dn"), method = "apref"), "SpatRaster")
  expect_equal(nlyr(rc), 1L)
  expect_is(rc <- radCor(lsat, metaData = mtlFile, bandSet = c("B1_dn", "B2_dn"), method = "apref"), "SpatRaster")
  expect_equal(nlyr(rc), 2L)
  expect_is(rc <- radCor(lsat, metaData = mtlFile, bandSet = c("B1_dn", "B2_dn", "B6_dn"), method = "apref"), "SpatRaster")
  expect_equal(nlyr(rc), 3L)

})


# test_that("DOS approaches", {
#   suppressWarnings({
#     expect_error(rc <- radCor(lsat, metaData = mtlFile, method = "sdos"), "Please specify the bands")
#     expect_error(rc <- radCor(lsat, metaData = mtlFile, method = "sdos", hazeValues = c(1, 2), hazeBands = 1), "hazeBands and hazeValues are not of the same length")
#     expect_error(rc <- radCor(lsat, metaData = mtlFile, method = "sdos", hazeValues = c(dummy = 1), hazeBands = 1), "Names of hazeValues do not correspond to hazeBands")
#
#     toaref <- radCor(lsat, mtlFile, "apref")[]
#
#     hb <- if (identical(Sys.getenv("NOT_CRAN"), "true")) list(1, 2, 2:3, c(2, 4), "B1_dn", c("B1_dn", "B2_dn"), c("B3_dn", "B1_dn")) else list(1)
#     for (i in seq_along(hb)) {
#       expect_is(rc <- radCor(lsat, metaData = mtlFile, method = "sdos", hazeBands = hb[[i]]), "SpatRaster", info = paste0("hazeBands=", hb[[i]], collapse = ","))
#       expect_equal(names(rc), paste0(gsub("dn", "", names(lsat)), c(rep("sre", 5), "bt", "sre")))
#       rcm <- rc[]
#       rra <- apply(rcm, 2, range, na.rm = TRUE)
#       expect_equivalent(colSums(is.na(rcm)) < ncell(rc), rep(TRUE, nlyr(rc)), info = paste0("one or more layers are filled with NA! hazeBands = ", hb[[i]], collapse = ","))
#       notdos <- if (is.character(hb[[i]])) which(!names(lsat) %in% hb[[i]]) else setdiff(1:7, hb[[i]])
#       expect_equivalent(colSums(rcm[, notdos] - toaref[, notdos]) > 0, rep(FALSE, length(notdos))) # test bands that are not 'dos'ified are equal to just apref
#     }
#
#     hb <- list(1, 2)
#     for (method in c("dos", "costz")) {
#       if (method == "dos") expect_error(rc <- radCor(lsat, metaData = mtlFile, method = method, hazeBands = c(1, 2)), "Method 'dos' not yet supported for sensor LANDSAT5")
#       for (i in seq_along(hb)) {
#         i <- 1
#         expect_is(rc <- radCor(lsat, metaData = mtlFile, method = method, hazeBands = hb[[i]]), "SpatRaster", info = paste0("hazeBands=", hb[[i]], collapse = ","))
#         expect_equal(names(rc), paste0(gsub("dn", "", names(lsat)), c(rep("sre", 5), "bt", "sre")))
#         rcm <- rc[]
#         rra <- apply(rcm, 2, range, na.rm = TRUE)
#         expect_equivalent(colSums(is.na(rcm)) < ncell(rc), rep(TRUE, nlayers(rc)), info = paste0("one or more layers are filled with NA! hazeBands = ", hb[[i]], collapse = ","))
#         if (method == "dos") expect_equivalent(colSums(rcm[, 5:7] - toaref[, 5:7]) > 0, rep(FALSE, 3), info = paste(method, "| i=", i, "| hazeBands=", hb[[i]])) # test bands that are not 'dos'ified are equal to just apref
#       }
#     }
#
#     expect_error(radCor(img = lsat, metaData = mtlFile, method = "dos", hazeValues = 1, hazeBands = 1), "Lhaze is < 0")
#     expect_warning(radCor(img = lsat, metaData = mtlFile, method = "dos", hazeValues = c(55, 56), hazeBands = 1:2), "Truncating hazeValues")
#   })
# })

