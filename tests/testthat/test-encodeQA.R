context("test-encodeqa")

test_that("encodeQA OLI collection1 pixel value matching", {
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "na", cloudMask = "no", cloud = "na", cloudShadow = "na",
                        snow = "na", cirrus = "na", sensor = "OLI", legacy = "collection1"), 0)
  expect_equal(encodeQA(fill = "yes", terrainOcclusion = "no", radSaturation = "na", cloudMask = "no", cloud = "na", cloudShadow = "na",
                        snow = "na", cirrus = "na", sensor = "OLI", legacy = "collection1"), 1)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "yes", radSaturation = "na", cloudMask = "no", cloud = "na", cloudShadow = "na",
                        snow = "na", cirrus = "na", sensor = "OLI", legacy = "collection1"), 2)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "na", cloudMask = "no", cloud = "low", cloudShadow = "low",
                        snow = "low", cirrus = "low", sensor = "OLI", legacy = "collection1"), 2720)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "low", cloudMask = "yes", cloud = "high", cloudShadow = "low",
                        snow = "low", cirrus = "low", sensor = "OLI", legacy = "collection1"), 2804)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "high", cloudMask = "no", cloud = "low", cloudShadow = "high",
                        snow = "low", cirrus = "low", sensor = "OLI", legacy = "collection1"), 2988)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "na", cloudMask = "no", cloud = "low", cloudShadow = "low",
                        snow = "high", cirrus = "low", sensor = "OLI", legacy = "collection1"), 3744)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "low", cloudMask = "no", cloud = "low", cloudShadow = "low",
                        snow = "high", cirrus = "low", sensor = "OLI", legacy = "collection1"), 3748)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "na", cloudMask = "no", cloud = "low", cloudShadow = "high",
                        snow = "low", cirrus = "high", sensor = "OLI", legacy = "collection1"), 7072)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "low", cloudMask = "no", cloud = "low", cloudShadow = "high",
                        snow = "low", cirrus = "high", sensor = "OLI", legacy = "collection1"), 7076)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "high", cloudMask = "no", cloud = "med", cloudShadow = "high",
                        snow = "low", cirrus = "high", sensor = "OLI", legacy = "collection1"), 7116)
})

test_that("encodeQA TM collection1 pixel value matching", {
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "na", cloudMask = "no", cloud = "na", cloudShadow = "na",
                        snow = "na", sensor = "TM", legacy = "collection1"), 0)
  expect_equal(encodeQA(fill = "yes", terrainOcclusion = "no", radSaturation = "na", cloudMask = "no", cloud = "na", cloudShadow = "na",
                        snow = "na", sensor = "TM", legacy = "collection1"), 1)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "yes", radSaturation = "na", cloudMask = "no", cloud = "na", cloudShadow = "na",
                        snow = "na", sensor = "TM", legacy = "collection1"), 2)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "na", cloudMask = "no", cloud = "low", cloudShadow = "low",
                        snow = "low", sensor = "TM", legacy = "collection1"), 672)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "yes", radSaturation = "na", cloudMask = "no", cloud = "low", cloudShadow = "low",
                        snow = "low", sensor = "TM", legacy = "collection1"), 674)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "low", cloudMask = "no", cloud = "low", cloudShadow = "low",
                        snow = "low", sensor = "TM", legacy = "collection1"), 676)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "med", cloudMask = "no", cloud = "low", cloudShadow = "low",
                        snow = "low", sensor = "TM", legacy = "collection1"), 680)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "high", cloudMask = "no", cloud = "low", cloudShadow = "low",
                        snow = "low", sensor = "TM", legacy = "collection1"), 684)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "na", cloudMask = "no", cloud = "med", cloudShadow = "low",
                        snow = "low", sensor = "TM", legacy = "collection1"), 704)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "na", cloudMask = "no", cloud = "low", cloudShadow = "low",
                        snow = "high", sensor = "TM", legacy = "collection1"), 1696)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "low", cloudMask = "no", cloud = "low", cloudShadow = "low",
                        snow = "high", sensor = "TM", legacy = "collection1"), 1700)
})


test_that("encodeQA MSS collection1 pixel value matching", {
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "na", cloudMask = "no", cloud = "na", 
                        sensor = "MSS", legacy = "collection1"), 0)
  expect_equal(encodeQA(fill = "yes", terrainOcclusion = "no", radSaturation = "na", cloudMask = "no", cloud = "na", 
                        sensor = "MSS", legacy = "collection1"), 1)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "yes", radSaturation = "na", cloudMask = "no", cloud = "na", 
                        sensor = "MSS", legacy = "collection1"), 2)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "na", cloudMask = "no", cloud = "low", 
                        sensor = "MSS", legacy = "collection1"), 32)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "yes", radSaturation = "na", cloudMask = "no", cloud = "low", 
                        sensor = "MSS", legacy = "collection1"), 34)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "low", cloudMask = "no", cloud = "low", 
                        sensor = "MSS", legacy = "collection1"), 36)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "med", cloudMask = "no", cloud = "low", 
                        sensor = "MSS", legacy = "collection1"), 40)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "high", cloudMask = "no", cloud = "low", 
                        sensor = "MSS", legacy = "collection1"), 44)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "na", cloudMask = "no", cloud = "med", 
                        sensor = "MSS", legacy = "collection1"), 64)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "low", cloudMask = "no", cloud = "med", 
                        sensor = "MSS", legacy = "collection1"), 68)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "med", cloudMask = "no", cloud = "med", 
                        sensor = "MSS", legacy = "collection1"), 72)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "high", cloudMask = "no", cloud = "med", 
                        sensor = "MSS", legacy = "collection1"), 76)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "na", cloudMask = "yes", cloud = "high", 
                        sensor = "MSS", legacy = "collection1"), 112)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "low", cloudMask = "yes", cloud = "high", 
                        sensor = "MSS", legacy = "collection1"), 116)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "med", cloudMask = "yes", cloud = "high", 
                        sensor = "MSS", legacy = "collection1"), 120)
  expect_equal(encodeQA(fill = "no", terrainOcclusion = "no", radSaturation = "high", cloudMask = "yes", cloud = "high", 
                        sensor = "MSS", legacy = "collection1"), 124)
  
})


test_that("encodeQA OLI pre_collection pixel value matching", {
  expect_equal(encodeQA(fill = "no", droppedFrame = "no", terrainOcclusion = "no", water = "na", snow = "na", cirrus = "na", cloud = "na",
                        sensor = "OLI", legacy = "pre_collection"), 0)
  expect_equal(encodeQA(fill = "yes", droppedFrame = "no", terrainOcclusion = "no", water = "na", snow = "na", cirrus = "na", cloud = "na",
                        sensor = "OLI", legacy = "pre_collection"), 1)
  expect_equal(encodeQA(fill = "no", droppedFrame = "yes", terrainOcclusion = "no", water = "na", snow = "na", cirrus = "na", cloud = "na",
                        sensor = "OLI", legacy = "pre_collection"), 2)
  expect_equal(encodeQA(fill = "no", droppedFrame = "no", terrainOcclusion = "no", water = "na", snow = "na", cirrus = "high", cloud = "high",
                        sensor = "OLI", legacy = "pre_collection"), 61440)
  expect_equal(encodeQA(fill = "no", droppedFrame = "no", terrainOcclusion = "no", water = "na", snow = "high", cirrus = "high", cloud = "high",
                        sensor = "OLI", legacy = "pre_collection"), 64512)
})