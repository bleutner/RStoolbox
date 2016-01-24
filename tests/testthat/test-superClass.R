context("superClass")

suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(pls))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(caret))

data(lsat)
lsat <- lsat[[1:4]]

## Set-up test data
set.seed(1)
poly     <- readRDS(system.file("external/trainingPolygons.rds", package="RStoolbox"))
poly$res <- as.numeric(poly$class)
pts      <- spsample(poly, n = 100, type ="random")
pts      <- SpatialPointsDataFrame(pts, data = over(pts, poly))
ll       <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
polyll   <- spTransform(poly, ll)  
ptsll    <- spTransform(pts, ll)  
lsatll   <- projectRaster(lsat, crs = ll)
valInd   <- createDataPartition(poly$class)[[1]]

trainList <- list(projected = list(polygons = poly, points = pts, img = lsat), 
        geographical = list(polygons = polyll, points = ptsll, img = lsatll))




## Maximum likelihood custom model
test_that("maximum likelihood model",
        expect_is(superClass(lsat, trainData = poly, responseCol = "class", model = "mlc", trainPartition = 0.7, predict = FALSE), "superClass")
) 


## Projection checks
test_that("projection mismatch errors", 
        expect_error(superClass(trainList[[1]]$img, trainData = trainList[[2]]$polygons, responseCol = "class"), "Projection of trainData does not match img")
)


for(proj in c("projected", "geographical")){
    for(type in c("polygons", "points")){
        
        info <- paste(c("train type = ", type, " | coordinates = ", proj), collapse = "")       
        train <- trainList[[proj]][[type]]
        img   <- trainList[[proj]][["img"]]
        
        ## No prediction	
        set.seed(1)
        sc <- superClass(img, trainData = train, nSamples = 50, responseCol = "class", model = "pls", 
                tuneGrid = data.frame(ncomp = 3), tuneLength = 1, trainPartition = 0.7, predict = FALSE)
        
        ## with prediction
        set.seed(1)	
        sc2 <- superClass(img, trainData = train, nSamples = 50, responseCol = "class", model = "pls", 
                tuneGrid = data.frame(ncomp = 3), tuneLength = 1, trainPartition = 0.7, predict = TRUE)	
        
        ## Based on numeric classes
        set.seed(1)
        sc3 <- superClass(img, trainData = train,  nSamples = 50,responseCol = "res", model = "pls", 
                tuneGrid = data.frame(ncomp = 3), tuneLength = 1, trainPartition = 0.7, predict = TRUE)
        
        test_that("validation raster vs pixel based, numeric vs. factor predictors",{
                    expect_is(sc$map, "character", info = info)
                    expect_equal(sc$validation, sc2$validation, info = info)								
                }) 
        
        test_that("numeric vs. factor predictors", {						
                    expect_equal(cellStats(sc2$map - sc3$map, sum), 0, info = info)
                })
        
        
        test_that("predict.superClass map is identical to superClass$map",{
                    expect_true(compareRaster(sc2$map, predict(sc2, img), values = TRUE), info = info)                      
                })
        
        test_that("prediction of probability layers", {   
                    expect_is(sc4  <- superClass(img, trainData = train, nSamples = 50, ntree = 200, responseCol = "class", model = "rf",
                                    predType = "prob", tuneLength = 1, predict = T),  "superClass", info = info)
                    expect_is(scp <- predict(sc4, img, predType = "prob"), "RasterBrick")
                    expect_identical(nlayers(sc4$map), 4L, info=info)
                    expect_true(compareRaster(sc4$map, scp, values = T), info = info) 
                    expect_equal(unique(round(sum(sc4$map))) , 1, info = info)			
                })
        
        test_that("external valData instead of trainPartition",{
                    expect_is(sc <- superClass(img, trainData = train[valInd,], valData = train[-valInd,], nSamples = 50, responseCol = "class", model = "pls", 
                                    mode = "classification", predict = FALSE), "superClass", info = info)	
                    expect_is(sc$validation$performance, "confusionMatrix")
                    
                }) 
        
        
        test_that("regression mode works", {
                    expect_is(sc2 <- superClass(img, trainData = train, trainPartition=.7, nSamples = 50, responseCol = "res", model = "pls", 
                                    mode = "regression", predict = FALSE), "superClass", info = info)
                })
        
        
        
    }
    
}


