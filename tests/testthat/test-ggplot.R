context("ggplot: ggR, ggRGB & fortify")
library(terra)

lsat <- lsat_rs
rlogo <- rlogo_rs

for(lib in c("terra", "raster")) {
  test_that("ggR returns proper ggplot2 classes or data.frames", {
    suppressWarnings({
      tests  <- expand.grid(forceCat = c(TRUE, FALSE), anno = c(TRUE, FALSE), ggLayer = c(TRUE, FALSE), ggObj = c(TRUE,FALSE))
      builds <- lapply(1:nrow(tests), function(i) ggR(rlogo, forceCat = tests$forceCat[i], ggObj = tests$ggObj[i], geom_raster = !tests$anno[i], ggLayer = tests$ggLayer[i]))
      tinfo  <- paste0("forceCat=", tests[,1], ", anno=", tests[,2], ", ggLayer=", tests[,3], ", ggObj=", tests[,4])

      ## Annotation vs geom_raster
      for(s in which(with(tests, ggObj & !ggLayer))) expect_is(builds[[s]], c("gg", "ggplot"), info = tinfo[s])

      ## ggLayers
      if(!inherits(builds[[which(with(tests, ggObj & ggLayer))[1]]], "ggproto")){
        ## Current ggplot2 release version
        for(s in which(with(tests, ggObj & ggLayer))) expect_is(builds[[s]], c("proto"), info = tinfo[s])
        for(s in which(with(tests, ggObj & ggLayer & anno)))  expect_equal(builds[[s]]$geom$objname, "raster_ann", info = tinfo[s])
        for(s in which(with(tests, ggObj & ggLayer & !anno))) expect_equal(builds[[s]]$geom$objname, "raster", info = tinfo[s])
      } else {
        ## Upcoming ggplot2 version (>=1.0.1.9002)
        for(s in which(with(tests, ggObj & ggLayer & anno)))  expect_is(builds[[s]]$geom, "GeomRasterAnn", info = tinfo[s])
        for(s in which(with(tests, ggObj & ggLayer & !anno))) expect_is(builds[[s]]$geom, "GeomRaster", info = tinfo[s])
      }
      ## Data.frames
      for(s in  which(with(tests, !ggObj))) expect_is(builds[[s]], "data.frame", info = tinfo[s])
      for(s in  which(with(tests, !ggObj & forceCat))) expect_is(builds[[s]][,3], "factor", info = tinfo[s])
      for(s in  which(with(tests, !ggObj ))) expect_is(builds[[s]]$fill, "character", info = tinfo[s])

    })
  })
  
  test_that("ggR works with single valued rasters", {
    r <- rast(vals = 1, ncol = 2, nrow = 1)[[c(1,1,1)]]
    suppressWarnings(r[[1]][]<- NA)
    r[[2]][]<- 17
    r[[3]][]<- c(NA,2)
    
    for(i in 1:3) {
      expect_is(ggR(r,i), c("gg", "ggplot"))
    }
    for(i in 1:3) {
      expect_is(ggR(r,i,geom_raster = TRUE), c("gg", "ggplot"))
    }
    
    ## All NAs
    expect_equal(sum(is.na(ggR(r,1, ggObj = FALSE)[,c("value", "fill")])), 4)
    
    ## Single value
    gp <- ggR(r, 2, ggObj = FALSE)
    expect_equal(unique(gp[, "fill"]), "#FFFFFFFF")  ## fill colour
    expect_equal(unique(gp[, 3]), 17) ## actual value
    
    ## Single values + NAs
    gp <- ggR(r, 3, ggObj = FALSE)
    expect_equal(gp[,"fill"], c(NA, "#FFFFFFFF"))  ## fill colour
    expect_equal(gp[,3], c(NA, 2)) ## actual values
  })
  
  
  test_that("ggRGB returns proper ggplot2 classes or data.frames", {
    
    tests  <- expand.grid(anno = c(TRUE, FALSE), ggLayer = c(TRUE, FALSE), ggObj = c(TRUE,FALSE), stretch=c("sqrt", "hist", "log", "lin"))
    builds <- lapply(1:nrow(tests), function(i) ggRGB(rlogo, ggObj = tests$ggObj[i], geom_raster = !tests$anno[i], ggLayer = tests$ggLayer[i], stretch = tests$stretch[i] ))            
    tinfo <- paste0("anno=", tests$anno, ", ggLayer=", tests$ggLayer, ", ggObj=", tests$ggObj)
    
    ## Stand-alone
    for(s in which(with(tests, ggObj & !ggLayer))) expect_is(builds[[s]], c("gg", "ggplot"), info = tinfo[s])
    
    ## ggLayers
    if(!inherits(builds[[which(with(tests, ggObj & ggLayer))[1]]], "ggproto")){
      ## Current ggplot2 release version
      for(s in which(with(tests, ggObj & ggLayer))) expect_is(builds[[s]], c("proto"), info = tinfo[s])
      for(s in which(with(tests, ggObj & ggLayer & anno)))  expect_equal(builds[[s]]$geom$objname, "raster_ann", info = tinfo[s])
      for(s in which(with(tests, ggObj & ggLayer & !anno))) expect_equal(builds[[s]]$geom$objname, "raster", info = tinfo[s])                       
    } else {
      ## Upcoming ggplot2 version (>=1.0.1.9002)
      for(s in which(with(tests, ggObj & ggLayer & anno)))  expect_is(builds[[s]]$geom, "GeomRasterAnn", info = tinfo[s])
      for(s in which(with(tests, ggObj & ggLayer & !anno))) expect_is(builds[[s]]$geom, "GeomRaster", info = tinfo[s])                       
    }    
    
    ## Data.frames
    for(s in  which(with(tests, !ggObj))) expect_is(builds[[s]], "data.frame", info = tinfo[s])
    for(s in  which(with(tests, !ggObj))) expect_is(builds[[s]]$fill, "character", info = tinfo[s])
    
  })
  
  test_that("fortify.raster returns proper data.frames", {
    df <- fortify(rlogo)
    expect_named(df, c("x", "y", "red", "green", "blue"))       
    expect_identical(nrow(df), 7777L)  
    ## Single layer
    expect_named(fortify(rlogo[[1]]), c("x", "y", "red"))
  })
  
}