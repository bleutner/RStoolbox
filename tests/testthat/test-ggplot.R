context("ggplot")


test_that("ggR returns ggplot2 classes", {
            data(rlogo) 
            
            tests <- expand.grid(forceCat = c(TRUE, FALSE), anno = c(TRUE, FALSE), ggLayer = c(TRUE, FALSE), ggObj = c(TRUE,FALSE))
            builds <- lapply(1:nrow(tests), function(i) ggR(rlogo, forceCat = tests$forceCat[i], ggObj = tests$ggObj[i], annotation = tests$anno[i], ggLayer = tests$ggLayer[i]))            
            tinfo <- paste0("forceCat=", tests[,1], ", anno=", tests[,2], ", ggLayer=", tests[,3], ", ggObj=", tests[,4])
          
            ## Annotation vs geom_raster    
            for(s in which(with(tests, ggObj & !ggLayer))) expect_is(builds[[s]], c("gg", "ggplot"), info = tinfo[s])

            ## ggLayers
            for(s in which(with(tests, ggObj & ggLayer))) expect_is(builds[[s]], "proto", info = tinfo[s])
            for(s in which(with(tests, ggObj & ggLayer & anno))) expect_equal(builds[[s]]$geom$objname, "raster_ann", info = tinfo[s])
            for(s in which(with(tests, ggObj & ggLayer & !anno))) expect_equal(builds[[s]]$geom$objname, "raster", info = tinfo[s])
                      
            ## Data.frames
            for(s in  which(with(tests, !ggObj))) expect_is(builds[[s]], "data.frame", info = tinfo[s])
            for(s in  which(with(tests, !ggObj & forceCat))) expect_is(builds[[s]][,3], "factor", info = tinfo[s])
            for(s in  which(with(tests, !ggObj & forceCat))) expect_is(builds[[s]][,4], "character", info = tinfo[s])
  
            
        })
