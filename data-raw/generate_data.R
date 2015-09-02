library(raster)
library(RStoolbox)
## Rlogo example data #####################################################################
r       <- brick(system.file("external/rlogo.grd", package="raster"))
rlogo   <- brick(r)  ## to remove loval file connections etc (not achieved by readAll)
rlogo[] <- r[]
save(rlogo, file = "data/rlogo.rda", compress = "bzip2")

## Landsat Example Data ###################################################################
tdir <- paste0("/tmp/rstbx_lsat", sample(1:1e4,1))
dir.create(tdir)
file.copy(list.files("data-raw/LandsatExample/", full = T), tdir)
files <- list.files(tdir, "TIF", full = TRUE)
f     <- lapply(files, untar, exdir = tdir)
r     <- stackMeta(list.files(tdir, "MTL", full = TRUE))

# plot(r,5)
# ex <- drawExtent()
ex <- new("Extent"
        , xmin = 619400
        , xmax = 628000
        , ymin = -419500
        , ymax = -410200
)
rex <- crop(r, ex)
for(i in 1:7) writeRaster(rex[[i]], filename = paste0("inst/external/landsat/", gsub(".tar.gz", "", basename(files[i]))), datatype="INT1U", overwrite = TRUE)
mtl <- list.files("data-raw/LandsatExample", "MTL", full = TRUE)
file.copy(mtl, paste0("inst/external/landsat/", basename(mtl)), overwrite = TRUE)
files <- list.files("inst/external/landsat/", "tif", full = TRUE)
file.rename(files, gsub("tif", "TIF", files))

#lsatf <- writeRaster(lsat, "/tmp/test.grd", datatype = "INT1U", overwrite = TRUE)
#lsato <- readAll(lsatf)
#size(lsato)
#save(lsato, file = "test.rda", compress = "bzip2")  #257Kb


## SRTM Example Data #######################################################################
dem   <- raster("data-raw/LandsatExample/s04_w050_1arc_v3.tif")
lsat  <- stackMeta("inst/external/landsat/LT52240631988227CUB02_MTL.txt")
dems  <- projectRaster(dem, lsat)
dh    <- writeRaster(dems, "/tmp/test.grd", datatype = "INT1U", overwrite=T)
srtm  <- readAll(dh)
save(srtm, file = "data/srtm.rda", compress = "bzip2")


