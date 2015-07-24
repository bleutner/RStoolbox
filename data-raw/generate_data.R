## Rlogo example data
r       <- brick(system.file("external/rlogo.grd", package="raster"))
rlogo   <- brick(r)  ## to remove loval file connections etc (not achieved by readAll)
rlogo[] <- r[]
save(rlogo, file = "data/rlogo.rda", compress = "bzip2")

## Landsat Example Data
files <- list.files("data-raw/LandsatExample", "TIF", full = TRUE)
r     <- stack(files)

# plot(r,5)
# ex <- drawExtent()
ex <- new("Extent"
		, xmin = 636908.794482874
		, xmax = 644660.440453371
		, ymin = -442178.129427452
		, ymax = -434910.961330111
)
rex <- crop(r, ex)
for(i in 1:7) writeRaster(rex[[i]], filename = paste0("inst/external/landsat/", basename(files[i])), datatype="INT1U", overwrite = TRUE)
mtl <- list.files("data-raw/LandsatExample", "MTL", full = TRUE)
file.copy(mtl, paste0("inst/external/landsat/", basename(mtl)))
files <- list.files("inst/external/landsat/", "tif", full = TRUE)
file.rename(files, gsub("tif", "TIF", files))

