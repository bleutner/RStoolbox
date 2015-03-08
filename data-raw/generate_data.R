r <- brick(system.file("external/rlogo.grd", package="raster"))
rlogo <- brick(r)  ## to remove loval file connections etc (not achieved by readAll)
rlogo[] <- r[]
save(rlogo, file = "data/rlogo.rda", compress = "bzip2")

