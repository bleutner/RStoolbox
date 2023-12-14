.onAttach <- function(libname = find.package("RStoolbox"), pkgname = "RStoolbox") {
  packageStartupMessage("This is version ", utils::packageVersion(pkgname), " of ", pkgname)

  packageStartupMessage("
        ____       __              ____
       / __ \\_____/ /_____  ____  / / /_  ____  _  __
      / /_/ / ___/ __/ __ \\/ __ \\/ / __ \\/ __ \\| |/_/
     / _, _(__  ) /_/ /_/ / /_/ / / /_/ / /_/ />  <
    /_/ |_/____/\\__/\\____/\\____/_/_.___/\\____/_/|_|
  ")

  lsat_rs <- terra::readRDS(system.file("external", "lsat.rds", package = pkgname))
  assign("lsat_rs", lsat_rs, envir=as.environment("package:RStoolbox"))

  rlogo_rs <- terra::readRDS(system.file("external", "rlogo.rds", package = pkgname))
  assign("rlogo_rs", rlogo_rs, envir=as.environment("package:RStoolbox"))

  srtm_rs <- terra::readRDS(system.file("external", "srtm.rds", package = pkgname))
  assign("srtm_rs", srtm_rs, envir=as.environment("package:RStoolbox"))

}
