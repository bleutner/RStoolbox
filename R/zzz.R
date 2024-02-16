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
  assign("lsat", lsat_rs, envir=as.environment("package:RStoolbox"))

  srtml_rs <- terra::readRDS(system.file("external", "srtm_lsat.rds", package = pkgname))
  assign("srtm", srtml_rs, envir=as.environment("package:RStoolbox"))

  rlogo_rs <- terra::readRDS(system.file("external", "rlogo.rds", package = pkgname))
  assign("rlogo", rlogo_rs, envir=as.environment("package:RStoolbox"))

  sen2_rs <- terra::readRDS(system.file("external", "sen2.rds", package = pkgname))
  assign("sen2", sen2_rs, envir=as.environment("package:RStoolbox"))

  srtms_rs <- terra::readRDS(system.file("external", "srtm_sen2.rds", package = pkgname))
  assign("srtm_sen2", srtms_rs, envir=as.environment("package:RStoolbox"))

}
