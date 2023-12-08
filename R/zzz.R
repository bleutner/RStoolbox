.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This is version ", packageVersion(pkgname), " of ", pkgname)

  packageStartupMessage("
        ____       __              ____
       / __ \\_____/ /_____  ____  / / /_  ____  _  __
      / /_/ / ___/ __/ __ \\/ __ \\/ / __ \\/ __ \\| |/_/
     / _, _(__  ) /_/ /_/ / /_/ / / /_/ / /_/ />  <
    /_/ |_/____/\\__/\\____/\\____/_/_.___/\\____/_/|_|
  ")

  lsat_rs <- terra::unwrap(readRDS("data/lsat.rds"))
  assign("lsat_rs", lsat_rs, envir=as.environment("package:RStoolbox"))

  rlogo_rs <- terra::unwrap(readRDS("data/rlogo.rds"))
  assign("rlogo_rs", rlogo_rs, envir=as.environment("package:RStoolbox"))

  srtm_rs <- terra::unwrap(readRDS("data/srtm.rds"))
  assign("srtm_rs", srtm_rs, envir=as.environment("package:RStoolbox"))



}