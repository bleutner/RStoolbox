.onAttach <- function(libname = find.package("RStoolbox"), pkgname = "RStoolbox") {
  packageStartupMessage("This is version ", utils::packageVersion(pkgname), " of ", pkgname)

  packageStartupMessage("
        ____       __              ____
       / __ \\_____/ /_____  ____  / / /_  ____  _  __
      / /_/ / ___/ __/ __ \\/ __ \\/ / __ \\/ __ \\| |/_/
     / _, _(__  ) /_/ /_/ / /_/ / / /_/ / /_/ />  <
    /_/ |_/____/\\__/\\____/\\____/_/_.___/\\____/_/|_|
  ")

  lsat_rs <- terra::unwrap(readRDS("inst/external/lsat.rds"))
  assign("lsat_rs", lsat_rs, envir=as.environment(paste0("package:", pkgname)))

  rlogo_rs <- terra::unwrap(readRDS("inst/external/rlogo.rds"))
  assign("rlogo_rs", rlogo_rs, envir=as.environment(paste0("package:", pkgname)))

  srtm_rs <- terra::unwrap(readRDS("inst/external/srtm.rds"))
  assign("srtm_rs", srtm_rs, envir=as.environment(paste0("package:", pkgname)))



}