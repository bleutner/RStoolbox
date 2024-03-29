#' Set global options for RStoolbox
#' 
#' shortcut to options(RStoolbox.*)
#' 
#' @param verbose Logical. If \code{TRUE} many functions will print status messages about the current processing step. By default verbose mode is disabled.
#' @export
#' @return
#' No return, just a setter for the verbosiness of the RStoolbox package
#' @examples 
#' rsOpts(verbose=TRUE)
#'
rsOpts <- function(verbose){
    options(RStoolbox.verbose=verbose)
}


