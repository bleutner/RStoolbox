#' Set global options for RStoolbox
#' 
#' shortcut to options(RStoolbox.*)
#' 
#' @param verbose Logical. If \code{TRUE} many functions will print status messages about the current processing step.
#' @export
#' @examples 
#' \dontrun{
#' rsOpts(verbose=TRUE)
#' }
rsOpts <- function(verbose){
    options(RStoolbox.verbose=verbose)
}