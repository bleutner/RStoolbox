#' Set global options for RStoolbox
#' 
#' shortcut to options(RStoolbox.*)
#' 
#' @param verbose Logical. Make all functions print status messages
#' @export
#' @examples 
#' \dontrun{
#' rsOpts(verbose=TRUE)
#' }
rsOpts <- function(verbose){
    options(RStoolbox.verbose=verbose)
}