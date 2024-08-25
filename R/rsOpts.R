#' Set global options for RStoolbox
#' 
#' shortcut to options(RStoolbox.*)
#' 
#' @param verbose Logical. If \code{TRUE} many functions will print status messages about the current processing step. By default verbose mode is disabled.
#' @param idxdb List. The list conatins the formal calculation of spectral indices. Modify this list to pipe your own spectral index through the internal C++ calculation of RStoolbox. Call \code{.initIDXdb} to reset the list.
#' @export
#' @return
#' No return, just a setter for the verbosiness of the RStoolbox package
#' @examples 
#' rsOpts(verbose=TRUE)
#'
rsOpts <- function(verbose=NULL, idxdb=NULL){
    if(!is.null(verbose)){
        options(RStoolbox.verbose=verbose)
    }
    if(!is.null(idxdb)){
        options(RStoolbox.idxdb=idxdb)

        .IDXdb <- force(getOption("RStoolbox.idxdb"))
    }
}


