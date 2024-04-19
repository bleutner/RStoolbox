#' Multiple Endmember Spectral Mixture Analysis (Spectral Unmixing)
#' 
#' \code{mesma} performs a spectral mixture anlylsis (SMA) or multiple endmember spectral mixture analysis (MESMA) on a multiband raster image.
#' 
#' @param img SpatRaster. Remote sensing imagery (usually hyperspectral).
#' @param em Matrix or data.frame with spectral endmembers. Columns represent the spectral bands (i.e. columns correspond to number of bands in \code{img}). Rows represent either a single endmember per class (SMA) or multiple endmembers per class (MESMA), if a column with name \code{class} is present, containing the class name each endmember belongs to, e.g. "water" or "land". See details below. Number of rows needs to be > 1.
#' @param method Character. Select an unmixing method. Currently, only "NNLS" is implemented. Default is "NNLS".
#' \itemize{
#'    \item \code{NNLS:} applies a non-negative least squares (NNLS) regression which is using a sequential coordinate-wise algorithm (SCA) based on Franc et al. (2005).
#' }
#' @param iterate Integer. Set maximum iteration per pixel. Processing time could increase the more iterations are made possible. Default is 400.
#' @param tolerance Numeric. Tolerance limit representing a nearly zero minimal number. Default is 1e-8. 
#' @param n_models Logical. Only applies if \code{em} contains column \code{class}. Defines how many endmember combinations should be picked. Maximum is the minimum number of endmembers of a class. Defaults to 5.
#' @param sum_to_one Logical. Defines whether a sum-to-one constraint should be applied so that probabilities of endmember classes sum to one (a constraint not covered by NNLS) to be interpretable as fractions. Defaults to \code{TRUE}. To get actual NNLS results, change to \code{FALSE}.
#' @param verbose Logical. Prints progress messages during execution.
#' @param ... further arguments passed to \link[terra]{writeRaster}.
#' 
#' @return SpatRaster. The object will contain one band per class, with each value representing the estimated probability of the respective endmember class per pixel, and an RMSE band. If \code{sum_to_one} is \code{TRUE} (default), values of the class bands can be interpreted as fractions per endmember class (0 to 1).
#' 
#' @details Argument \code{em} determines whether an SMA (each row representing a single endmember per class) or a MESMA (multiple endmembers per class differentiate using the \code{class} column) is computed.
#' If multiple endmembers per class are provided, \code{mesma} will compute a number of SMA (determined by argument \code{n_models}) for multiple endmember combinations drawn from \code{em} and will select the best fit per pixel based on the lowest RMSE, based on the MESMA approach proposed by Roberts et al. (1998).
#' 
#' @note Depending on \code{iterate} and \code{tolerance} settings and the selected endmembers, the sum of estimated probabilities per pixel varies around 1. NNLS does not account for a sum-to-one constraint. Use \code{sum_to_one} to sum to one post-NNLS.
#' 
#' To achieve best results, it is recommended to adjust \code{n_models} in accordance to the number of endemembers per class provided through \code{em} so that as many endmember combinations as possible (with each endmember being used once) are computed. The more models are being calculated, the more processing and memory recourses are needed.
#' 
#' @references 
#' 
#' Franc, V., Hlaváč, V., & Navara, M. (2005). Sequential coordinate-wise algorithm for the non-negative least squares problem. In: International Conference on Computer Analysis of Images and Patterns (pp. 407-414). Berlin, Heidelberg.
#' 
#' Roberts, D. A., Gardner, M., Church, R., Ustin, S., Scheer, G., & Green, R. O. (1998). Mapping chaparral in the Santa Monica Mountains using multiple endmember spectral mixture models. Remote sensing of environment, 65(3), 267-279.
#' 
#' @author Jakob Schwalb-Willmann
#' @examples
#' 
#' library(RStoolbox)
#' library(terra)
#'
#' #  to perform a SMA, use a single endmember per class, row by row:
#' em <- data.frame(lsat[c(5294, 47916)])
#' rownames(em) <- c("forest", "water")
#' 
#' # umix the lsat image
#' probs <- mesma(img = lsat, em = em)
#' plot(probs)
#'
#' # to perform a MESMA, use multiple endmembers per class, differntiating them
#' # by a column named 'class':
#' \dontrun{
#' em <- rbind(
#'   data.frame(lsat[c(4155, 17018, 53134, 69487, 83704)], class = "forest"),
#'   data.frame(lsat[c(22742, 25946, 38617, 59632, 67313)], class = "water")
#' )
#' 
#' # unmix the lsat image
#' probs <- mesma(img = lsat, em = em)
#' plot(probs)
#' 
#' # MESMA can also be performed on more than two endmember classes:
#' em <- rbind(
#'   data.frame(lsat[c(4155, 17018, 53134, 69487, 83704)], class = "forest"),
#'   data.frame(lsat[c(22742, 25946, 38617, 59632, 67313)], class = "water"),
#'   data.frame(lsat[c(4330, 1762, 1278, 1357, 17414)], class = "shortgrown")
#' )
#' 
#' # unmix the lsat image
#' probs <- mesma(img = lsat, em = em)
#' plot(probs)
#' }

#' 
#' @importFrom terra which.min rast selectRange nlyr
#' @export
#' 
mesma <- function(img, em, method = "NNLS", iterate = 400, tolerance = 0.00000001, n_models = 5, sum_to_one = TRUE, ..., verbose){
  img <- .toTerra(img)
  ## messages
  if(!missing("verbose")) .initVerbose(verbose)
  verbose <- getOption("RStoolbox.verbose")
  
  ## breaking pre-checks
  .vMessage("Checking user inputs...")
  if(!inherits(img, "SpatRaster")){
    stop("'img' needs to be a 'SpatRaster' object.")
  }
  if(!inherits(em, c("matrix", "data.frame"))){
    stop("'em' needs to be a 'matrix' or 'data.frame' class object.")
  }
  if(anyNA(em)){
    stop("'em' is not allowed to contain NA values. Spectra must be consistent.")
  }
  
  method <- toupper(method) 
  meth_avail <- c("NNLS") #available methods

  if(!inherits(method, "character")){
    stop("'method' needs to be a 'character' class object.")
  }
  if(!method %in% meth_avail){
    stop(paste0("Unknown 'method': '", method, "'"))
  }
  
  # check em
  if(length(em[,1]) < 2){
    stop("'em' must contain at least two endmembers (number of rows in 'em').")
  }
  em_check <- em[,which(colnames(em) != "class")]
  if(length(em_check[1,]) != nlyr(img)){
    stop("'em' and 'img' have different numbers of spectral features (number of columns in 'em', excluding column 'class'). Both need to represent the same number of spectral bands for equal spectral resolutions/ranges.")
  }
  
  # MESMA or SMA?
  em <- as.data.frame(em) # safer for ow but generally unnecessary as we go back to matrix before nnls
  if(!is.null(em$class)){
    classes <- unique(em$class)
    # check n_models
    n_em_cl <- min(sapply(classes, function(x) nrow(em[em$class == x,]), USE.NAMES = F))
    if(n_em_cl < n_models){
      warning(paste0("'n_models' cannot be larger than the minimum number of provided endmembers per class. Setting 'n_models' to ", n_em_cl, "."))
      n_models <- n_em_cl
    }
    .vMessage(paste0("Found column 'class' in 'em', creating endmember combinations using 'n_models = ", n_models, "'...")) 
    
    # create endmember combinations
    em_combo <- data.frame(lapply(classes, function(x) sample((1:nrow(em))[em$class == x], n_models)))
    colnames(em_combo) <- classes
    
    # run SMA per combo
    .vMessage(paste0("Unmixing imagery by applying MESMA on endmember combinations derived from 'em' using '", method, "'..."))
    probs_models <- apply(em_combo, MARGIN = 1, function(em_sel){
      em_model <- em[as.numeric(em_sel),]
      em_model$class <- NULL
      rownames(em_model) <- classes
      em_model <- as.matrix(em_model)
      .paraRasterFun(img, rasterFun = app, args = list(fun = function(xi, na.rm = FALSE) nnls_solver(x = xi, A = em_model, iterate = iterate, tolerance = tolerance)), wrArgs = list(...))
    })
    
    # compute rmse
    rmse_models <- rast(lapply(probs_models, function(x) x[[nlyr(x)]]))
    which_lowest_rmse <- which.min(rmse_models)
    
    # select based on rmse
    probs <- rast(c(lapply(1:length(classes), function(i){
      selectRange(rast(lapply(probs_models, function(x) x[[i]])), which_lowest_rmse)
    }),
      selectRange(rast(lapply(probs_models, function(x) x[[nlyr(x)]])), which_lowest_rmse)
    ))
    names(probs) <- c(classes, "RMSE")
    
  } else{
  
    ## hand over to C++ nnls_solver
    .vMessage(paste0("No column 'class' in 'em', treating each row as single class.")) 
    .vMessage(paste0("Unmixing imagery by applying SMA on 'em' directly using '", method, "'..."))
    if(method == "NNLS"){
      em <- as.matrix(em)
      probs <- .paraRasterFun(img, rasterFun = app, args = list(fun = function(xi, na.rm = FALSE) nnls_solver(x = xi, A = em, iterate = iterate, tolerance = tolerance)), wrArgs = list(...))
    }
    ## assign band names
    if(length(rownames(em)) != 0){
      names(probs)[1:(nlyr(probs)-1)] <- rownames(em)
    }
    names(probs)[nlyr(probs)] <- "RMSE"
  }
  
  # make results sum to one
  if(sum_to_one){
    probs[[1:(nlyr(probs)-1)]] <- app(probs[[1:(nlyr(probs)-1)]], fun = function(x) x/sum(x))
  }
  
  ## return brick
  return(probs)
}