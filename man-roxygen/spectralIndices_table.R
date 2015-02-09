#' @details
#' \code{spectralIndices} calculates all indices in one go, i.e. one call to overlay, which is far more efficient, than calculating each index separately (for large rasters).
#' By default all indices which can be calculated given the specified indices will be calcultated. If you don't want all indices, use the \code{indices} argument to specify exactly which indices are to be calculated.
#' See the table bellow for index names and required bands.
#' 
#' \code{coefs} can be used to redefine additional coefficients used to calculate \code{EVI} (\code{L_evi, C1, C2, G}), and \code{SAVI} (\code{L}).
#' Although the defaults are sensible values, it can make sense to adjust the SAVI soil brigthness factor \code{L} depending on the characteristics of the vegetation cover in your scene.
#'   
#' <% fr <- sapply(.IDXdb, function(x) paste0("\\code{",paste0(names(formals(x)), collapse=", "),"}")) %>
#' <% dl <- sapply(.IDXdb, function(x) paste0("\\eqn", paste0(body(x), collapse=""), "}")) %>
#' <% fn <- sapply(.IDX.REFdb[names(.IDXdb)],"[",2) %>
#' <% sr <- sapply(.IDX.REFdb[names(.IDXdb)],"[",1) %>
#' <% df <- data.frame(Index = names(.IDXdb), Description = fn, Source = sr, Bands = fr, Formula = dl) %>
#' <%= .df2tab(df, "lllll") %>


