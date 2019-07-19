#' @details
#' \code{spectralIndices} calculates all indices in one go in C++,  which is more efficient than calculating each index separately (for large rasters).
#' By default all indices which can be calculated given the specified indices will be calculated. If you don't want all indices, use the \code{indices} argument to specify exactly which indices are to be calculated.
#' See the table bellow for index names and required bands.
#'    
#' Index values outside the valid value ranges (if such a range exists) will be set to NA. For example a pixel with NDVI > 1 will be set to NA.
#' 
#'  
#' <% fr <- sapply(.IDXdbFormulae, function(x) paste0("\\code{",paste0(names(formals(x)), collapse=", "),"}")) %>
#' <% dl <- sapply(.IDXdbFormulae, function(x) paste0("\\eqn", paste0(body(x), collapse=""), "}")) %>
#' <% fn <- sapply(.IDX.REFdb[names(.IDXdbFormulae)],"[",2) %>
#' <% sr <- sapply(.IDX.REFdb[names(.IDXdbFormulae)],"[",1) %>
#' <% df <- data.frame(Index = names(.IDXdbFormulae), Description = fn, Source = sr, Bands = fr, Formula = dl) %>
#' <%= .df2tab(df, "lllll") %>
#' 
#' 
#' Some indices require additional parameters, such as the slope of the soil line which are specified via a list to the \code{coefs} argument. 
#' Although the defaults are sensible values, values like the soil brightnes factor \code{L} for SAVI should be adapted depending on the characteristics of the scene.
#' The coefficients are:
#' \tabular{lll}{
#' \strong{Coefficient} \tab \strong{Description} \tab \strong{Affected Indices} \cr
#' \code{s} \tab slope of the soil line \tab DVI, WDVI \cr
#' \code{L_evi, C1, C2, G} \tab various \tab EVI \cr
#' \code{L} \tab soil brightness factor \tab SAVI, SATVI \cr
#' \code{swir2ccc} \tab minimum swir2 value (completely closed forest canopy) \tab NDVIC \cr
#' \code{swir2coc} \tab maximum swir2 value (completely open canopy) \tab NDVIC \cr
#' }
#' 
#' 
#' The wavelength band names are defined following Schowengertd 2007, p10. 
#' The last column shows exemplarily which Landsat 5 TM bands correspond to which wavelength range definition.
#' <%= .df2tab(.wavlDB, "lllllll") %>



