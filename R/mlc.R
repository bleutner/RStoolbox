#' Maximum Likelihood Classification
#' @param x matrix with predictors
#' @param y vector with classes (factor)
#' @param ... not used
#' @keywords internal
#' @noRd
mlc <- function(x, y, ...){
        classes <- setNames(nm=levels(y))
        mod <- lapply(classes, function(ci){
          cl <- y %in% ci
          
          if(!any(cl)) return(list(warn=TRUE, msg=paste("No data for class", ci)))
          
          cod <- cov(x[cl,])
          ## Check&Fix for singularity due to fully correlated variables
          ## TODO: exclude correlated variables on a per class basis?
          dups <- duplicated(cod)
          warn <- any(dups) 
          if(warn) {
            diag(cod) <- diag(cod) + cumsum(1e-10*dups)*dups
          }
          
          upper <- chol(cod)
          
          list(m = colMeans(x[cl,]),  
               D = -2*sum(log(diag(upper))), 
               I = provideDimnames(chol2inv(upper), base=dimnames(upper)), 
               warn = warn)
  			})		
    
  warn <- classes[ vapply(mod, "[[", FALSE, "warn")]
  if(length(warn)) warning("Covariance matrix of class/classes ", paste0(warn, collapse =", "), " is singular, i.e. holds perfectly correlated variables.")
  for(i in seq_along(mod)) mod[[i]][["warn"]] <- NULL
  mod[["levels"]] <- unique(y)
	mod
}

#' Predict Maximum Likelihood Classification
#' 
#' @param modelFit model result from mlc
#' @param newdata Matrix. New data.
#' @param ... not used
#' @noRd 
#' @keywords internal
predict.mlc <- function(modelFit, newdata, ...){	
	if(inherits(modelFit, "train")) modelFit <- modelFit$finalModel
	classes <- modelFit$obsLevels
	pred <- predictMlcCpp(newdata, model = modelFit, nclasses = length(classes))
    factor(classes[pred[,1]], classes)
}

#' Predict Maximum Likelihood Classification - Probabilities
#' 
#' @param modelFit model result from mlc
#' @param newdata Matrix. New data.
#' @param ... not used
#' @noRd 
#' @keywords internal
predict.mlc.prob <- function(modelFit, newdata, ...){	
    if(inherits(modelFit, "train")) modelFit <- modelFit$finalModel
    classes <- modelFit$obsLevels
    if(is.data.frame(newdata)) newdata <- as.matrix(newdata)
    pred <- predictMlcCpp(newdata, model = modelFit, nclasses = length(classes))
    pred <- pred[,-1]
    colnames(pred) <- classes
    pred
}

#' Define caret custom model for maximum likelihood classification
#' @noRd 
#' @keywords internal
mlcCaret <- list(
		label = "Maximum Likelihood Classification",
		library = NULL,
		type = "Classification",
		parameters = data.frame(parameter = "parameter", class = "class", label = "label"),
		grid = function (x, y, len = NULL, ...) {data.frame(parameter = "none")},
		fit = mlc,
		predict = predict.mlc,
		prob = predict.mlc.prob,
		sort = function(x) x,
        levels = function(x) levels(x$levels)
)
