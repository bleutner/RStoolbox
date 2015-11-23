#' Maximum Likelihood Classification
#' @param x matrix with predictors
#' @param y vector with classes (factor)
#' @param ... not used
#' @keywords internal
#' @noRd
mlc <- function(x, y, ...){
	classes <- levels(y)
	mod <- lapply(classes, function(ci){
				cl <- y == ci
				cod <- cov(x[cl,])
				## Check&Fix for singularity due to fully correlated variables
				## TODO: exclude correlated variables on a per class basis?
				dups <- duplicated(cod)
                warn <- FALSE
				while(any(dups)) {
                    warn <- TRUE
					d <- which(dups)[1]
					cod[d, d] <- cod[d, d] + 1e-10
					dups <- duplicated(cod)				
				}
				list(m = colMeans(x[cl,]),  D = -log(det(cod)), I = solve(cod), warn = warn)
			})		
    
    warn <- classes[ vapply(mod, "[[", logical(1),"warn")]
    if(length(warn)) warning(paste0("Covariance matrix of class/classes ", warn, " is singular, i.e. holds perfectly correlated variables."))
    for(i in seq_along(mod)) mod[[i]][["warn"]] <- NULL
	names(mod) <- as.character(levels(y))
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
