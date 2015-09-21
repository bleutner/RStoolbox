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
				while(any(dups)) {
					d <- which(dups)[1]
				    warning(paste0("Covariance matrix of class ", ci, " is singular, i.e. holds perfectly correlated variables."))
					cod[d, d] <- cod[d, d] + 1e-10
					dups <- duplicated(cod)				
				}
				list(m = colMeans(x[cl,]),  D = -log(det(cod)), I = solve(cod))
			})
	names(mod) <- as.character(levels(y))
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
	modelFit <- modelFit$finalModel
	classes <- modelFit$obsLevels
	lp <- lapply(1:nrow(newdata), function(s){
				lik <- vapply(classes, function(i){
							xm <- newdata[s,] - modelFit[[i]]$m
							drop(modelFit[[i]]$D - xm %*% modelFit[[i]]$I %*% as.matrix(xm))
						}, numeric(1))
			})
	
	vp <- vapply(lp, which.max, numeric(1))
	vp
}

#' Define caret custom model for maximum likelihood classification
#' @noRd 
#' @keywords internal
mlcCaret <- list(
		label = "Maximum Likelihood Classification",
		library = NULL,
		type = "Classification",
		parameters = data.frame(parameter = "parameter", class = "class", label = "label"),
		grid = function (x, y, len = NULL) {data.frame(parameter = "none")},
		fit = mlc,
		predict = predict.mlc,
		prob = NULL,
		sort = function(x) x	
)
