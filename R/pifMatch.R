#' PIF-based Image Matching
#' 
#' Automatically estimates pseudo-invariant-features (PIF) by pixel similarity and fits a linear model which is then applied
#' to the whole scene
#' @param img Raster*. Image to be adjusted.
#' @param ref Raster*. Reference image.
#' @param method Method to calculate pixel similariry
#' @param quantile Numeric. Threshold quantile used to identify PIFs
#' @noRd 
pifMatch <- function(img, ref, method = "cor", quantile = 0.95){
	stopifnot(nlayers(img)==nlayers(ref))
	flist <- list(
			ed = function(x,y){-sqrt(sum((x-y)^2))},
			sam = function(x,y){acos(sum(x*y)/sqrt(sum(x^2)*sum(y^2)))},
			cor = function(x,y){
				a <- (x - mean(x))
				b <- (y-mean(y))
				sum(a*b)/sqrt(sum(a^2)*sum(b^2))
			}
	)
	
	funny <- function(x, y, f = flist[[method]]) {
		vapply(1:nrow(x), function(i){
					f(x[i,], y[i,])
				}, numeric(1))
	}
	
	imgfull <- img
	if(!extent(img)==extent(ref)) { 
		img <- crop(img, ref)
		ref <- crop(ref, img)
	}
	
	pifield <- overlay(img, ref, fun = funny)
	thresh <- quantile(pifield, p = quantile)
	pi <- pifield > thresh
	
	
	models <- lapply(1:nlayers(img), function(i){
				df <- data.frame(img = img[[i]][pi], ref = ref[[i]][pi])
				mod <- lm(ref~img, data = df)
			})
	
	correct <- lapply(seq_along(models), function(i){
				corLayer <- imgfull[[i]]
				mod <- models[[i]]
				names(corLayer) <- "img"
				predict(corLayer, mod)
			})
	
	plot(pi)
	correct <- stack(correct)
	names(correct) <- names(imgfull)
	return(correct)
	
} 






