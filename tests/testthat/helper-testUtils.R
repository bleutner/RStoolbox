.th_naCount <- function(x) {
  if (inherits(x, "SpatRaster")) {
    return(as.numeric(global(is.na(x), "sum")))
  }
  cellStats(is.na(x), "sum")
}

.th_minmax <- function(x, names = FALSE) {
  if (inherits(x, "SpatRaster")) {
    mm <- minmax(x)
    if(!names) dimnames(mm) <- NULL
  } else {
    mm <- rbind(minValue(x),maxValue(x))
    if(names) colnames(mm) <- names(x)
  }
  return(mm)
}
