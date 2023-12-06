.th_naCount <- function(x) {
  if (inherits(x, "SpatRaster")) {
    return(as.numeric(global(is.na(x), "sum")))
  }
  t(global(is.na(x), sum))
}

.th_minmax <- function(x, names = FALSE) {
  if (inherits(x, "SpatRaster")) {
    mm <- minmax(x)
    if(!names) dimnames(mm) <- NULL
  } else {
    mm <- rbind(min(values(x)), max(values(x)))
    if(names) colnames(mm) <- names(x)
  }
  return(mm)
}
