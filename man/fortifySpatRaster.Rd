% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fortify-raster.R
\name{fortifySpatRaster}
\alias{fortifySpatRaster}
\title{Fortify method for classes from the terra package.}
\usage{
fortifySpatRaster(x, maxpixels = 50000)
}
\arguments{
\item{x}{\code{SpatRaster} object to convert into a dataframe.}

\item{maxpixels}{Integer. Maximum number of pixels to sample}
}
\value{
Returns a data.frame with coordinates (x,y) and corresponding raster values.
}
\description{
Fortify method for classes from the terra package.
}
\examples{
r_df <- fortifySpatRaster(rlogo)
head(r_df)
}
