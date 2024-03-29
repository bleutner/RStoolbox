% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/encodeQA.R
\name{decodeQA}
\alias{decodeQA}
\title{Decode QA flags to bit-words}
\usage{
decodeQA(x)
}
\arguments{
\item{x}{Integer (16bit)}
}
\value{
Returns the decoded QA values from an integer
}
\description{
Intended for use with Landsat 16-bit QA bands. Decodes pixel quality flags from integer to bit-words.
}
\examples{
decodeQA(53248)
}
\seealso{
\link{encodeQA}
}
