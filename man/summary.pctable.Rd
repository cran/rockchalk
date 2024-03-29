% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pctable.R
\name{summary.pctable}
\alias{summary.pctable}
\title{Extract presentation from a pctable object}
\usage{
\method{summary}{pctable}(object, ..., colpct = TRUE, rowpct = FALSE)
}
\arguments{
\item{object}{A pctable object}

\item{...}{Other arguments, currently unused}

\item{colpct}{Default TRUE: should column percents be included}

\item{rowpct}{Default FALSE: should row percents be included}
}
\value{
An object of class summary.pctable
}
\description{
Creates a column and/or row percent display of a pctable
result
}
\author{
Paul Johnson \email{pauljohn@ku.edu}
}
