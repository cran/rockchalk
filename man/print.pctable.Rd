% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pctable.R
\name{print.pctable}
\alias{print.pctable}
\title{Display pctable objects}
\usage{
\method{print}{pctable}(x, colpct = TRUE, rowpct = FALSE, ...)
}
\arguments{
\item{x}{A pctable object}

\item{colpct}{Default TRUE: include column percentages?}

\item{rowpct}{Default FALSE: include row percentages?}

\item{...}{Other arguments passed through to print method}
}
\value{
A table object for the final printed table.
}
\description{
This is not very fancy. Note that the saved pctable object
has the information inside it that is required to write both
column and row percentages. The arguments colpct and rowpct
are used to ask for the two types.
}
\author{
Paul Johnson \email{pauljohn@ku.edu}
}
