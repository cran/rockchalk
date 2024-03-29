% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lazyCor.R
\name{lazyCor}
\alias{lazyCor}
\title{Create correlation matrices.}
\usage{
lazyCor(X, d)
}
\arguments{
\item{X}{Required. May be one value, a vech, or a matrix}

\item{d}{Optional. The number of rows in the correlation matrix to
be created. lazyCor will deduce the desired size from X if
possible. If X is a single value, d is a required argument.}
}
\value{
A correlation matrix.
}
\description{
Use can supply either a single value (the common correlation among
all variables), a column of the lower triangular values for a
correlation matrix, or a candidate matrix. The function will check
X and do the right thing. If X is a matrix, check that it
is a valid correlation matrix. If its a single value, use that
to fill up a matrix. If itis a vector, try to use it as a vech
to fill the lower triangle..
}
\examples{
lazyCor(0.5, 8)
lazyCor(c(0.1, 0.2, 0.3))
lazyCor(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6))
}
\author{
Paul Johnson \email{pauljohn@ku.edu}
}
