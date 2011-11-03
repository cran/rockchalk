\name{magRange}
\alias{magRange}
\title{magRange
Magnify the range of a variable.}
\usage{
  magRange(x, mult = 1.25)
}
\arguments{
  \item{x}{an R vector variable}

  \item{mult}{a multiplier by which to magnify the range of
  the variable. A value of 1 leaves the range unchanged.
  May be a scalar, in which case both ends of the range are
  magnified by the same amount.  May also be a two valued
  vector, such as c(minMag,maxMag), in which case the
  magnification applied to the minimum is minMag and the
  magnification of the maximum is maxMag.}
}
\description{
  By default, R's range function returns the minimum and
  maximum values of a variable. This returns a magnified
  range. It is used for some plotting functions in the
  rockchalk package
}
\examples{
x1 <- rnorm(100)
range(x1)
magRange(x1)
magRange(x1, 1.5)
magRange(x1, c(1,1.5))
}

