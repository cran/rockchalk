\name{cutByQuantile}
\alias{cutByQuantile}
\title{Calculates the "center" quantiles, always including the median.}
\usage{
  cutByQuantile(x, n = 3)
}
\arguments{
  \item{x}{A numeric vector.}

  \item{n}{The number of quantile points. See details.}
}
\value{
  A vector
}
\description{
  If the numeric variable has fewer than 6 unique observed
  values, this will send the data to cutByTable. The
  default return will find dividing points at three
  quantiles: c(0.25, 0.50, 0.75) If n=4, the dividing
  points will be c(0.20, 0.40, 0.60, 0.80) If n=5, c(0.10,
  0.30, 0.50, 0.70, 0.90) Larger n that are odd will
  include 0.5 and evenly spaced points out to 0 and 1.
  Larger n that is even will return evenly spaced points
  calculated by R's \code{pretty} function.
}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}

