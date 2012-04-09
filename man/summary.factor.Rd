\name{summary.factor}
\alias{summary.factor}
\title{Tabulates observed values and calculates entropy}
\usage{
  summary.factor(y, maxLevels)
}
\arguments{
  \item{y}{a factor (non-numeric variable)}

  \item{maxLevels}{The maximum number of levels that will
  be presented in the tabulation.}
}
\value{
  a vector of named elements including the summary table as
  well as entropy and normed entropy.
}
\description{
  This adapts code from R base summary.factor. It adds the
  calculation of entropy as a measure of diversity.
}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}

