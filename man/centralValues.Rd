\name{centralValues}
\alias{centralValues}
\title{Central Tendency estimates for variables}
\usage{
  centralValues(x)
}
\arguments{
  \item{x}{a data frame}
}
\value{
  a data frame with the same variables and one row, the
  summary indicators.
}
\description{
  This is needed for the creation of summaries and
  predicted values of regression models. It takes a data
  frame and returns a new data frame with one row in which
  the mean or mode of the columns is reported.
}
\examples{
myDat <- data.frame(x=rnorm(100), y=rpois(100,l=4), z = cut(rnorm(100), c(-10,-1,0,10)))
centralValues(myDat)
}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}

