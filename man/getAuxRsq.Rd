\name{getAuxRsq}
\alias{getAuxRsq}
\title{retrieves estimates of the coefficient of determination from a list of regressions}
\usage{
  getAuxRsq(auxRegs)
}
\arguments{
  \item{auxRegs}{a list of fitted regression objects}
}
\value{
  a numeric vector of the same length as auxRegs.
}
\description{
  Asks each regression model in a list for a summary and
  then reports the R-squares.
}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}

