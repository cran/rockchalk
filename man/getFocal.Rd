\name{getFocal}
\alias{getFocal}
\title{Select focal values from an observed variable.}
\usage{
  getFocal(x, ...)
}
\arguments{
  \item{x}{Required. A variable}

  \item{...}{Other arguments that will be passed to the
  user-specified xvals function.}
}
\value{
  A vector.
}
\description{
  This is a generic function with 2 methods,
  getFocal.default handles numeric variables, while
  getFocal.factor handles factors. No other methods have
  been planned for preparation.
}
\details{
  This is used in functions like \code{plotSlopes} or
  \code{plotCurves}.
}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}

