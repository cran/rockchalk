\name{perspEmpty}
\alias{perspEmpty}
\title{perspEmpty
Creates a persp plot without drawing anything in the interior.
Does equivalent \code{of plot( type="n")} for persp.}
\usage{
  perspEmpty(x1, x2, y, ... )
}
\arguments{
  \item{x1}{data for the first horizontal axis, an R
  vector}

  \item{x2}{data for the second horizontal axis, an R
  vector}

  \item{y}{data for the vertical axis, an R vector}

  \item{...}{further arguments that are passed to persp}
}
\description{
  Regression demonstrations require a blank slate in which
  points and planes can be drawn. This function creates
  that blank persp canvas for those projects. It is not
  necessary that x1, x2 and y be vectors of the same
  length, since this function's only purpose is to plot an
  empty box with ranges determined by the input variables.
  persp calls the 3 axes x, y, and z, but here they are
  called x1, x2, and y.
}
\examples{
x1 <- 1:10
x2 <- 40:50
y <-  rnorm(10)
perspEmpty(x1, x2, y)
perspEmpty(x1, x2, y, ticktype="detailed", nticks=10)
}

