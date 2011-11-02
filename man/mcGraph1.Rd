\name{mcGraph1}
\alias{mcGraph1}
\title{Illustrate the effect of multicollinearity in regression.}
\usage{
  mcGraph1(x1, x2, y, ...)
}
\arguments{
  \item{x1}{a predictor vector}

  \item{x2}{a predictor vector}

  \item{y}{the dependent variable}

  \item{...}{additional parameters passed to persp}
}
\description{
  This is a set of functions that faciliates the
  examination of multicollinearity. Suppose the "true"
  relationship is y[i] = 0.2 * x1[i] + 0.2 * x2[i] + e
  where e is Normal(0, stde^2).
}
\examples{
set.seed(12345)
## Create data with x1 and x2 correlated at 0.10
dat <- genCorrelatedData(rho=.1, stde=7)

mcGraph1(dat$x1, dat$x2, dat$y, theta=20, phi=8, ticktype="detailed", nticks=10)
}
\author{
  Paul Johnson <pauljohn@ku.edu>
}
\keyword{hplot}
\keyword{regression}

