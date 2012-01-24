\name{genCorrelatedData}
\alias{genCorrelatedData}
\title{Generates a data frame (x1,x2,y) with user-specified correlation between x1 and x2}
\usage{
  genCorrelatedData(N = 100, means = c(50, 50),
    sds = c(10, 10), rho = 0, stde = 1,
    beta = c(0, 0.2, 0.2, 0))
}
\arguments{
  \item{N}{Number of cases desired}

  \item{means}{2-vector of means for x1 and x2}

  \item{sds}{2-vector of standard deviations for x1 and x2}

  \item{rho}{Correlation coefficient for x1 and x2}

  \item{stde}{standard deviation of the error term in the
  data generating equation}

  \item{beta}{beta vector of at most 4 coefficients for
  intercept, slopes, and interaction}
}
\description{
  The vector (x1,x2) is drawn from a multivariate normal
  distribution in which the expected value (mean) is the
  parameter \code{means} and the var/covar matrix is built
  from the assumed standard deviations \code{sds} and the
  correlation between x1 and x2 (\code{rho}). It is also
  necessary to specify the standard deviation of the error
  term (\code{stde}) and the coefficients of the regression
  equation (\code{beta}). The y (output) variable is
  created according to the equation y = b1 + b2 * x1 + b3 *
  x2 + b4 * x1 * x2 + e
}
\examples{
## 1000 observations of uncorrelated x1 and x2 with no
## interaction between x1 and x2
dat <- genCorrelatedData(N=1000, rho=0, beta=c(1, 1.0, -1.1, 0.0))
mcGraph1(dat$x1, dat$x2, dat$y, theta=20, phi=8, ticktype="detailed", nticks=10)
m1 <- lm(y ~ x1 + x2, data = dat)
plotPlane(m1, plotx1 = "x1", plotx2 = "x2")
}

