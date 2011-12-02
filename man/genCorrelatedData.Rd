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
  Generates a data frame (x1,x2,y) with user-specified
  correlation between x1 and x2
}

