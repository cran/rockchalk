% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/waldt.R
\name{waldt}
\alias{waldt}
\title{T-test for the difference in 2 regression parameters}
\usage{
waldt(parm1, parm2, model, model.cov = NULL, digits = getOption("digits"))
}
\arguments{
\item{parm1}{A parameter name, in quotes!}

\item{parm2}{Another parameter name, in quotes!}

\item{model}{A fitted regression model}

\item{model.cov}{Optional, another covariance matrix to use while
calculating the test. Primarily used for robust (or otherwise
adjusted) standard errors}

\item{digits}{How many digits to print? This affects only the on-screen
printout. The return object is numeric, full precision.}
}
\value{
A vector with the difference, std. err., t-stat, and p
    value. Prints a formatted output statement.
}
\description{
This is the one the students call the "fancy t test". It is just
the simplest, most easy to use version of the t test to decide if
2 coefficients are equal. It is not as general as other functions
in other packages. This is simpler to use for beginners.  The
\code{car} package's function \code{linearHypothesis} is more
general, but its documentation is much more difficult to understand.
It gives statistically identical results, albeit phrased as an F
test.
}
\details{
I did this because we have trouble understanding terminology in
documentation for more abstract functions in other R
packages.

It has an additional feature, it can import robust standard errors
to conduct the test.
}
\examples{
mdat <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
stde <- 2
mdat$y <- 0.2 * mdat$x1 + 0.24 * mdat$x2 + stde * rnorm(100)
m1 <- lm(y ~ x1 + x2, data = mdat)
waldt("x1", "x2", m1)
waldt("x1", "x2", m1, digits = 2)
## Returned object is not "rounded characters". It is still numbers
stillnumeric <-  waldt("x1", "x2", m1, digits = 2)
stillnumeric
## Equivalent to car package linearHypothesis:
if(require(car)){
    linearHypothesis(m1, "x1 = x2")
}
## recall t = sqrt(F) for a 1 degree of freedom test.
## If we could understand instructions for car, we probably
## would not need this function, actually.
}
\author{
Paul Johnson \email{pauljohn@ku.edu}
}
