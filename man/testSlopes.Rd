\name{testSlopes}
\alias{testSlopes}
\title{Hypothesis tests for Simple Slopes Objects}
\usage{
  testSlopes(plotSlopesObject)
}
\arguments{
  \item{plotSlopesObject}{Output from the plotSlopes
  function}
}
\value{
  A list including the hypothesis test table. For numeric
  modx variables, also the Johnson-Neyman (J-N) interval
  boundaries.
}
\description{
  The user must first run plotSlopes, and then supply that
  output object to this function.
}
\details{
  Consider a regression, for example

  y <- b0 + b1*x1 + b2*x2 + b3*(x1*x2) + b4*x3 + ... +
  error

  If \code{plotSlopes} has been run with the variable
  plotx="x1" and the variable modx="x2", then there will be
  several plotted lines, one for various values of x2.  We
  wonder if the combined effect of x1 is statistically
  significantly different from 0 for each of those values
  of x2.

  This function performs a test of the null hypothesis of
  the slope of each fitted line in a \code{plotSlopes}
  object is statistically significant from zero. A simple
  t-test for each line is offered. No correction for the
  conduct of multiple hypothesis tests (no Bonferroni
  correction).

  In addition, the so-called Johnson-Neyman
  (Johnson-Neyman, 1936; Preacher, Curran, and Bauer, 2006)
  interval is calculated and a couple of efforts are made
  to render it graphically.  Where the t-test considers the
  question, is the slope of the line (b1 + b3*x2) different
  from 0, the J-N approach asks "for which values of x2
  will that plotted line be statistically significant.
}
\examples{
library(car)
m3 <- lm(statusquo ~ income * sex, data = Chile)
m3ps <-plotSlopes(m3, modx = "sex", plotx = "income")
testSlopes(m3ps)

m4 <- lm(statusquo ~ region * income, data= Chile)
m4ps <- plotSlopes(m4, modx = "region", plotx = "income", plotPoints = FALSE)
testSlopes(m4ps)


m5 <- lm(statusquo ~ region * income + sex + age, data= Chile)
m5ps <- plotSlopes(m5, modx = "region", plotx = "income")
testSlopes(m5ps)

m6 <- lm(statusquo ~ income * age + education + sex + age, data=Chile)
m6ps <- plotSlopes(m6, modx = "income", plotx = "age")
testSlopes(m6ps)

## Finally, if model has no relevant interactions, testSlopes does nothing.
m9 <- lm(statusquo ~ age + income * education + sex + age, data=Chile)
m9ps <- plotSlopes(m9, modx = "education", plotx = "age", plotPoints = FALSE)
testSlopes(m9ps)
}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}
\references{
  Preacher, Kristopher J, Curran, Patrick J.,and Bauer,
  Daniel J. (2006). Computational Tools for Probing
  Interactions in Multiple Linear Regression, Multilevel
  Modeling, and Latent Curve Analysis. Journal of
  Educational and Behavioral Statistics. 31,4, 437-448.

  Johnson, P.O. and Neyman, J. (1936). "Tests of certain
  linear hypotheses and their applications to some
  educational problems. Statistical Research Memoirs, 1,
  57-93.
}
\seealso{
  plotSlopes
}

