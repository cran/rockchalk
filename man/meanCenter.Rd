\name{meanCenter}
\alias{meanCenter}
\alias{meanCenter.default}
\title{meanCenter}
\usage{
  meanCenter(model, centerOnlyInteractors = TRUE,
    centerDV = FALSE, standardize = FALSE)

  \method{meanCenter}{default} (model,
    centerOnlyInteractors = TRUE, centerDV = FALSE,
    standardize = FALSE)
}
\arguments{
  \item{model}{a fitted regression model (presumably from
  lm)}

  \item{centerOnlyInteractors}{If false, all predictors in
  the regression data frame are centered before the
  regression is conducted.}

  \item{centerDV}{Should the dependent variable be
  centered?}

  \item{standardize}{Instead of simply mean-centering the
  variables, should they also be "standardized" by first
  mean-centering and then dividing by the estimated
  standard deviation.}
}
\value{
  A regression model of the same type as the input model,
  with attributes representing the names of the centered
  variables.
}
\description{
  meanCenter selectively centers or standarizes variables
  in a regression model.
}
\details{
  Mean-centering has often been recommended as a way to
  ameliorate multi-collinearity in regression models that
  include interaction terms (Aiken and West, 1991; Cohen,
  et al 2002). While this claim may have been mistaken
  (Echambadi and Hess, 2007), mean-centering is still
  widely practiced.  This function facilitates comparison
  of mean-centered models with others by automatically
  re-calculating centered variables.  The defaults will
  cause a regression's numeric interactive variables to be
  mean centered. That is to say, if an interaction x1:x2 is
  present in the model, then x1 and x2 are replaced by
  (m1-mean(m1)) and (m2-mean(m2) in all of the terms in
  which they appear in the model (the main effect and the
  interaction).  If one wants all predictors to be
  centered, the option \code{centerOnlyInteractors} should
  be set to FALSE. The dependent variable will not be
  centered, unless the user explicitly requests it by
  setting centerDV = TRUE. The centered variables can be
  standardized (optionally, of course).
}
\examples{

library(rockchalk)
N <- 100
dat <- genCorrelatedData(N=N, means=c(100,200), sds=c(20,30), rho=0.4, stde=10)
dat$x3 <- rnorm(100, m=40, s=4)

m1 <- lm(y ~ x1 * x2 + x3, data=dat)
summary(m1)
mcDiagnose(m1)

m1c <- meanCenter(m1)
summary(m1c)
mcDiagnose(m1c)

m2 <- lm(y ~ x1 * x2 + x3, data=dat)
summary(m2)
mcDiagnose(m2)

m2c <- meanCenter(m2, standardize = TRUE)
summary(m2c)
mcDiagnose(m2c)

m2c2 <- meanCenter(m2, centerOnlyInteractors = FALSE)
summary(m2c2)

m2c3 <- meanCenter(m2, centerOnlyInteractors = FALSE, centerDV = TRUE)
summary(m2c3)

dat <- genCorrelatedData(N=N, means=c(100,200), sds=c(20,30), rho=0.4, stde=10)
dat$x3 <- rnorm(100, m=40, s=4)
dat$x3 <- gl(4, 25, labels=c("none","some","much","total"))

m3 <- lm(y ~ x1 * x2 + x3, data=dat)
summary(m3)
## visualize, for fun
plotPlane(m3, "x1", "x2")

m3c1 <- meanCenter(m3)
summary(m3c1)

## Not exactly the same as a "standardized" regression because the
## interactive variables are centered in the model frame,
## and the term "x1:x2" is never centered again.
m3c2 <- meanCenter(m3, centerDV=TRUE, centerOnlyInteractors=FALSE, standardize=TRUE)
summary(m3c2)

m3st <- standardize(m3)
summary(m3st)
}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}
\references{
  Aiken, L. S. and West, S.G. (1991). Multiple Regression:
  Testing and Interpreting Interactions. Newbury Park,
  Calif: Sage Publications.

  Cohen, J., Cohen, P., West, S. G., and Aiken, L. S.
  (2002). Applied Multiple Regression/Correlation Analysis
  for the Behavioral Sciences (Third.). Routledge Academic.

  Echambadi, R., and Hess, J. D. (2007). Mean-Centering
  Does Not Alleviate Collinearity Problems in Moderated
  Multiple Regression Models. Marketing Science, 26(3),
  438-445.
}
\seealso{
  \code{\link[pequod]{lmres}}
}

