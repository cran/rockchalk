\name{meanCenter}
\alias{meanCenter}
\alias{meanCenter.default}
\title{meanCenter}
\usage{
  meanCenter(model, centerOnlyInteractors = TRUE,
    centerDV = FALSE, standardize = FALSE,
    centerContrasts = F)

  \method{meanCenter}{default} (model,
    centerOnlyInteractors = TRUE, centerDV = FALSE,
    standardize = FALSE, centerContrasts = F)
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

  \item{centerContrasts}{This function was originally
  intended only to center numeric variables. However, this
  option will ask centering of the numeric contrasts that
  are created in the fitting process.}
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
  The defaults will cause a regression's numeric
  interactive variables to be mean centered. If one wants
  all predictors to be centered, the option
  centerOnlyInteractors should be set to FALSE. The
  dependent variable will not be centered, unless the user
  explicitly requests it by setting centerDV = TRUE.
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

m3c1 <- meanCenter(m3)
summary(m3c1)

m3c2 <- meanCenter(m3, centerContrasts=TRUE)
summary(m3c2)

## Not exactly the same as a "standardized" regression because the
## interactive variables are centered in the model frame,
## and the term "x1:x2" is never centered again.
m3c3 <- meanCenter(m3, centerDV=TRUE, centerContrasts=TRUE, centerOnlyInteractors=FALSE, standardize=TRUE)
summary(m3c3)

m3st <- standardize(m3)
summary(m3st)
}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}
\seealso{
  \code{\link[pequod]{lmres}}
}

