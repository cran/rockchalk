% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcDiagnose.R
\name{mcDiagnose}
\alias{mcDiagnose}
\title{Multi-collinearity diagnostics}
\usage{
mcDiagnose(model)
}
\arguments{
\item{model}{a fitted regression model}
}
\value{
a list of the "auxiliary regressions" that were
fitted during the analysis
}
\description{
Conducts a series of checks for multicollinearity.
}
\examples{

library(rockchalk)
N <- 100
dat <- genCorrelatedData3(y~ 0 + 0.2*x1 + 0.2*x2, N=N, means=c(100,200),
                          sds=c(20,30), rho=0.4, stde=10)
dat$x3 <- rnorm(100, m=40, s=4)
m1 <- lm(y ~ x1 + x2 + x3, data=dat)
summary(m1)
m1d <- mcDiagnose(m1)

m2 <- lm(y ~ x1 * x2 + x3, data=dat)
summary(m2)
m2d <- mcDiagnose(m2)



m3 <- lm(y ~ log(10+x1) + x3 + poly(x2,2), data=dat)
summary(m3)
m3d <- mcDiagnose(m3)

N <- 100
x1 <- 50 + rnorm(N)
x2 <- log(rgamma(N, 2,1))
x3 <- rpois(N, lambda=17)
z1 <- gl(5, N/5)
dummies <- contrasts(z1)[ as.numeric(z1), ]
dimnames(dummies) <- NULL ## Avoids row name conflict in data.frame below
y3 <- x1  -.5 * x2 + 0.1 * x2^2 + dummies \%*\% c(0.1,-0.1,-0.2,0.2)+ 5 * rnorm(N)
dat <- data.frame(x1=x1, x2=x2, x3=x3,  z1=z1, y3 = y3)

m3 <- lm(y3 ~ x1 + poly(x2,2)  + log(x1) + z1, dat)
summary(m3)

mcDiagnose(m3)



}
\author{
Paul E. Johnson \email{pauljohn@ku.edu}
}
