\name{mcGraph3}
\alias{mcGraph3}
\title{mcGraph3 draws a 3-dimensional scatter and a regression plane}
\usage{
  mcGraph3(x1, x2, y, interaction = FALSE, ...)
}
\arguments{
  \item{x1}{a predictor vector}

  \item{x2}{a predictor vector}

  \item{y}{the dependent variable}

  \item{interaction}{a TRUE or FALSE request for inclusion
  of the x1-x2 interaction in the regression calculation}

  \item{...}{optional arguments passed to persp}
}
\value{
  a list of 2 objects, the fitted regression model and the
  perspective matrix used with persp to draw the image.
}
\description{
  The observations are scattered in 3-dimensions, the
  fitted values are represented by a mesh, and their
  shadows in the x1-x2 plane are also represented.
}
\examples{
set.seed(12345)
## Create data with x1 and x2 correlated at 0.10
dat <- genCorrelatedData(rho=.1, stde=7)

mcGraph3(dat$x1, dat$x2, dat$y, theta = 0)

dat2 <- genCorrelatedData(rho = 0, stde = 7)

mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = 0, phi = 10)
mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = 30, phi = 10)
mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = -30, phi = 10)
mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = -30, phi = -10)
mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = -30, phi = -15)

## Run regressions with not-strongly correlated data
modset1 <- list()
for(i in 1:20){
dat2 <- genCorrelatedData(rho = .1, stde = 7)
summary(lm( y ~ x1 + x2 , data = dat2))
modset1[[i]] <- mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = -30)
}


## Run regressions with strongly correlated data
modset2 <- list()
for(i in 1:20){
dat2 <- genCorrelatedData(rho = .981, stde = 7)
summary(lm( y ~ x1 + x2 , data = dat2))
modset2[[i]] <- mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = -30)
}

dat3 <- genCorrelatedData(rho = .981, stde = 100, beta=c(0.1, 0.2, 0.3, -0.1))
mcGraph3(dat3$x1, dat3$x2, dat3$y, theta=-10, interaction = TRUE)
}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}

