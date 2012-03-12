\name{plotCurves}
\alias{plotCurves}
\title{Assists creation of predicted value curves for regression models.}
\usage{
  plotCurves(model = NULL, plotx = NULL, modx = NULL,
    modxVals = NULL, plotPoints = TRUE,
    envir = environment(formula(model)), ...)
}
\arguments{
  \item{model}{Fitted regression object. Must have a
  predict method}

  \item{plotx}{String with name of IV to be plotted on x
  axis}

  \item{modx}{String for moderator variable name. May be
  either numeric or factor.}

  \item{modxVals}{If modx is numeric, either a character
  string, "quantile", "std.dev.", or "table", or a vector
  of values for which plotted lines are sought. If modx is
  a factor, the default approach will create one line for
  each level, but the user can supply a vector of levels if
  a subset is desired.}

  \item{plotPoints}{Should the plot include the scatterplot
  points along with the lines.}

  \item{envir}{environment to search for variables.}

  \item{...}{further arguments that are passed to plot.}
}
\value{
  A plot is created as a side effect, a list is returned
  including 1) the call, 2) a newdata object that includes
  information on the curves that were plotted, 3) a vector
  modxVals, the values for which curves were drawn.
}
\description{
  This is similar to \code{plotSlopes}, but it accepts
  regressions in which there are transformed variables,
  such as "log(x1)". Think of this a new version of R's
  \code{termplot}, but it allows for interactions.  It
  creates a plot of the predicted dependent variable
  against one of the numeric predictors, \code{plotx}. It
  draws a predicted value line for several values of
  \code{modx}, a moderator variable. The moderator may be a
  numeric or categorical moderator variable.
}
\details{
  The user may designate which particular values of the
  moderator are used for calculating the predicted value
  lines.  That is, \code{modxVals = c( 12,22,37)} would
  draw lines for values 12, 22, and 37 of the moderator.

  If the user does not specify the parameter
  \code{modxVals}, built-in algorithms will select the "cut
  points". Three algorithms have been prepared so far,
  \code{quantile}, \code{std.dev.}, and \code{table}. If
  the number of unique observed values is smaller than 6,
  the \code{table} method is used.  The 5 most frequently
  observed values of modx are selected. Otherwise, the
  quantile method is used. Predictive lines are plotted for
  the following percentiles {0.25,0.50,0.75}. The algorithm
  \code{std.dev.} plots three lines, one for the mean of
  modx, and one for the mean minus one standard deviation,
  and the other for the mean plus one standard deviation.
}
\examples{
set.seed(12345)
N <- 500
x1 <- rnorm(N, m=5, s=1)
x2 <- rnorm(N)
x3 <- rnorm(N)
x4 <- rnorm(N)
xcat1 <- gl(2,50, labels=c("Monster","Human"))
xcat2 <- cut(rnorm(N), breaks=c(-Inf, 0, 0.4, 0.9, 1, Inf), labels=c("R", "M", "D", "P", "G"))
dat <- data.frame(x1, x2, x3, x4, xcat1, xcat2)
rm(x1, x2, x3, x4, xcat1, xcat2)

###The design matrix for categorical variables, xcat numeric
dat$xcat1n <- with(dat, contrasts(xcat1)[xcat1, ])
dat$xcat2n <- with(dat, contrasts(xcat2)[xcat2, ])


stde <- 2
dat$y <- with(dat, 0.03 + 11.5*log(x1)*xcat1n + 0.1*x2 + 0.04*x2^2 + stde*rnorm(N))

stde <- 1              
dat$y2 <- with(dat, 0.03 + 0.1*x1 + 0.1*x2 + 0.25*x1*x2 + 0.4*x3 -0.1*x4 + stde*rnorm(N))
stde <- 8
dat$y3 <- with(dat, 3 + 0.5*x1 + 1.2 * (as.numeric(xcat1)-1) +
-0.8* (as.numeric(xcat1)-1) * x1 +  stde * rnorm(N))

stde <- 8

dat$y4 <- with(dat, 3 + 0.5*x1 + xcat2n \%*\% c(0.1, -0.2, 0.3, 0.05)  + stde * rnorm(N))


## Curvature with interaction
m1 <- lm(y ~ log(x1)*xcat1 + x2 + I(x2^2), data=dat)
summary(m1)
plotCurves(m1, plotx="x1", modx="xcat1")

## Verify that plot by comparing against a manually contructed alternative
par(mfrow=c(1,2))
plotCurves(m1, plotx="x1", modx="xcat1")
newdat <- with(dat, expand.grid(x1 = plotSeq(x1, 30), xcat1=levels(xcat1)))
newdat$x2 <-  with(dat, mean(x2, na.rm=TRUE))
newdat$m1p <- predict(m1, newdata=newdat)
plot( y ~ x1, data=dat, type="n")
points( y ~ x1, data=dat, col=dat$xcat1)
by(newdat, newdat$xcat1, function(dd) {lines(dd$x1, dd$m1p)})
legend("topleft", legend=levels(dat$xcat1), col=as.numeric(dat$xcat1), lty=1)
par(mfrow=c(1,1))
##Close enough!


plotCurves(m1, plotx="x2", modx="x1")
##OK

plotCurves(m1, plotx="x2", modx="xcat1")
##OK

m2 <- lm(y ~ log(x1)*xcat1 + xcat1*(x2 + I(x2^2)), data=dat)
summary(m2)
plotCurves(m2, plotx="x2", modx="xcat1")
##OK 

plotCurves(m2, plotx="x2", modx="x1")
##OK

plotCurves(m2, plotx="x2", modx="x1")
##OK



m3a <- lm(y ~ poly(x2,2) + xcat1, data=dat)
plotCurves(m3a, plotx="x2", modx="xcat1")
#OK

m3b <- lm(y ~ x2 + I(x2^2) + xcat1, data=dat)
plotCurves(m3b, plotx="x2", modx="xcat1")
#OK

m4 <- lm(log(y+10) ~ poly(x2, 2)*xcat1 + x1, data=dat)
summary(m4)
plotCurves(m4, plotx="x2", modx="xcat1")
#OK
plotCurves(m4, plotx="x2", modx="x1")
#OK
plotCurves(m4, plotx="x2", modx="xcat1", modxVals=c("Monster"))
#OK

##ordinary interaction
m5 <- lm(y2 ~ x1*x2 + x3 +x4, data=dat)
summary(m5)
plotCurves(m5, plotx="x1", modx="x2")
plotCurves(m5, plotx="x1", modx="x2", modxVals=c( -2, -1, 0, 1, 2))
plotCurves(m5, plotx="x1", modx="x2", modxVals=c(-2 ))
plotCurves(m5, plotx="x1", modx="x2", modxVals="std.dev.")
plotCurves(m5, plotx="x1", modx="x2", modxVals="quantile")
plotCurves(m5, plotx="x3", modx="x2")


library(car)
mc1 <- lm(statusquo ~ income * sex, data = Chile)
summary(mc1)
plotCurves(mc1, modx = "sex", plotx = "income")
plotCurves(mc1, modx = "sex", plotx = "income", modxVals = "M")

mc2 <- lm(statusquo ~ region * income, data= Chile)
summary(mc2)
plotCurves(mc2, modx = "region", plotx = "income")
plotCurves(mc2, modx = "region", plotx = "income", modxVals = levels(Chile$region)[c(1,4)])
plotCurves(mc2, modx = "region", plotx = "income", modxVals = c("S","M","SA"))

plotCurves(mc2, modx = "region", plotx = "income", plotPoints=FALSE)


mc3 <- lm(statusquo ~ region * income + sex + age, data= Chile)
summary(mc3)
plotCurves(mc3, modx = "region", plotx = "income")


mc4 <- lm(statusquo ~ income * (age + I(age^2)) + education + sex + age, data=Chile)
summary(mc4)
plotCurves(mc4, modx = "income", plotx = "age")
plotCurves(mc4, modx = "income", plotx = "age", plotPoints=FALSE)

plotCurves(mc4, modx = "age", plotx = "income")
plotCurves(mc4, modx = "income", plotx = "age", plotPoints=FALSE)
}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}

