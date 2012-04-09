\name{plotSlopes}
\alias{plotSlopes}
\title{Assists creation of predicted value lines for values of a moderator variable.}
\usage{
  plotSlopes(model = NULL, plotx = NULL, modx = NULL,
    modxVals = NULL, plotPoints = TRUE, plotLegend = TRUE,
    col, ...)
}
\arguments{
  \item{model}{Required. Fitted regression object. Must
  have a predict method}

  \item{plotx}{Required. String with name of IV to be
  plotted on x axis}

  \item{modx}{Required. String for moderator variable name.
  May be either numeric or factor.}

  \item{modxVals}{Optional. If modx is numeric, either a
  character string, "quantile", "std.dev.", or "table", or
  a vector of values for which plotted lines are sought. If
  modx is a factor, the default approach will create one
  line for each level, but the user can supply a vector of
  levels if a subset is desired..}

  \item{plotPoints}{TRUE or FALSE: Should the plot include
  the scatterplot points along with the lines.}

  \item{plotLegend}{TRUE or FALSE: Include a default
  legend. Set to FALSE if use wants to run a different
  legend command after the plot has been drawn.}

  \item{col}{An optional color vector.  By default, the R's
  builtin colors will be used, which are "black", "red",
  and so forth.  Instead, a vector of color names can be
  supplied, as in c("pink","black", "gray70").  A
  color-vector generating function like rainbow(10) or
  gray.colors(5) can also be used. A vector of color names
  can be supplied with this function. Color names will be
  recycled if the plot requires more different colors than
  the user provides.}

  \item{...}{further arguments that are passed to plot}
}
\value{
  The plot is drawn on the screen, and the return object
  includes the "newdata" object that was used to create the
  plot, along with the "modxVals" vector, the values of the
  moderator for which lines were drawn. It also includes
  the call that generated the plot.
}
\description{
  This is a "simple slope" plotter for linear regression.
  The term "simple slopes" was coined by psychologists
  (Aiken and West, 1991; Cohen, et al 2002) to refer to
  analysis of interaction effects for particular values of
  a moderating variable, be it continuous or categorical.
  To use this function, the user should estimate a
  regression (with as many variables as desired, including
  interactions) and the resulting regression object is then
  supplied to this function, along with user requests for
  plots of certain variables.
}
\details{
  The variable \code{plotx} will be the horizontal plotting
  variable; it must be numeric.  The variable \code{modx}
  is the moderator variable. It may be either a numeric or
  a factor variable.  A line will be drawn to represent the
  predicted value for selected values of the moderator.

  The parameter \code{modxVals} is optional.  It is used to
  fine-tune the values of the moderator that are used to
  create the simple slope plot.  Numeric and factor
  moderators are treated differently. If the moderator is a
  numeric variable, then some particular values must be
  chosen for plottings. If the user does not specify the
  parameter \code{modxVals}, then lines will be drawn for
  the quantile values of the moderator.  If the moderator
  is a factor, then lines are drawn for each different
  value of the factor variable, unless the user specifies a
  subset of levels with the \code{modxVals} parameter.

  For numeric moderators, the user may specify a vector of
  values for the numeric moderator variable, such as
  c(1,2,3). The user may also specify an algorithm, either
  "quantile" (which would be selected by default) or
  "std.dev." The alternative method at this time is
  "std.dev.", which causes 5 lines to be drawn. These lines
  are the "standard deviations about the mean of
  \code{modx}" lines, at which modx is set at mean - k*
  standard deviation, and k takes on values -2, -1, 0, 1,
  2.

  Here is a wrinkle. There can be many variables in a
  regression model, and we are plotting only for the
  \code{plotx} and \code{modx} variables. How should we
  calculate predicted values when the values of the other
  variables are required?  For the other variables, the
  ones that are not explicitly inlcluded in the plot, we
  use the mean and mode, for numeric or factor variables
  (respectively). Those values can be reviewed in the
  newdata object that is created as a part of the output
  from this function
}
\examples{
set.seed(12345)
x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- rnorm(100)
x4 <- rnorm(100)
xcat1 <- gl(2,50, labels=c("M","F"))
xcat2 <- cut(rnorm(100), breaks=c(-Inf, 0, 0.4, 0.9, 1, Inf), labels=c("R", "M", "D", "P", "G"))
dat <- data.frame(x1, x2, x3, x4, xcat1, xcat2)
rm(x1, x2, x3, x4, xcat1, xcat2)

##ordinary regression. 
dat$y <- with(dat, 0.03 + 0.1*x1 + 0.1*x2 + 0.4*x3 -0.1*x4 + 2*rnorm(100))
m1 <- lm(y ~ x1 + x2 +x3 + x4, data=dat)
## These will be parallel lines emf 
plotSlopes(m1, plotx="x1", modx="x2")
plotSlopes(m1, plotx="x1", modx="x2", modxVals=c(-0.5,0,0.5))
plotSlopes(m1, plotx="x1", modx="x2", modxVals="std.dev.", main="A plotSlopes result with \\"std.dev.\\" values of modx")


plotSlopes(m1, plotx="x1", modx="x2", modxVals="std.dev.", ylab="Call Y What You Want")
plotSlopes(m1, plotx="x1", modx="x2")
plotSlopes(m1, plotx="x4", modx="x1")


## now some numeric interactions worth plotting
dat$y2 <- with(dat, 0.03 + 0.1*x1 + 0.1*x2 + 0.25*x1*x2 + 0.4*x3 -0.1*x4 + 1*rnorm(100))

m2 <- lm(y2 ~ x1*x2 + x3 + x4, data=dat)
summary(m2)
plotSlopes(m2, plotx="x1", modx="x2")
plotSlopes(m2, plotx="x1", modx="x2", modxVals=c( -2, -1, 0, 1))
plotSlopes(m2, plotx="x2", modx="x1", modxVals="std.dev.")
plotSlopes(m2, plotx="x2", modx="x1", modxVals="std.dev.", xlab="Any label You Want")

## Catch output, send to testSlopes

m2ps1 <- plotSlopes(m2, plotx="x1", modx="x2")
testSlopes(m2ps1)

### Examples with categorical Moderator variable

stde <- 8
dat$y3 <- with(dat, 3 + 0.5*x1 + 1.2 * (as.numeric(xcat1)-1) +
-0.8* (as.numeric(xcat1)-1) * x1 +  stde * rnorm(100))

m3 <- lm (y3 ~ x1 + xcat1, data=dat)
plotSlopes(m3, modx = "xcat1", plotx = "x1")

m4 <- lm (y ~ x1 * xcat1, data=dat)
summary(m4)
plotSlopes(m4, modx = "xcat1", plotx = "x1")

dat$xcat2n <- with(dat, contrasts(xcat2)[xcat2, ])
dat$y4 <- with(dat, 3 + 0.5*x1 + xcat2n \%*\% c(0.1, -0.2, 0.3, 0.05)  + stde * rnorm(100))
m5 <- lm(y4 ~ x1 + xcat2, data=dat)
plotSlopes(m5, plotx="x1", modx="xcat2")
m6 <- lm(y4 ~ x1 * xcat2, data=dat)
plotSlopes(m6, plotx="x1", modx="xcat2")

## Make data with a more pronounced interaction
dat$y5 <- with(dat, 3 + 0.5*x1 + xcat2n \%*\% c(0.1, -0.2, 0.3, 0.05)  + (xcat2n \%*\% c(0.-1, 0.2, -0.3, 0.25)  )*x1 + stde * rnorm(100))
m7 <- lm(y4 ~ x1 * xcat2, data=dat)
plotSlopes(m7, plotx="x1", modx="xcat2")
##only plot first and third levels
m7ps <- plotSlopes(m7, plotx="x1", modx="xcat2", modxVals=levels(dat$xcat2)[c(1,3)]) 
##see what testSlopes says about this one
##testSlopes(m7ps)

## Now examples with real data
library(car)
m3 <- lm(statusquo ~ income * sex, data = Chile)
summary(m3)
plotSlopes(m3, modx = "sex", plotx = "income")


m4 <- lm(statusquo ~ region * income, data= Chile)
summary(m4)
plotSlopes(m4, modx = "region", plotx = "income")

plotSlopes(m4, modx = "region", plotx = "income", plotPoints=FALSE)


m5 <- lm(statusquo ~ region * income + sex + age, data= Chile)
summary(m5)
plotSlopes(m5, modx = "region", plotx = "income")

m6 <- lm(statusquo ~ income * age + education + sex + age, data=Chile)
summary(m6)
plotSlopes(m6, modx = "income", plotx = "age")

plotSlopes(m6, modx = "income", plotx = "age", plotPoints=FALSE)


## Should cause error because education is not numeric
## m7 <- lm(statusquo ~ income * age + education + sex + age, data=Chile)
## summary(m7)
## plotSlopes(m7, modx = "income", plotx = "education")

## Should cause error because "as.numeric(education") not same as
## plotx="education"
## m8 <- lm(statusquo ~ income * age + as.numeric(education) + sex + age, data=Chile)
## summary(m8)
## plotSlopes(m8, modx = "income", plotx = "education")

## Still fails. 
## plotSlopes(m8, modx = "income", plotx = "as.numeric(education)")

## Must recode variable first so that variable name is coherent
Chile$educationn <- as.numeric(Chile$education)
m9 <- lm(statusquo ~ income * age + educationn + sex + age, data=Chile)
summary(m9)
plotSlopes(m9, modx = "income", plotx = "educationn")
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
}
\seealso{
  plotCurves and testSlopes
}

