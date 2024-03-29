% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictOMatic.R
\name{predictOMatic}
\alias{predictOMatic}
\title{Create predicted values after choosing values of predictors.  Can
demonstrate marginal effects of the predictor variables.}
\usage{
predictOMatic(
  model = NULL,
  predVals = "margins",
  divider = "quantile",
  n = 5,
  ...
)
}
\arguments{
\item{model}{Required. A fitted regression model. A \code{predict}
method must exist for that model.}

\item{predVals}{Optional. How to choose predictor values?  Can be
as simple as a keyword "auto" or "margins". May also be very
fine-grained detail, including 1) a vector of variable names (for
which values will be automatically selected) 2) a named
vector of variable names and divider functions, or 3) a list
naming variables and values. See details and examples.}

\item{divider}{An algorithm name from c("quantile", "std.dev",
"seq", "table") or a user-provided function.  This sets the method
for selecting values of the predictor. Documentation for the
rockchalk methods can be found in the functions
\code{cutByQuantile}, \code{cutBySD}, \code{plotSeq}, and
\code{cutByTable},.}

\item{n}{Default = 5. The number of values for which
predictions are sought.}

\item{...}{Optional arguments to be passed to the predict
function. In particular, the arguments se.fit and interval are
extracted from ... and used to control the output.}
}
\value{
A data frame or a list of data frames.
}
\description{
It creates "newdata" frames which are passed
to predict. The key idea is that each predictor has certain focal
values on which we want to concentrate. We want a more-or-less
easy way to spawn complete newdata objects along with fitted values.
The \code{newdata} function creates those objects, its documentation
might be helpful in understanding some nuances.
}
\details{
If no predVals argument is supplied (same as
\code{predVals = "margins"}, predictOMatic creates a list of new
data frames, one for each predictor variable. It uses the default
divider algorithm (see the divider argument) and it estimates
predicted values for \code{n} different values of the predictor. A
model with formula \code{y ~ x1 + x2 + x3} will cause 3 separate
output data frames, one for each predictor. They will be named
objects in the list.

The default approach will have marginal tables, while the setting
\code{predVals = "auto"} will create a single large newdata frame
that holds the Cartesian product of the focal values of each predictor.

\code{predVals} may be a vector of variable names, or it may be a
list of names and particular values. Whether a vector or a list is supplied,
\code{predVals} must name only predictors that are fitted in the
model. \code{predictOMatic} will choose the mean or mode for
variables that are not explicitly listed, and selected values of
the named variables are "mixed and matched" to make a data set.
There are many formats in which it can be supplied.  Suppose a
regression formula is \code{y1 ~ sex + income + health +
height}. The simplest format for predVals will be a vector of
variable names, leaving the selection of detailed values to the
default algorithms. For example, \code{predVals = c("income",
"height")} will cause sex and health to be set at central values
and income and height will have target values selected according
to the divider algorithm (see the argument \code{divider}).

The user can spcecify divider algoriths to choose focal values,
\code{predvals = c(income = "quantile", height = "std.dev.")}. The
dividers provided by the rockchalk package are "quantile",
"std.dev.", "seq" and "table".  Those are discussed more
completely in the help for \code{focalVals}.  The appropriate
algorithms will select focal values of the predictors and they
will supply \code{n} values for each in a "mix and match" data
frame. After rockchalk 1.7.2, the divider argument can also be the
name of a function, such as R's pretty.

Finally, users who want very fine grained control over
predictOMatic can supply a named list of predictor
values. For example,
\code{predVals = list(height = c(5.5, 6.0, 6.5),
income = c(10, 20, 30, 40, 50), sex = levels(dat$sex))}. One can
also use algorithm names, \code{predVals = list(height =
c(5.5, 6.0, 6.5), income = "quantile")} and so forth. Examples are
offered below.

The variables named in the \code{predVals} argument should be the names
of the variables in the raw data frame, not the names that R
creates when it interprets a formula. We want "x", not the
transformation in the functions (not \code{log(x)}, or
\code{as.factor(x)} or \code{as.numeric(x)}). If a formula has a
predictor \code{poly(height, 3)}, then the predVals argument
should refer to height, not \code{poly(height, 3)}.  I've invested
quite a bit of effort to make sure this "just works" (many
alternative packages that calculate predicted values do not).

It it important to make sure that diagnostic plots and summaries
of predictions are calculated with the exact same data that was
used to fit the model. This is surprisingly difficult because
formulas can include things like log(income + d) and so forth. The
function \code{model.data} is the magic bullet for that part of
the problem.

Here is one example sequence that fits a model, discerns some
focal values, and then uses predictOMatic.

\code{
d <- 3
alpha <- 13
m1 <- lm(yout ~ xin + xout + poly(xother,2) + log(xercise + alpha), data = dat)
m1dat <- model.data(m1)
}

Now, when you are thinking about which values you might like to
specify in predVals, use m1dat to decide. Try

\code{summarize(m1dat)}

Then run something like

\code{predictOMatic( m1, predVals = list(xin = median(m1dat$xin), xout =
c(1,2,3), xother = quantile(m1dat$xother))}

Get the idea?
}
\examples{
library(rockchalk)

## Replicate some R classics.  The budworm.lg data from predict.glm
## will work properly after re-formatting the information as a data.frame:

## example from Venables and Ripley (2002, pp. 190-2.)
df <- data.frame(ldose = rep(0:5, 2),
                 sex = factor(rep(c("M", "F"), c(6, 6))),
                 SF.numdead = c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16))
df$SF.numalive = 20 - df$SF.numdead

budworm.lg <- glm(cbind(SF.numdead, SF.numalive) ~ sex*ldose,
                  data = df,  family = binomial)

predictOMatic(budworm.lg)

predictOMatic(budworm.lg, n = 7)

predictOMatic(budworm.lg, predVals = c("ldose"), n = 7)

predictOMatic(budworm.lg, predVals = c(ldose = "std.dev.", sex = "table"))



## Now make up a data frame with several numeric and categorical predictors.

set.seed(12345)
N <- 100
x1 <- rpois(N, l = 6)
x2 <- rnorm(N, m = 50, s = 10)
x3 <- rnorm(N)
xcat1 <- gl(2,50, labels = c("M","F"))
xcat2 <- cut(rnorm(N), breaks = c(-Inf, 0, 0.4, 0.9, 1, Inf),
             labels = c("R", "M", "D", "P", "G"))
dat <- data.frame(x1, x2, x3, xcat1, xcat2)
rm(x1, x2, x3, xcat1, xcat2)
dat$xcat1n <- with(dat, contrasts(xcat1)[xcat1, , drop = FALSE])
dat$xcat2n <- with(dat, contrasts(xcat2)[xcat2, ])
STDE <- 15
dat$y <- with(dat,
              0.03 + 0.8*x1 + 0.1*x2 + 0.7*x3 + xcat1n \%*\% c(2) +
              xcat2n \%*\% c(0.1,-2,0.3, 0.1) + STDE*rnorm(N))
## Impose some random missings
dat$x1[sample(N, 5)] <- NA
dat$x2[sample(N, 5)] <- NA
dat$x3[sample(N, 5)] <- NA
dat$xcat2[sample(N, 5)] <- NA
dat$xcat1[sample(N, 5)] <- NA
dat$y[sample(N, 5)] <- NA
summarize(dat)


m0 <- lm(y ~ x1 + x2 + xcat1, data = dat)
summary(m0)
## The model.data() function in rockchalk creates as near as possible
## the input data frame.
m0.data <- model.data(m0)
summarize(m0.data)

## no predVals: analyzes each variable separately
(m0.p1 <- predictOMatic(m0))

## requests confidence intervals from the predict function
(m0.p2 <- predictOMatic(m0, interval = "confidence"))

## predVals as vector of variable names: gives "mix and match" predictions
(m0.p3 <- predictOMatic(m0, predVals = c("x1", "x2")))

## predVals as vector of variable names: gives "mix and match" predictions
(m0.p3s <- predictOMatic(m0, predVals = c("x1", "x2"), divider = "std.dev."))

## "seq" is an evenly spaced sequence across the predictor.
(m0.p3q <- predictOMatic(m0, predVals = c("x1", "x2"), divider = "seq"))

(m0.p3i <- predictOMatic(m0, predVals = c("x1", "x2"),
                         interval = "confidence", n = 3))

(m0.p3p <- predictOMatic(m0, predVals = c("x1", "x2"), divider = pretty))

## predVals as vector with named divider algorithms.
(m0.p3 <- predictOMatic(m0, predVals = c(x1 = "seq", x2 = "quantile")))
## predVals as named vector of divider algorithms

## same idea, decided to double-check
(m0.p3 <- predictOMatic(m0, predVals = c(x1 = "quantile", x2 = "std.dev.")))
getFocal(m0.data$x2, xvals =  "std.dev.", n = 5)


## Change from quantile to standard deviation divider
(m0.p5 <- predictOMatic(m0, divider = "std.dev.", n = 5))

## Still can specify particular values if desired
(m0.p6 <- predictOMatic(m0, predVals = list("x1" = c(6,7),
                            "xcat1" = levels(m0.data$xcat1))))

(m0.p7 <- predictOMatic(m0, predVals = c(x1 = "quantile", x2 = "std.dev.")))
getFocal(m0.data$x2, xvals =  "std.dev.", n = 5)

(m0.p8 <- predictOMatic(m0, predVals = list( x1 = quantile(m0.data$x1,
                        na.rm = TRUE, probs = c(0, 0.1, 0.5, 0.8,
                        1.0)), xcat1 = levels(m0.data$xcat1))))

(m0.p9 <- predictOMatic(m0, predVals = list(x1 = "seq", "xcat1" =
                                levels(m0.data$xcat1)), n = 8) )


(m0.p10 <- predictOMatic(m0, predVals = list(x1 = "quantile",
                                 "xcat1" = levels(m0.data$xcat1)), n =  5) )


(m0.p11 <- predictOMatic(m0, predVals = c(x1 = "std.dev."), n = 10))

## Previous same as

(m0.p11 <- predictOMatic(m0, predVals = c(x1 = "default"), divider =
                         "std.dev.", n = 10))

## Previous also same as

(m0.p11 <- predictOMatic(m0, predVals = c("x1"), divider = "std.dev.", n = 10))


(m0.p11 <- predictOMatic(m0,  predVals = list(x1 = c(0, 5, 8), x2 = "default"),
                         divider = "seq"))



m1 <- lm(y ~ log(10+x1) + sin(x2) + x3, data = dat)
m1.data <- model.data(m1)
summarize(m1.data)


(newdata(m1))
(newdata(m1, predVals = list(x1 = c(6, 8, 10))))
(newdata(m1, predVals = list(x1 = c(6, 8, 10), x3 = c(-1,0,1))))
(newdata(m1, predVals = list(x1 = c(6, 8, 10),
                 x2 = quantile(m1.data$x2, na.rm = TRUE), x3 = c(-1,0,1))))

(m1.p1 <- predictOMatic(m1, divider = "std.dev", n = 5))
(m1.p2 <- predictOMatic(m1, divider = "quantile", n = 5))

(m1.p3 <- predictOMatic(m1, predVals = list(x1 = c(6, 8, 10),
                            x2 = median(m1.data$x2, na.rm = TRUE))))

(m1.p4 <- predictOMatic(m1, predVals = list(x1 = c(6, 8, 10),
                                x2 = quantile(m1.data$x2, na.rm = TRUE))))

(m1.p5 <- predictOMatic(m1))
(m1.p6 <- predictOMatic(m1, divider = "std.dev."))
(m1.p7 <- predictOMatic(m1, divider = "std.dev.", n = 3))
(m1.p8 <- predictOMatic(m1, divider = "std.dev.", interval = "confidence"))


m2 <- lm(y ~ x1 + x2 + x3 + xcat1 + xcat2, data = dat)
##  has only columns and rows used in model fit
m2.data <- model.data(m2)
summarize(m2.data)

## Check all the margins
(predictOMatic(m2, interval = "conf"))

## Lets construct predictions the "old fashioned way" for comparison

m2.new1 <- newdata(m2, predVals = list(xcat1 = levels(m2.data$xcat1),
                           xcat2 = levels(m2.data$xcat2)), n = 5)
predict(m2, newdata = m2.new1)


(m2.p1 <- predictOMatic(m2,
                        predVals = list(xcat1 = levels(m2.data$xcat1),
                            xcat2 = levels(m2.data$xcat2)),
                        xcat2 = c("M","D")))
## See? same!

## Pick some particular values for focus
m2.new2 <- newdata(m2, predVals = list(x1 = c(1,2,3), xcat2 = c("M","D")))
## Ask for predictions
predict(m2, newdata = m2.new2)


## Compare: predictOMatic generates a newdata frame and predictions in one step

(m2.p2 <- predictOMatic(m2, predVals = list(x1 = c(1,2,3),
                                xcat2 = c("M","D"))))

(m2.p3 <- predictOMatic(m2, predVals = list(x2 = c(0.25, 1.0),
                                xcat2 = c("M","D"))))

(m2.p4 <- predictOMatic(m2, predVals = list(x2 = plotSeq(m2.data$x2, 10),
                                xcat2 = c("M","D"))))

(m2.p5 <- predictOMatic(m2, predVals = list(x2 = c(0.25, 1.0),
                                xcat2 = c("M","D")), interval = "conf"))

(m2.p6 <- predictOMatic(m2, predVals = list(x2 = c(49, 51),
                                xcat2 = levels(m2.data$xcat2),
                                x1 = plotSeq(dat$x1))))

plot(y ~ x1, data = m2.data)
by(m2.p6, list(m2.p6$xcat2), function(x) {
    lines(x$x1, x$fit, col = x$xcat2, lty = as.numeric(x$xcat2))
})

m2.newdata <- newdata(m2, predVals = list(x2 = c(48, 50, 52),
                              xcat2 = c("M","D")))
predict(m2, newdata = m2.newdata)

(m2.p7 <- predictOMatic(m2, predVals = list(x2 = c(48, 50, 52),
                                xcat2 = c("M","D"))))

(m2.p8 <- predictOMatic(m2,
             predVals = list(x2 = range(m2.data$x2, na.rm = TRUE),
             xcat2 = c("M","D"))))

(m2.p9 <- predictOMatic(m2, predVals = list(x2 = plotSeq(m2.data$x2),
             x1 = quantile(m2.data$x1, pr =c(0.33, 0.66), na.rm = TRUE),
             xcat2 = c("M","D"))))
plot(y ~ x2 , data = m2.data)

by(m2.p9, list(m2.p9$x1, m2.p9$xcat2), function(x) {lines(x$x2, x$fit)})



(predictOMatic(m2, predVals = list(x2 = c(50, 60), xcat2 = c("M","D")),
               interval = "conf"))

## create a dichotomous dependent variable
y2 <- ifelse(rnorm(N) > 0.3, 1, 0)
dat <- cbind(dat, y2)

m3 <- glm(y2 ~ x1 + x2 + x3 + xcat1, data = dat, family = binomial(logit))
summary(m3)
m3.data <- model.data(m3)
summarize(m3.data)

(m3.p1 <- predictOMatic(m3, divider = "std.dev."))

(m3.p2 <- predictOMatic(m3, predVals = list(x2 = c(40, 50, 60),
                             xcat1 = c("M","F")),
                        divider = "std.dev.", interval = "conf"))

## Want a full accounting for each value of x2?
(m3.p3 <- predictOMatic(m3,
                predVals = list(x2 = unique(m3.data$x2),
                    xcat1 = c("M","F")), interval = "conf"))


## Would like to write a more beautiful print method
## for output object, but don't want to obscure structure from user.
## for (i in names(m3.p1)){
##     dns <- cbind(m3.p1[[i]][i], m3.p1[[i]]$fit)
##     colnames(dns) <- c(i, "predicted")
##     print(dns)
## }


}
\author{
Paul E. Johnson \email{pauljohn@ku.edu}
}
