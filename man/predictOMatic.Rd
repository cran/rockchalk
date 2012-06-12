\name{predictOMatic}
\alias{predictOMatic}
\title{predictOMatic creates predicted values for a fitted regression model.}
\usage{
  predictOMatic(model = NULL, fl = NULL,
    divider = "quantile", n = 3, ...)
}
\arguments{
  \item{model}{Required. A fitted regression model}

  \item{fl}{(focus list). Optional. A named list of
  variables and values, such as fl = list("x1" = c(1,2,3),
  "x2" = c("M","F"), "x3" = quantile(dat$x3)). Must name
  only predictors that are fitted in \code{model}.  Need
  not include all predictors in a model. Predictor
  variables in \code{model} that are not named in fl will
  be set to mean or mode values. See details and examples.}

  \item{divider}{Optional. If fl is not specified,
  automatic selection of predictor values is employed.
  \code{divider} determines the method of selection. Should
  be one of c("quantile","std.dev","table"). This
  determines whether values selected are quantile values,
  values based on the mean plus-or-minus standard deviation
  values, or a table of most frequently occurring values.
  Documentation for the details can be found in the
  functions \code{cutByTable}, \code{cutByQuantile}, and
  \code{cutBySD}.}

  \item{n}{If fl is not specified, automatic selection of
  predictor values is employed. This determines the number
  of values for which predictions are sought.}

  \item{...}{Optional arguments to be passed to the predict
  function}
}
\value{
  A data frame or a list of data frames.
}
\description{
  If a "focus list" is supplied, predictOMatic supplies
  predicted values only for those selected input values. If
  no "focus list" is supplied, predictOMatic supplies a
  prediction summary for each separate independent
  variable. That is, in a model with formula y ~ x1 + x2 +
  x3, then separate tables of predicted values will be
  supplied, one for each of x1, x2, and x3.
}
\details{
  It may be important to make sure that diagnostic plots
  and summaries of predictions are calculated with the
  exact same data that was used to fit the model. The
  function \code{model.data} is intended to facilitate that
  comparison. One can fit a model, use model.data to get
  the data that was used, and then use that extracted data
  to decide on values to be set in the focus list.

  For example, create a copy of the data from a model m1
  with

  m1dat <- model.data(m1)

  and then use m1dat to select values that might be
  predicted in a command like

  predictOMatic( m1, fl = list("x1" = median(m1dat$x1),
  "x2"=c(1,2,3), "x3" = quantile(m1dat$x3))
}
\examples{
library(rockchalk)

set.seed(12345)
N <- 100
x1 <- rpois(N, l=6)
x2 <- rnorm(N, m=50, s=10)
x3 <- rnorm(N)
xcat1 <- gl(2,50, labels=c("M","F"))
xcat2 <- cut(rnorm(N), breaks=c(-Inf, 0, 0.4, 0.9, 1, Inf), labels=c("R", "M", "D", "P", "G"))
dat <- data.frame(x1, x2, x3, xcat1, xcat2)
rm(x1, x2, x3, xcat1, xcat2)
xcat1n <- with(dat, contrasts(xcat1)[xcat1, ,drop=FALSE])
xcat2n <- with(dat, contrasts(xcat2)[xcat2, ])
STDE <- 15
dat$y <- 0.03 + 0.8*dat$x1 + 0.1*dat$x2 + 0.7*dat$x3 + xcat1n \%*\% c(2) + xcat2n \%*\% c(0.1,-2,0.3, 0.1) + STDE*rnorm(N)
rownames(dat$y) <- NULL
## rownames of dat$y don't match rownames of dat. Humphf.
## I've not seen that problem before

dat$x1[sample(N, 5)] <- NA
dat$x2[sample(N, 5)] <- NA
dat$x3[sample(N, 5)] <- NA
dat$xcat2[sample(N, 5)] <- NA
dat$xcat1[sample(N, 5)] <- NA
dat$y[sample(N, 5)] <- NA


m0 <- lm(y ~ x1 + x2 + xcat1, data = dat)
summary(m0)
m0.data <- model.data(m0)
summarize(m0.data)

(m0.p1 <- predictOMatic(m0))
(m0.p2 <- predictOMatic(m0, interval = "confidence"))

(m0.p3 <- predictOMatic(m0, divider="std.dev.", n=5))

(m0.p3 <- predictOMatic(m0, fl = list("x1" = c(6,7), "xcat1" = levels(m0.data$xcat1))))






m1 <- lm(y ~ log(10+x1) + sin(x2) + x3, data=dat)
m1.data <- model.data(m1)
summarize(m1.data)

(newdata(m1))
(newdata(m1, fl = list(x1=c(6, 8, 10))))
(newdata(m1, fl = list(x1 = c(6, 8, 10), x3 = c(-1,0,1))))
(newdata(m1, fl = list(x1 = c(6, 8, 10), x2 = quantile(m1.data$x2), x3 = c(-1,0,1))))

(m1.p1 <- predictOMatic(m1, divider="std.dev", n = 5))
(m1.p1 <- predictOMatic(m1, divider="quantile", n = 5))

(m1.p1 <- predictOMatic(m1, fl=list(x1=c(6, 8, 10), x2 = median(m1.data$x2, na.rm = TRUE))))
(m1.p1 <- predictOMatic(m1, fl=list(x1=c(6, 8, 10), x2 = quantile(m1.data$x2, na.rm = TRUE))))

(m1.p1 <- predictOMatic(m1))
(m1.p1 <- predictOMatic(m1, divider="std.dev."))
(m1.p1 <- predictOMatic(m1, divider="std.dev.", n=5))
(m1.p1 <- predictOMatic(m1, divider="std.dev.", interval="confidence"))




m2 <- lm(y ~ x1 + x2 + x3 + xcat1 + xcat2, data = dat)


##  has only columns and rows used in model fit
m2.data <- model.data(m2)
summarize(m2.data)

newdata(m2, fl = list(xcat1 = levels(m2.data$xcat1)))
## mix and match all combinations of xcat1 and xcat2
newdata(m2, fl = list(xcat1 = levels(m2.data$xcat1), xcat2 = levels(m2.data$xcat2)))

m2.new1 <- newdata(m2, fl = list(xcat1 = levels(m2.data$xcat1), xcat2 = levels(m2.data$xcat2)))

predict(m2, newdata = m2.new1)

## Pick some particular values for focus
m2.new2 <- newdata(m2, fl = list(x1 = c(1,2,3), xcat2 = c("M","D")))
## Ask for predictions
predict(m2, newdata = m2.new2)




## Compare: predictOMatic generates a newdata frame and predictions in one step

(m2.p1 <- predictOMatic(m2, fl = list(xcat1 = levels(m2.data$xcat1), xcat2 = levels(m2.data$xcat2))))


(m2.p2 <- predictOMatic(m2, fl = list(x1 = c(1,2,3), xcat2 = c("M","D"))))

(m2.p3 <- predictOMatic(m2, fl = list(x2 = c(0.25, 1.0), xcat2 = c("M","D"))))

(m2.p4 <- predictOMatic(m2, fl = list(x2 = plotSeq(m2.data$x2, 10) , xcat2 = c("M","D"))))

(m2.p5 <- predictOMatic(m2, fl = list(x2 = c(0.25, 1.0), xcat2 = c("M","D")), interval="conf"))

(predictOMatic(m2, interval="conf"))

(m2.p6 <- predictOMatic(m2, fl = list(x2 = c(49, 51), xcat2 = levels(m2.data$xcat2), x1=plotSeq(dat$x1))))

plot(y ~ x1, data= m2.data)
by(m2.p6, list(m2.p6$xcat2), function(x) {lines(x$x1, x$fit, col=x$xcat2, lty=as.numeric(x$xcat2))})

m2.newdata <- newdata(m2, fl = list(x2 = c(48, 50, 52), xcat2 = c("M","D")))
predict(m2, newdata = m2.newdata)

(m2.p7 <- predictOMatic(m2, fl = list(x2 = c(48, 50, 52), xcat2 = c("M","D"))))

(m2.p8 <- predictOMatic(m2, fl = list(x2 = range(m2.data$x2), xcat2 = c("M","D"))))

(m2.p9 <- predictOMatic(m2, fl = list(x2 = plotSeq(m2.data$x2), x1 = quantile(m2.data$x1, pr =c(0.33, 0.66), na.rm = TRUE), xcat2 = c("M","D"))))
plot(y ~ x2 , data = m2.data)
by(m2.p9, list(m2.p9$x1, m2.p9$xcat2), function(x) {lines(x$x2, x$fit)})



(predictOMatic(m2, fl = list(x2 = c(50, 60), xcat2 = c("M","D")), interval="conf"))


## create a dichotomous dependent variable
y2 <- ifelse(rnorm(N) > 0.3, 1, 0)
dat <- cbind(dat, y2)

m3 <- glm(y2 ~ x1 + x2 + x3 + xcat1, data=dat, family=binomial(logit))
summary(m3)
m3.data <- model.data(m3)
summarize(m3.data)

(m3.p1 <- predictOMatic(m3, divider="std.dev.", type="response"))

(m3.p2 <- predictOMatic(m3, fl = list(x2 = c(40, 50, 60), xcat1 = c("M","F")), divider="response", interval="conf"))

## Want a full accounting for each value of x2?
(m3.p3 <- predictOMatic(m3, fl = list(x2 = unique(m3.data$x2), xcat1 = c("M","F")), interval="conf", type="response"))


## Would like to write a more beautiful print method
## for output object, but don't want to obscure structure from user.
for (i in names(m3.p1)){
    dns <- cbind(m3.p1[[i]][i], m3.p1[[i]]$fit)
    colnames(dns) <- c(i, "predicted")
    print(dns)
}


}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}

