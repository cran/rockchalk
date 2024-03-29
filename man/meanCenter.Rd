% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meanCenter.R
\name{meanCenter}
\alias{meanCenter}
\alias{meanCenter.default}
\title{meanCenter}
\usage{
meanCenter(
  model,
  centerOnlyInteractors = TRUE,
  centerDV = FALSE,
  standardize = FALSE,
  terms = NULL
)

\method{meanCenter}{default}(
  model,
  centerOnlyInteractors = TRUE,
  centerDV = FALSE,
  standardize = FALSE,
  terms = NULL
)
}
\arguments{
\item{model}{a fitted regression model (presumably from lm)}

\item{centerOnlyInteractors}{Default TRUE. If FALSE, all numeric
predictors in the regression data frame are centered before
the regression is conducted.}

\item{centerDV}{Default FALSE. Should the dependent variable be
centered? Do not set this option to TRUE unless the dependent
variable is a numeric variable. Otherwise, it is an error.}

\item{standardize}{Default FALSE. Instead of simply mean-centering
the variables, should they also be "standardized" by first
mean-centering and then dividing by the estimated standard
deviation.}

\item{terms}{Optional. A vector of variable names to be
centered. Supplying this argument will stop meanCenter from
searching for interaction terms that might need to be
centered.}
}
\value{
A regression model of the same type as the input model,
with attributes representing the names of the centered variables.
}
\description{
meanCenter selectively centers or standarizes variables in a regression model.
}
\details{
Works with "lm" class objects, objects estimated by \code{glm()}. This
centers some or all of the the predictors and then re-fits the
original model with the new variables. This is a convenience to
researchers who are often urged to center their predictors.  This
is sometimes suggested as a way to ameliorate multi-collinearity
in models that include interaction terms (Aiken and West, 1991;
Cohen, et al 2002). Mean-centering may enhance interpretation of
the regression intercept, but it actually does not help with
multicollinearity.  (Echambadi and Hess, 2007). This function
facilitates comparison of mean-centered models with others by
calculating centered variables.  The defaults will cause a
regression's numeric interactive variables to be mean
centered. Variations on the arguments are discussed in details.

Suppose the user's formula that fits the original model is
\code{m1 <- lm(y ~ x1*x2 + x3 + x4, data = dat)}. The fitted model
will include estimates for predictors \code{x1}, \code{x2},
\code{x1:x2}, \code{x3} and \code{x4}. By default,
\code{meanCenter(m1)} scans the output to see if there are
interaction terms of the form \code{x1:x2}. If so, then x1 and x2
are replaced by centered versions (m1-mean(m1)) and
(m2-mean(m2)). The model is re-estimated with those new variables.
model (the main effect and the interaction). The resulting thing
is "just another regression model", which can be analyzed or
plotted like any R regression object.

The user can claim control over which variables are centered in
several ways. Most directly, by specifying a vector of variable
names, the user can claim direct control. For example, the
argument \code{terms=c("x1","x2","x3")} would cause 3 predictors
to be centered. If one wants all predictors to be centered, the
argument \code{centerOnlyInteractors} should be set to
FALSE. Please note, this WILL NOT center factor variables. But it
will find all numeric predictors and center them.

The dependent variable will not be centered, unless the user
explicitly requests it by setting centerDV = TRUE.

As an additional convenience to the user, the argument
\code{standardize = TRUE} can be used.  This will divide each
centered variable by its observed standard deviation. For people
who like standardized regression, I suggest this is a better
approach than the \code{standardize} function (which is brain-dead
in the style of SPSS). meanCenter with \code{standardize = TRUE}
will only try to standardize the numeric predictors.

To be completely clear, I believe mean-centering is not helpful
with the multicollinearity problem. It doesn't help, it doesn't
hurt.  Only a misunderstanding leads its proponents to claim
otherwise. This is emphasized in the vignette "rockchalk" that is
distributed with this package.
}
\examples{

library(rockchalk)
N <- 100
dat <- genCorrelatedData(N = N, means = c(100, 200), sds = c(20, 30),
                         rho = 0.4, stde = 10)
dat$x3 <- rnorm(100, m = 40, s = 4)

m1 <- lm(y ~ x1 * x2 + x3, data = dat)
summary(m1)
mcDiagnose(m1)

m1c <- meanCenter(m1)
summary(m1c)
mcDiagnose(m1c)

m2 <- lm(y ~ x1 * x2 + x3, data = dat)
summary(m2)
mcDiagnose(m2)

m2c <- meanCenter(m2, standardize = TRUE)
summary(m2c)
mcDiagnose(m2c)

m2c2 <- meanCenter(m2, centerOnlyInteractors = FALSE)
summary(m2c2)

m2c3 <- meanCenter(m2, centerOnlyInteractors = FALSE, centerDV = TRUE)
summary(m2c3)

dat <- genCorrelatedData(N = N, means = c(100, 200), sds = c(20, 30),
                         rho = 0.4, stde = 10)
dat$x3 <- rnorm(100, m = 40, s = 4)
dat$x3 <- gl(4, 25, labels = c("none", "some", "much", "total"))

m3 <- lm(y ~ x1 * x2 + x3, data = dat)
summary(m3)
## visualize, for fun
plotPlane(m3, "x1", "x2")

m3c1 <- meanCenter(m3)
summary(m3c1)

## Not exactly the same as a "standardized" regression because the
## interactive variables are centered in the model frame,
## and the term "x1:x2" is never centered again.
m3c2 <- meanCenter(m3, centerDV = TRUE,
                   centerOnlyInteractors = FALSE, standardize = TRUE)
summary(m3c2)

m3st <- standardize(m3)
summary(m3st)

## Make a bigger dataset to see effects better
N <- 500
dat <- genCorrelatedData(N = N, means = c(200,200), sds = c(60,30),
                         rho = 0.2, stde = 10)
dat$x3 <- rnorm(100, m = 40, s = 4)
dat$x3 <- gl(4, 25, labels = c("none", "some", "much", "total"))
dat$y2 <- with(dat,
               0.4 - 0.15 * x1 + 0.04 * x1^2 -
               drop(contrasts(dat$x3)[dat$x3, ] \%*\% c(-1.9, 0, 5.1))  +
               1000* rnorm(nrow(dat)))
dat$y2 <- drop(dat$y2)

m4literal <- lm(y2 ~ x1 + I(x1*x1) + x2 + x3, data = dat)
summary(m4literal)
plotCurves(m4literal, plotx="x1")
## Superficially, there is multicollinearity (omit the intercept)
cor(model.matrix(m4literal)[ -1 , -1 ])

m4literalmc <- meanCenter(m4literal, terms = "x1")
summary(m4literalmc)

m4literalmcs <- meanCenter(m4literal, terms = "x1", standardize = TRUE)
summary(m4literalmcs)

m4 <- lm(y2 ~ poly(x1, 2, raw = TRUE) + x2 + x3, data = dat)
summary(m4)
plotCurves(m4, plotx="x1")

m4mc1 <- meanCenter(m4, terms = "x1")
summary(m4mc1)

m4mc2 <- meanCenter(m4, terms = "x1", standardize = TRUE)
summary(m4mc2)

m4mc3 <- meanCenter(m4, terms = "x1", centerDV = TRUE, standardize = TRUE)
summary(m4mc3)
}
\references{
Aiken, L. S. and West, S.G. (1991). Multiple
    Regression: Testing and Interpreting Interactions. Newbury
    Park, Calif: Sage Publications.

Cohen, J., Cohen, P., West, S. G., and Aiken, L. S. (2002). Applied
Multiple Regression/Correlation Analysis for the Behavioral Sciences
(Third.). Routledge Academic.

Echambadi, R., and Hess, J. D. (2007). Mean-Centering Does Not Alleviate
Collinearity Problems in Moderated Multiple Regression Models.
Marketing Science, 26(3), 438-445.
}
\seealso{
\code{\link[rockchalk]{standardize}}
    \code{\link[rockchalk]{residualCenter}}
}
\author{
Paul E. Johnson \email{pauljohn@ku.edu}
}
