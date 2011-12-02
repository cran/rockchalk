\name{lmrc}
\alias{lmrc}
\title{Calculates a "residual-centered" interaction regression.}
\usage{
  lmrc(model)
}
\arguments{
  \item{model}{A fitted lm object}
}
\value{
  a list containing rcReg=the residual centered regression
  that is requested centeringRegressions= a list including
  each of the intermediate regressions that was calculated
  in order to create the residual centered interaction
  terms. These latter objects may be necessary for
  diagnostics and to calculate predicted values for
  hypothetical values of the inputs. If there are no
  interactive terms, then NULL is returned.
}
\description{
  Given a fitted lm, this function scans for coefficients
  estimated from "interaction terms" by checking for colon
  symbols. The function then calculates the "residual
  centered" estimate of the interaction term and replaces
  the interaction term with that residual centered
  estimate. It works for any order of interaction, unlike
  other implementations of the same approach. See also
  function lmres in package pequod.
}
\examples{
set.seed(123)
x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- rnorm(100)
x4 <- rnorm(100)
y <- rnorm(100)
dat <- data.frame(y, x1,x2,x3,x4)
rm(x1,x2,x3,x4,y)
m1 <- lm(y~ x1*x2*x3 + x4, data=dat)
summary(m1)

lmRCm1 <- lmrc(m1)
lmRCm1s <- summary(lmRCm1[["rcReg"]])
coef(lmRCm1[["rcReg"]])

### lmrc as good as pequod's lmres
### not run because pequod generates R warnings
###
### if (require(pequod)){
###  pequodm1 <- lmres(y ~ x1*x2*x3 + x4, data=dat)
###  pequodm1s <- summary(pequodm1)
###  coef(pequodm1s)
### }

### lmrc works with any number of interactions. See:

m2 <- lm(y~ x1*x2*x3*x4, data=dat)
lmRCm2 <- lmrc(m2)
summary(lmRCm2[["rcReg"]])


### If you want to fit a sequence of models, as in pequod, can do.

tm <-terms(m2)
tmvec <- attr(terms(m2), "term.labels")
f1 <- tmvec[grep(":", tmvec, invert = TRUE)]
f2 <- tmvec[grep(":.*:", tmvec, invert = TRUE)]
f3 <- tmvec[grep(":.*:.*:", tmvec, invert = TRUE)]

## > f1
## [1] "x1" "x2" "x3" "x4"
## > f2
## [1] "x1"    "x2"    "x3"    "x4"    "x1:x2" "x1:x3" "x2:x3"
## > f3
## [1] "x1"       "x2"       "x3"       "x4"       "x1:x2"    "x1:x3"    "x2:x3"
## [8] "x1:x2:x3"

lmf1 <- lm(paste("y","~", paste(f1, collapse=" + ")), data=dat)
lmRCf1 <- lmrc(lmf1)
summary(lmRCf1[["rcReg"]])

lmf2 <- lm(paste("y","~", paste(f2, collapse=" + ")), data=dat)
lmRCf2 <- lmrc(lmf2)
summary(lmRCf2[["rcReg"]])

lmf3 <- lm(paste("y","~", paste(f3, collapse=" + ")), data=dat)
lmRCf3 <- lmrc(lmf3)
summary(lmRCf3[["rcReg"]])
}
\author{
  Paul E. Johnson \email{<pauljohn@ku.edu>}
}
\references{
  Little, T. D., Bovaird, J. A., & Widaman, K. F. (2006).
  On the Merits of Orthogonalizing Powered and Product
  Terms: Implications for Modeling Interactions Among
  Latent Variables. Structural Equation Modeling, 13(4),
  497-519.
}

