\name{plotSeq}
\alias{plotSeq}
\title{Create sequences for plotting}
\usage{
  plotSeq(x, length.out = length(x))
}
\arguments{
  \item{x}{an R vector variable}

  \item{length.out}{the number of elements in the desired
  plotting sequence.}
}
\description{
  \code{plotSeq} is a convenience for the creation of
  sequence across the range of a variable. By default, the
  length of the plotting sequence will be equal to the
  length of the original sequence. In that case, the only
  effect is to create an evenly-spaced set of values. If
  \code{length.out} is specified, the user determines the
  number of elements in plotSeq.
}
\details{
  The primary intended usage is for the creation of
  plotting sequences of numeric variables.  It takes a
  variable's range and the fills in evenly spaced steps. If
  x is a factor variable, the levels will be returned. Uses
  of this functionality are planned in the future.
}
\examples{
#Create a quadratic regression

stde <- 14
x <- rnorm(100, m = 50, s = 10)
y <- 0.2 - 02*x + 0.2*x^2 + stde*rnorm(100)
mod1 <- lm (y ~ poly(x, 2))

plot(x, y, main="The Quadratic Regression")
seqx <- plotSeq(x, length.out = 10)
seqy <- predict(mod1, newdata = data.frame(x = seqx))
lines(seqx, seqy, col = "red")

# Notice the bad result when a plotting sequence is
# not used.
plot(x, y, main = "Bad Plot Result")
seqy <- predict(mod1)
lines(x, seqy, col = "green")
}
\seealso{
  \code{pretty}
}

