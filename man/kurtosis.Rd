% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize.R
\name{kurtosis}
\alias{kurtosis}
\title{Calculate excess kurtosis}
\usage{
kurtosis(x, na.rm = TRUE, excess = TRUE, unbiased = TRUE)
}
\arguments{
\item{x}{A numeric variable (vector)}

\item{na.rm}{default TRUE. If na.rm = FALSE and there are missing
values, the mean and variance are undefined and this function
returns NA.}

\item{excess}{default TRUE. If true, function returns excess
kurtosis (kurtosis -3). If false, the return is simply
kurtosis as defined above.}

\item{unbiased}{default TRUE. Should the denominator of the
variance estimate be divided by N-1, rather than N?}
}
\value{
A scalar value or NA
}
\description{
Kurtosis is a summary of a distribution's shape, using the Normal
distribution as a comparison. A distribution with high kurtosis is
said to be leptokurtic. It has wider, "fatter" tails and a
"sharper", more "peaked" center than a Normal distribution. In a
standard Normal distribution, the kurtosis is 3.  The term
"excess kurtosis" refers to the difference \eqn{kurtosis - 3}.
Many researchers use the term kurtosis to refer to
"excess kurtosis" and this function follows suit.  The user may
set excess = FALSE, in which case the uncentered kurtosis is
returned.
}
\details{
If kurtosis is smaller than 3 (or excess kurtosis is negative),
the tails are "thinner" than the normal distribution (there is
lower chance of extreme deviations around the mean). If kurtosis
is greater than 3 (excess kurtosis positive), then the tails are
fatter (observations can be spread more widely than in the Normal
distribution).

The kurtosis may be calculated with the small-sample
bias-corrected estimate of the variance. Set unbiased = FALSE if
this is not desired.  It appears somewhat controversial whether
this is necessary. According to the
US NIST,
\url{http://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm},
kurtosis is defined as
                  
\deqn{kurtosis =  ( mean((x - mean(x))^4) )/ var(x)^2}
            
where \eqn{var(x)} is calculated with the denominator \eqn{N},
rather than \eqn{N-1}.

A distribution is said to be leptokurtic if it is tightly bunched
in the center (spiked) and there are long tails. The long tails
reflect the probability of extreme values.
}
\author{
Paul Johnson \email{pauljohn@ku.edu}
}
