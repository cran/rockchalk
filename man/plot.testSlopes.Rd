% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/testSlopes.R
\name{plot.testSlopes}
\alias{plot.testSlopes}
\title{Plot testSlopes objects}
\usage{
\method{plot}{testSlopes}(x, ..., shade = TRUE, col = rgb(1, 0, 0, 0.1))
}
\arguments{
\item{x}{A testSlopes object.}

\item{...}{Additional arguments that are ignored currently.}

\item{shade}{Optional. Create colored polygon for significant regions.}

\item{col}{Optional. Color of the shaded area. Default transparent pink.}
}
\value{
\code{NULL}
}
\description{
plot.testSlopes is a method for the
generic function plot. It has been revised so that it creates a plot
illustrating the marginal effect, using the
Johnson-Neyman interval calculations to highlight the
"statistically significantly different from zero" slopes.
}
\author{
\email{pauljohn@ku.edu}
}
