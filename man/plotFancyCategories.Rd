% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotSlopes.R
\name{plotFancyCategories}
\alias{plotFancyCategories}
\title{Draw display for discrete predictor in plotSlopes}
\usage{
plotFancyCategories(
  newdf,
  olddf,
  plotx,
  modx = NULL,
  modxVals,
  xlab,
  xlim,
  ylab,
  ylim,
  col = c("black", "blue", "darkgreen", "red", "orange", "purple", "green3"),
  opacity = 120,
  main,
  space = c(0, 1),
  width = 0.2,
  llwd = 1,
  offset = 0,
  ...,
  gridArgs = list(lwd = 0.3, lty = 5),
  legendArgs
)
}
\arguments{
\item{newdf}{The new data object, possibly from predictOMatic}

\item{olddf}{The model data matrix}

\item{plotx}{Name of horizontal axis variable}

\item{modx}{Name of moderator}

\item{modxVals}{values for modx}

\item{xlab}{X axis label}

\item{xlim}{x axis limits. Don't bother setting this, the internal
numbering is too complicated.}

\item{ylab}{y axis label}

\item{ylim}{y axis limits}

\item{col}{color pallet for values of moderator variable}

\item{opacity}{Value in 0, 255 for darkness of interval shading}

\item{main}{main title}

\item{space}{same as space in barplot, vector c(0, 1) is
c(space_between, space_before_first)}

\item{width}{width of shaded bar area, default is 0.2. Maximum is 1.}

\item{llwd}{requested line width, will re-cycle.}

\item{offset}{Shifts display to right (not tested)}

\item{...}{Arguments sent to par}

\item{gridArgs}{A list of values to control printing of reference grid.
Set as "none" if no grid is desired.}

\item{legendArgs}{Arguments to the legend function. Set as "none"
if no legend is needed. Otherwise, provide a list}
}
\value{
None
}
\description{
There's plotFancy for numeric predictor. This is for discrete
}
\author{
Paul Johnson \email{pauljohn@ku.edu}
}
