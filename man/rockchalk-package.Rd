\docType{package}
\name{rockchalk-package}
\alias{rockchalk}
\alias{rockchalk-package}
\title{Miscellaneous regression functions}
\description{
  \tabular{ll}{ Package: \tab rockchalk\cr Type: \tab
  Package\cr Version: \tab 1.4\cr Date: \tab 2012-01-10\cr
  License: \tab GPL >= 3\cr LazyLoad: \tab yes\cr } The
  rockchalk package includes an ever-growing collection of
  functions that assist in the presentation of regression
  models.  The initial function was \code{\link{outreg}},
  which produces LaTeX tables that summarize one or many
  fitted regression models.  It also offers plotting
  conveniences like \code{\link{plotPlane}} and
  \code{\link{plotSlopes}}, which illustrate some of the
  variables from a fitted regression model. For a detailed
  check on multicollinearity, see \code{\link{mcDiagnose}}.
  The user should be aware of this fact: Not all of these
  functions lead to models or types of analysis that we
  endorese.  Rather, they all lead to analysis that is
  endorsed by some scholars, and we feel it is important to
  facilitate the comparison of competing methods.  For
  example, the function \code{\link{standardize}} will
  calculate standardized regression coefficients for all
  predictors in a regression model's design matrix in order
  to replicate results from other statistical frameworks,
  no matter how unwise the use of such coefficients might
  be. The function \code{\link{meanCenter}} will allow the
  user to more selectively choose variables for centering
  (and possibly standardization) before they are entered
  into the design matrix.  Because of the importance of
  interaction variables in regression analysis, the
  \code{\link{residualCenter}} and \code{\link{meanCenter}}
  functions are offered.  While mean centering does not
  actually help with multicollinearity of interactive
  terms, many scholars have argued that it does.  The
  meanCenter function can be compared with the "residual
  centering" of interaction terms.
}
\author{
  Paul E. Johnson \email{pauljohn@ku.edu}

  Maintainer: Paul Johnson \email{<pauljohn@ku.edu>}
}
\references{
  http://pj.freefaculty.org/R
}
\keyword{hplot}
\keyword{regression}

