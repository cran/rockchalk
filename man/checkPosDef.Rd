\name{checkPosDef}
\alias{checkPosDef}
\title{Check a matrix for positive definitness}
\usage{
  checkPosDef(X, tol = 1e-06)
}
\arguments{
  \item{X}{A matrix}

  \item{tol}{Tolerance (closeness to 0 required to declare
  failure)}
}
\value{
  TRUE or FALSE
}
\description{
  Uses eigen to check positive definiteness. Follows
  example used in \code{MASS} package by W. N. Venables and
  Brian D. Ripley
}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}

