\name{vech2Corr}
\alias{vech2Corr}
\title{Convert the vech (column of strictly lower trianglar values from a matrix) into a correlation matrix.}
\usage{
  vech2Corr(vech)
}
\arguments{
  \item{vech}{A vector of values for the strictly lower
  triangle of a matrix. All values must be in the [0,1]
  interval (because they are correlations) and the matrix
  formed must be positive definite.}
}
\value{
  A symmetric correlation matrix, with 1's on the diagonal.
}
\description{
  vech2Corr is a convenience function for creating
  correlation matrices from a vector of the lower
  triangular values. It checks the arguments to make sure
  they are consistent with the requirements of a
  correlation matrix. All values must be in [-1, 1], and
  the number of values specified must be correct for a
  lower triangle.
}
\details{
  Use this in combination with the \code{lazyCov} function
  to convert a vector of standard deviations and the
  correlation matrix into a covariance matrix.
}
\examples{
v <- c(0.1, 0.4, -0.5)
vech2Corr(v)
v <- c(0.1, 0.4, -0.4, 0.4, 0.5, 0.1)
vech2Corr(v)
}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}
\seealso{
  Similar functions exist in many packages, see
  \code{vec2sm} in corpcor, \code{xpnd} in MCMCpack
}

