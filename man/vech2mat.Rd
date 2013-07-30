\name{vech2mat}
\alias{vech2mat}
\title{Convert a half-vector (vech) into a matrix.}
\usage{
  vech2mat(vech, diag = NULL)
}
\arguments{
  \item{vech}{A vector}

  \item{diag}{Optional. If supplied, We assume vech is a
  strictly lower triangluar vech, it does not include
  diagonal values. diag can be either a single value (to
  replace all elements along the diagonal) or a vector of
  the correct length to replace the diagonal.}
}
\description{
  This may treat the input vech as the fill material for a
  strictly lower triangle (no diagonal) or it may include
  the diagonal values, depending on the value of the diag
  argument. If diag = NULL (default), this will treat vech
  as if it includes values for the diagonal. If diag is
  either a number or a vector, this will treat vech as
  strictly lower triangular and fill the diagonal with the
  value provided by diag.
}
\examples{
x <- 1:6
vech2mat(x)
vech2mat(x, diag = 7)
vech2mat(x, diag = c(99, 98, 97, 96))
}
\seealso{
  Similar functions exist in many packages, see
  \code{vec2sm} in corpcor, \code{xpnd} in MCMCpack
}

