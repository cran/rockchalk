\name{summarizeNumerics}
\alias{summarizeNumerics}
\title{Extracts numeric variables and presents an alphabetized summary in
a workable format.}
\usage{
  summarizeNumerics(dat, alphaSort = TRUE,
    digits = max(3, getOption("digits") - 3))
}
\arguments{
  \item{dat}{a data frame or a matrix}

  \item{alphaSort}{If TRUE (default), the columns are
  re-organized in alphabetical order. If FALSE, they are
  presented in the original order.}

  \item{digits}{integer, used for number formatting
  output.}
}
\value{
  a matrix with one column per variable and the rows
  representing the quantiles as well as the mean, standard
  deviation, and variance.
}
\description{
  This function finds the numeric variables and ignores the
  others. (See \code{summarizeFactors} for a function that
  handles non-numeric variables.). It calculates the
  quantiles for each variable, as well as the mean,
  standard deviation, and variance, and then packs those
  results into a matrix. The main benefits from this
  compared to R's default summary are 1) more summary
  information is returned for each variable (dispersion),
  2) the results are returned in a matrix that is easy to
  use in further analysis, 3) the columns in the output are
  alphabetized. To prevent alphabetization, use alphaSort =
  FALSE.
}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}
\seealso{
  summarize and summarizeFactors
}

