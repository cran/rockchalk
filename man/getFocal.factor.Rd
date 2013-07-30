\name{getFocal.factor}
\alias{getFocal.factor}
\title{Select focal values from factor variables.}
\usage{
  \method{getFocal}{factor} (x, xvals = NULL, n = 3, ...)
}
\arguments{
  \item{x}{Required. A factor variable}

  \item{n}{Number of values to be selected.}

  \item{xvals}{Optional. With factor variables, this
  argument is generally not used because the only
  implemented divider algorithm is "table" (see
  \code{cutByTable}), which selects the \code{n} most
  frequently observed values. That is the default
  algorithm. It is legal to specify xvals = "table", but
  there is no point in doing that. However, xvals may take
  two other formats. It may be a user-specified function
  that can to select levels values from \code{x} or it may
  be a vector of labels (or, names of levels). The purpose
  of the latter is to check that the requested levels are
  actually present in the supplied data vector \code{x}. If
  the levels specified are not in the observed variable,
  then this function stops with an error message.}

  \item{...}{Additional arguments passed to user-specified
  function in \code{xvals}.}
}
\value{
  A named vector of level names (labels).
}
\description{
  This is used by predictOMatic and plotSlopes to choose
  values of a categorical moderator variable.
}
\examples{
x <- factor(c("A","B","A","B","C","D","D","D"))
getFocal(x)
getFocal(x, n = 2)
}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}

