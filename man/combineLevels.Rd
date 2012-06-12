\name{combineLevels}
\alias{combineLevels}
\title{recode a factor by "combining" levels}
\usage{
  combineLevels(fac, levs, newLabel)
}
\arguments{
  \item{fac}{An R factor variable, either ordered or not.}

  \item{levs}{The levels to be combined. Users may specify
  either a numerical vector of level values, such as
  c(1,2,3), to combine the first three elements of
  level(fac), or they may specify level names.  This can be
  done as a character vector of *correctly spelled* factor
  values, such as c("Yes","Maybe","Always") or it may be
  provided as a subset of the output from levels, such as
  levels(fac)[1:3].}

  \item{newLabel}{A character string that represents the
  label of the new level to be created when \code{levs}
  values are combined.}
}
\value{
  A new factor variable, with unused levels removed.
}
\description{
  If a factor variable is currently coded with levels
  c("Male","Female","Man", "M"), and the user needs to
  combine the redundant levels for males, this is the
  function to use!
}
\details{
  If the factor is an ordinal factor, then levels may be
  combined only if they are adjacent. That is to say, a
  factor with levels c("Lo","Med","Hi","Extreme") allows us
  to combine responses "Lo" and "Med", while it will NOT
  allow us to combine "Lo" with "Hi".

  A non-ordered factor can be reorganized to combine any
  values, no matter what positions they occupy in the
  levels vector.
}
\examples{
x <- c("M","A","B","C","A","B","A","M")
x <- factor(x)
levels(x)
x2a <- combineLevels(x, levs = c("M","A"), newLabel = "M_or_A")
addmargins(table(x2a, x, exclude=NULL))
x2b <- combineLevels(x, c(1,4), "M_or_A")
addmargins(table(x2b, x, exclude=NULL))
x3 <- combineLevels(x, levs = c("M","A","C"), newLabel = "MAC")
addmargins(table(x3, x, exclude=NULL))
## Now an ordinal factor
z <- c("M","A","B","C","A","B","A","M")
z <- ordered(z)
levels(z)
table(z, exclude=NULL)
z2a <-  combineLevels(z, levs = c(1,2), "Good")
addmargins(table(z2a, z, exclude = NULL))
z2b <- combineLevels(z, levs = c("A","B"), "AorB")
addmargins(table(z2b, z, exclude = NULL))
## Should fail:
## z2 <- combineLevels(z, levs=c("A","C"), "Whoops!")
}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}
