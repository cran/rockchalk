\name{cutBySD}
\alias{cutBySD}
\title{Returns center values of x, the mean, mean-std.dev, mean+std.dev}
\usage{
  cutBySD(x, n = 3)
}
\arguments{
  \item{x}{A numeric variable}

  \item{n}{Should be either 3 or 5. If 3, values that
  divide the data at c(m-sd, m, m+sd) are returned. If 5,
  the returned values are c(m-2sd, m-sd, m, m+sd, m+2sd).
  Other values of n will be rounded to 3 or 5.}
}
\value{
  A named vector
}
\description{
  If the numeric variable has fewer than 6 unique observed
  values, this will send the data to cutByTable.
}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}

