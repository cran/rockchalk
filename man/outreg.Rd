\name{outreg}
\alias{outreg}
\title{outreg}
\usage{
  outreg(incoming, title = "My Regression", label = "",
  modelLabels = NULL, varLabels = NULL, tight = TRUE,
  showAIC = FALSE, lyx = TRUE)
}
\arguments{
  \item{incoming}{A single regression model or an R list of
  regression models.}

  \item{title}{A title to be displayed on the top of the
  LaTeX regression table.}

  \item{label}{A string to be used as a LaTeX label in the
  table to be created.}

  \item{modelLabels}{If "incoming" is a list of fitted
  models, this can supply strings to separately label each
  one in the output table.}

  \item{varLabels}{To beautify the words associated with
  the fitted variables, use this argument.}

  \item{tight}{If T, parameter estimates and standard
  errors are printed in a single column, thus allowing
  several models to be displayed side by side. If F,
  parameter estimates and standard errors are printed side
  by side.}

  \item{showAIC}{if TRUE, the AIC estimate is included with
  the diagnostic values}

  \item{lyx}{If the LaTeX output is going into a LaTeX
  table that already exists, as, for example, it might be
  in LyX when the Sweaved code is inside a floating table
  object, set this to TRUE. Otherwise, the value of FALSE
  causes the output to include the output that creates more
  of the LaTeX table boilerplate.}
}
\value{
  None
}
\description{
  Creates a publication quality regression result table for
  models fitted by lm and glm. Can be called within Sweave
  documents.
}
\details{
  Uses a bunch of tedious "cat" statements to display the
  regression model in LaTeX output.
}
\note{
  There are many R packages that can be used to create
  LaTeX regression tables. memisc, apsrtable, xtables, and
  rms are some. This "outreg" version was in use in our
  labs before we were aware that those packages were in
  development. It is not intended as a competitor, it is
  just a slightly different version of the same that is
  more suited to our needs.
}
\examples{
x1 <- rnorm(100)
x2 <- rnorm(100)
y1 <- 5*rnorm(100)+3*x1 + 4*x2

y2 <- rnorm(100)+5*x2
m1 <- lm (y1~x1)
m2 <- lm (y1~x2)
m3 <- lm (y1 ~ x1 + x2)
gm1 <- glm(y1~x1)

outreg(m1,title="My One Tightly Printed Regression", lyx=FALSE )

outreg(m1,tight=FALSE,modelLabels=c("Fingers"), title="My Only Spread Out Regressions" ,lyx=FALSE)

outreg(list(m1,m2),modelLabels=c("Mine","Yours"),varLabels=list(x1="Billie"), title="My Two Linear Regressions Tightly Printed" ,lyx=FALSE)

outreg(list(m1,m2),modelLabels=c("Whatever","Whichever"), title="My Two Linear Regressions Not Tightly  Printed", showAIC=FALSE, lyx=FALSE)

outreg(list(m1,m2,m3),title="My Three Linear Regressions", lyx=FALSE)

outreg(list(m1,m2,m3),tight=FALSE,modelLabels=c("I Love love love really long titles","Hate Long","Medium"), lyx=FALSE)

outreg(list(gm1),modelLabels=c("GLM"), lyx=FALSE)

outreg(list(m1,gm1),modelLabels=c("OLS","GLM"), lyx=FALSE)
}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}
\references{
  Citation:
}
\keyword{regression}

