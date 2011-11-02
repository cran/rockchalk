\docType{data}
\name{religioncrime}
\alias{religioncrime}
\title{Religious beliefs and crime rates}
\format{data.frame: 51 obs. of 3 variables}
\source{
  Anonymous researcher who claims the data is real.
}
\usage{
  data(religioncrime)
}
\description{
  The data national-level summary indicators of public
  opinion about the existence of heaven and hell as well as
  the national rate of violent crime.
}
\examples{
require(rockchalk)
data(religioncrime)
mod1 <- lm(crime ~ heaven, data=religioncrime)
mod2 <- lm(crime ~ hell, data=religioncrime)
mod3 <- lm(crime ~ heaven + hell, data=religioncrime)
with(religioncrime,
mcGraph1(heaven, hell, crime)
)
with(religioncrime,
mcGraph2(heaven, hell, crime)
)
mod1 <- with(religioncrime,
mcGraph3(heaven, hell, crime)
)
summary(mod1[[1]])
##TODO: Draw more with perspective matrix mod1[[2]]
}
\author{
  Paul E. Johnson \email{pauljohn@ku.edu} and Anonymous
}
\keyword{datasets}

