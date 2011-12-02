##' Generates a data frame (x1,x2,y) with user-specified correlation between x1 and x2
##' 
##' @param N Number of cases desired
##' @param means 2-vector of means for x1 and x2
##' @param sds 2-vector of standard deviations for x1 and x2
##' @param rho Correlation coefficient for x1 and x2
##' @param stde standard deviation of the error term in the data generating equation
##' @param beta beta vector of at most 4 coefficients for intercept, slopes, and interaction
##' @import MASS
##' @export genCorrelatedData
genCorrelatedData <- function(N = 100, means = c(50,50), sds = c(10,10), rho = 0.0, stde = 1, beta = c(0, 0.2, 0.2, 0.0)){ 
  require(MASS)
  if (length(beta)> 4) stop("beta vector can have at most 4 values")
  corr.mat <- matrix(c(1,rho,rho,1), nrow = 2)
  sigma <- diag(sds) %*% corr.mat %*% diag(sds)
  x.mat <-  mvrnorm(n = N, mu = means, Sigma = sigma)
  y = beta[1] + beta[2] * x.mat[,1] + beta[3] * x.mat[,2] + beta[4] * x.mat[,1] * x.mat[ ,2] +  stde*rnorm (N, m = 0, s = 1)
  dat <- data.frame(x.mat, y)
  names(dat) <- c("x1", "x2", "y")
  dat
}
  