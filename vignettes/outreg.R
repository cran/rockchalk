### R code from vignette source '/home/pauljohn/tmp/lyxtmp/lyx_tmpdir.T22511/lyx_tmpbuf0/outreg.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: outreg.Rnw:16-17
###################################################
  if(exists(".orig.enc")) options(encoding = .orig.enc)


###################################################
### code chunk number 2: Roptions
###################################################
options(device = pdf)
options(width=160, prompt=" ", continue="  ")
options(useFancyQuotes = FALSE) 


###################################################
### code chunk number 3: outreg.Rnw:167-168
###################################################
library(rockchalk)


###################################################
### code chunk number 4: outreg.Rnw:173-186
###################################################
set.seed(1234)
x1 <- rnorm(100)
x2 <- rnorm(100)
y1 <- 5*rnorm(100) - 3*x1 + 4*x2
y2 <- rnorm(100)+5*x2
dat <- data.frame(x1, x2, y1, y2)
rm (x1, x2, y1, y2)
m1 <- lm (y1~x1, data=dat)
m2 <- lm (y1~x2, data=dat)
m3 <- lm (y1 ~ x1 + x2, data=dat)
myilogit <- function(x) exp(x)/(1 + exp(x))
y3 <- rbinom(100, size=1, p=myilogit(scale(dat$y1)))
gm1 <- glm(y3~x1 + x2, data=dat)


###################################################
### code chunk number 5: outreg10
###################################################
outreg(m1)


###################################################
### code chunk number 6: outreg.Rnw:206-207
###################################################
outreg(m1)


###################################################
### code chunk number 7: outreg20
###################################################
outreg(m1, tight=FALSE, modelLabels=c("Fingers"))


###################################################
### code chunk number 8: outreg.Rnw:229-230
###################################################
outreg(m1, tight=FALSE, modelLabels=c("Fingers"))


###################################################
### code chunk number 9: outreg30
###################################################
outreg(list(m1,m2), modelLabels=c("Mine","Yours"), varLabels = list(x1="Billie"))


###################################################
### code chunk number 10: outreg33
###################################################
outreg(list(m1,m2), tight=FALSE,  modelLabels=c("Mine","Yours"), varLabels = list(x1="Billie"))


###################################################
### code chunk number 11: outreg.Rnw:257-258
###################################################
outreg(list(m1,m2), modelLabels=c("Mine","Yours"), varLabels = list(x1="Billie"))


###################################################
### code chunk number 12: outreg.Rnw:265-266
###################################################
outreg(list(m1,m2), tight=FALSE,  modelLabels=c("Mine","Yours"), varLabels = list(x1="Billie"))


###################################################
### code chunk number 13: outreg35
###################################################
outreg(list(m1,m2,m3), modelLabels=c("A","B","C"), varLabels = list(x1="I Forgot x1", x2="He Remembered x2"))


###################################################
### code chunk number 14: outreg.Rnw:286-287
###################################################
outreg(list(m1,m2,m3), modelLabels=c("A","B","C"), varLabels = list(x1="I Forgot x1", x2="He Remembered x2"))


###################################################
### code chunk number 15: outreg.Rnw:296-297
###################################################
outreg(list(m1,m2,m3), tight=F, modelLabels=c("I Love love love really long titles","Hate Long","Medium"))


###################################################
### code chunk number 16: outreg70
###################################################
outreg(list(m1,gm1),modelLabels=c("OLS:y1","GLM: Categorized y1"))


###################################################
### code chunk number 17: outreg.Rnw:329-330
###################################################
outreg(list(m1,gm1),modelLabels=c("OLS:y1","GLM: Categorized y1"))


###################################################
### code chunk number 18: outreg.Rnw:354-356
###################################################
dat$y5 <- with(dat, -3*x1 + 0.5*log(x2^2) + 1.1*x2 + 2.2 *x1 * x2 + 3*rnorm(100)) 
m5 <- lm (y5 ~ log(x2*x2) + x1 * x2, data=dat)


###################################################
### code chunk number 19: pcps10
###################################################
plotPlane(m5, plotx1="x1", plotx2="x2")


###################################################
### code chunk number 20: outreg.Rnw:370-371
###################################################
outreg(m5, tight=FALSE)


###################################################
### code chunk number 21: pcps20
###################################################
plotCurves(m5, plotx="x1", modx="x2")


###################################################
### code chunk number 22: ps10
###################################################
par(mfcol=c(2,1))
m3psa <- plotSlopes(m3, plotx="x1", modx="x2", xlab="x1 is a Continuous Predictor")
m3psb <- plotSlopes(m3, plotx="x1", modx="x2", modxVals=c(0.2, 0.5, 0.7), xlab="Continuous Predictor")
par(mfcol=c(1,1))


###################################################
### code chunk number 23: outreg.Rnw:443-444
###################################################
testSlopes(m3psa)


###################################################
### code chunk number 24: ps15
###################################################
dat$y4 <- with(dat, -3*x1 + 6*x2 - 0.17*x1*x2 + 5*rnorm(100))
m4 <- lm (y4 ~ x1 * x2, data=dat)


###################################################
### code chunk number 25: ps20
###################################################
m4ps <- plotSlopes(m4, plotx="x1", modx="x2", xlab="Continuous Predictor")


###################################################
### code chunk number 26: ts10
###################################################
testSlopes(m4ps)


###################################################
### code chunk number 27: pp100
###################################################
p100 <- plotPlane(m4, plotx1="x1", plotx2="x2")


###################################################
### code chunk number 28: pp110
###################################################
m4ps <- plotSlopes(m4, plotx="x1", modx="x2", xlab="Continuous Predictor", ylim=c(-25, 35))


###################################################
### code chunk number 29: pp111
###################################################
p110 <- plotPlane(m4, plotx1="x1", plotx2="x2", x1lab="Continuous Predictor")
for(j in unique(m4ps$newdata$x2)){
subdat <- m4ps$newdata[m4ps$newdata$x2==j,]
lines(trans3d(subdat$x1, subdat$x2, subdat$pred, pmat=p110$res), col="red", lwd=3)
}


###################################################
### code chunk number 30: outreg.Rnw:587-589
###################################################
m4 <- lm (y4 ~ x1 * x2, data=dat)
m4s <- standardize(m4)


###################################################
### code chunk number 31: outreg.Rnw:597-598
###################################################
summary(m4s)


###################################################
### code chunk number 32: stdreg10
###################################################
outreg(list(m4,m4s), tight=F, modelLabels=c("Not Standardized","Standardized"))


###################################################
### code chunk number 33: outreg.Rnw:729-733
###################################################
dat2 <- genCorrelatedData(N=400, rho=.4, stde=300, beta=c(2,0.1,0.1,0.2))
m4linear <- lm (y ~ x1 + x2, data=dat2)
m4int <- lm (y ~ x1 * x2, data=dat2)
m4mc <- meanCenter(m4int)


###################################################
### code chunk number 34: mcenter10
###################################################
outreg(list(m4linear, m4int,m4mc), tight=F, modelLabels=c("Linear", "Not Centered","Mean Centered"))


###################################################
### code chunk number 35: mcenter50
###################################################
op <- par()
par(mfcol=c(1,2))
par(mar=c(2,2,2,1))
plotPlane(m4int, plotx1="x1", plotx2="x2", plotPoints=FALSE, main="Not Centered", ticktype="detailed")
plotPlane(m4mc, plotx1="x1", plotx2="x2", plotPoints=FALSE, main="Mean Centered", ticktype="detailed")
par(op)


###################################################
### code chunk number 36: outreg.Rnw:794-805
###################################################
x <- rnorm(100, m=50, s=20)
y <- 3 + 0.2 * x + 15 * rnorm(100)
plot(x,y)
mp1 <- lm(y ~ x)
abline(mp1)
ndf <- data.frame(x=plotSeq(x,40))
p1 <- as.data.frame(predict(mp1, interval="conf", newdata = ndf))
lines(ndf$x, p1$fit, col="black", lwd=2)
lines(ndf$x, p1$lwr, col="red", lwd=2, lty=4)
lines(ndf$x, p1$upr, col="red", lwd=2, lty=3)
legend("topleft", legend=c("predicted","conf. lower", "conf. upper"), col=c("black","red","red"), lty=c(1,4,3))


###################################################
### code chunk number 37: outreg.Rnw:897-902
###################################################
dat2 <- genCorrelatedData(N=400, rho=.4, stde=300, beta=c(2,0.1,0.1,0.8))
m4linear <- lm (y ~ x1 + x2, data=dat2)
m4int <- lm (y ~ x1 * x2, data=dat2)
m4mc <- meanCenter(m4int)
m4rc <- residualCenter(m4int)


###################################################
### code chunk number 38: rcenter10
###################################################
outreg(list(m4linear, m4int, m4mc, m4rc), tight=T, modelLabels=c("Linear", "Not Centered","Mean Centered", "Resid Centered"))


###################################################
### code chunk number 39: rcenter30
###################################################
op <- par()
par(mfrow=c(2,2))
par(mar=c(2,2,2,1))
plotPlane(m4int, plotx1="x1", plotx2="x2", plotPoints=TRUE, main="Not Centered", ticktype="detailed", theta=-20)
plotPlane(m4mc, plotx1="x1", plotx2="x2", plotPoints=TRUE, main="Mean Centered", ticktype="detailed", theta=-20)
plotPlane(m4rc, plotx1="x1", plotx2="x2", plotPoints=TRUE, main="Residual Centered", ticktype="detailed", theta=-20)
par(op)


###################################################
### code chunk number 40: rcenter40
###################################################
m4mcpred <- predict(m4mc, newdata=dat2)
m4rcpred <- predict(m4rc, newdata=dat2)
plot(m4mcpred, m4rcpred, main="", xlab="Predictions of Mean-centered Regression",ylab="Predictions from Residual-centered Regression")
predcor <- round(cor(m4mcpred, m4rcpred),3)
legend("topleft", legend=c(paste("Correlation=", predcor)))


