### R code from vignette source '/home/pauljohn/tmp/lyxtmp/lyx_tmpdir.T10955/lyx_tmpbuf0/rockchalk.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: rockchalk.Rnw:15-16
###################################################
  if(exists(".orig.enc")) options(encoding = .orig.enc)


###################################################
### code chunk number 2: Roptions
###################################################
options(device = pdf)
options(width=80, prompt=" ", continue="  ")
options(useFancyQuotes = FALSE) 


###################################################
### code chunk number 3: rockchalk.Rnw:169-170
###################################################
library(rockchalk)


###################################################
### code chunk number 4: rockchalk.Rnw:189-191
###################################################
data(Chile)
(summChile <- summarize(Chile))


###################################################
### code chunk number 5: rockchalk.Rnw:198-199
###################################################
centralValues(Chile)


###################################################
### code chunk number 6: rockchalk.Rnw:210-212
###################################################
m1 <- lm(statusquo ~ age + income + population + region + sex, data=Chile)
m1mf <- model.frame(m1)


###################################################
### code chunk number 7: rockchalk.Rnw:251-253
###################################################
m1mfsumm <- summarize(m1mf)
m1cv <- centralValues(m1mf)


###################################################
### code chunk number 8: rockchalk.Rnw:260-262
###################################################
mixAndMatch <- expand.grid(age = summChile$numerics[2:4, "age"], region = c("C","N"))
mixAndMatch


###################################################
### code chunk number 9: rockchalk.Rnw:272-274
###################################################
mynewdf <- cbind(mixAndMatch, m1cv[  ,!colnames(m1cv) %in% colnames(mixAndMatch)])
mynewdf


###################################################
### code chunk number 10: rockchalk.Rnw:279-281
###################################################
mynewdf$fit <- predict(m1, newdata = mynewdf)
mynewdf


###################################################
### code chunk number 11: rockchalk.Rnw:287-291
###################################################
mynewdf <- cbind(mixAndMatch, m1cv[  ,!colnames(m1cv) %in% colnames(mixAndMatch)])
preds <- predict(m1, newdata = mynewdf, interval="confidence")
mynewdf <- cbind(mynewdf, preds)
mynewdf


###################################################
### code chunk number 12: createdata
###################################################
set.seed(1234)
x1 <- rnorm(100)
x2 <- rnorm(100, m=10)
x3 <- rnorm(100)
y1 <- 5*rnorm(100) - 3*x1 + 4*x2
y2 <- rnorm(100)+5*x2
dat <- data.frame(x1, x2, x3, y1, y2)
rm (x1, x2, y1, y2)
m1 <- lm (y1~x1, data=dat)
m2 <- lm (y1~x2, data=dat)
m3 <- lm (y1 ~ x1 + x2, data=dat)
myilogit <- function(x) exp(x)/(1 + exp(x))
dat$y3 <- rbinom(100, size=1, p=myilogit(scale(dat$y1)))
gm1 <- glm(y3~x1 + x2, data=dat)
dat$y4 <- 1 + 0.1 * dat$x1 - 6.9 * dat$x2 + 0.5 * dat$x1*dat$x2 + 0.2 * dat$x3 + rnorm(100,0, sd=10)


###################################################
### code chunk number 13: outreg10
###################################################
outreg(m1)


###################################################
### code chunk number 14: rockchalk.Rnw:345-346
###################################################
outreg(m1)


###################################################
### code chunk number 15: outreg20
###################################################
outreg(m1, tight=FALSE, modelLabels=c("Fingers"))


###################################################
### code chunk number 16: rockchalk.Rnw:368-369
###################################################
outreg(m1, tight=FALSE, modelLabels=c("Fingers"))


###################################################
### code chunk number 17: outreg30
###################################################
outreg(list(m1,m2), modelLabels=c("Mine","Yours"), varLabels = list(x1="Billie"))


###################################################
### code chunk number 18: outreg33
###################################################
outreg(list(m1,m2), tight=FALSE,  modelLabels=c("Mine","Yours"), varLabels = list(x1="Billie"))


###################################################
### code chunk number 19: rockchalk.Rnw:396-397
###################################################
outreg(list(m1,m2), modelLabels=c("Mine","Yours"), varLabels = list(x1="Billie"))


###################################################
### code chunk number 20: rockchalk.Rnw:404-405
###################################################
outreg(list(m1,m2), tight=FALSE,  modelLabels=c("Mine","Yours"), varLabels = list(x1="Billie"))


###################################################
### code chunk number 21: outreg35
###################################################
outreg(list(m1,m2,m3), modelLabels=c("A","B","C"), varLabels = list(x1="I Forgot x1", x2="He Remembered x2"))


###################################################
### code chunk number 22: rockchalk.Rnw:425-426
###################################################
outreg(list(m1,m2,m3), modelLabels=c("A","B","C"), varLabels = list(x1="I Forgot x1", x2="He Remembered x2"))


###################################################
### code chunk number 23: rockchalk.Rnw:435-436
###################################################
outreg(list(m1,m2,m3), tight=F, modelLabels=c("I Love love love really long titles","Hate Long","Medium"))


###################################################
### code chunk number 24: outreg70
###################################################
outreg(list(m1,gm1),modelLabels=c("OLS:y1","GLM: Categorized y1"))


###################################################
### code chunk number 25: rockchalk.Rnw:467-468
###################################################
outreg(list(m1,gm1),modelLabels=c("OLS:y1","GLM: Categorized y1"))


###################################################
### code chunk number 26: ps10
###################################################
m4 <- lm (y4 ~ x1*x2 + x3, data=dat)
par(mfcol=c(2,1))
m4psa <- plotSlopes(m4, plotx = "x1", modx = "x2", xlab = "x1 is a Continuous Predictor", ylim=magRange(dat$y4, c(1,1.3)), xlim=magRange(dat$x1, c(1.2,1)))
m4psb <- plotSlopes(m4, plotx = "x1", modx = "x2", modxVals=c(8, 10.5, 12), xlab="Continuous Predictor", ylim=magRange(dat$y4, c(1,1.3)), xlim=magRange(dat$x1, c(1.2,1)))
par(mfcol=c(1,1))


###################################################
### code chunk number 27: ps20
###################################################
fourCat <- gl(4,25, labels=c("East","West","South", "Midwest"))
dat$x4 <- sample(fourCat, 100, replace=TRUE)
dat$y5 <- 1 + 0.1 * dat$x1 + contrasts(dat$x4)[dat$x4, ] %*% c(-1,1,2) + rnorm(100,0, sd=10)
m5 <- lm (y5 ~ x1*x4 + x3, data=dat)
m5psa <- plotSlopes(m5, plotx = "x1", modx = "x4", xlab = "x1 is a Continuous Predictor", xlim=magRange(dat$x1, c(1.2,1)))


###################################################
### code chunk number 28: ts10
###################################################
testSlopes(m4psa)


###################################################
### code chunk number 29: rockchalk.Rnw:653-654
###################################################
dat$y5 <- with(dat, -3*x1 + 3.5*log(x2) + 2.1*x2 + 2.2 *x1 * x2 + 20*rnorm(100)) 


###################################################
### code chunk number 30: pcps20
###################################################
m6 <- lm (y5 ~ log(x2*x2) + x1 * x2, data=dat)
plotCurves(m6, plotx="x2", modx="x1")


###################################################
### code chunk number 31: pp100
###################################################
p100 <- plotPlane(m4, plotx1="x1", plotx2="x2")


###################################################
### code chunk number 32: pcps10
###################################################
plotPlane(m6, plotx1="x1", plotx2="x2")


###################################################
### code chunk number 33: rockchalk.Rnw:706-707
###################################################
outreg(m6, tight=FALSE)


###################################################
### code chunk number 34: pp110
###################################################
m6ps <- plotSlopes(m6, plotx="x1", modx="x2", xlab="Continuous Predictor", ylim=c(-25, 105))


###################################################
### code chunk number 35: pp111
###################################################
p110 <- plotPlane(m6, plotx1="x1", plotx2="x2", x1lab="Continuous Predictor", phi=30)
for(j in unique(m6ps$newdata$x2)){
subdat <- m6ps$newdata[m6ps$newdata$x2==j,]
lines(trans3d(subdat$x1, subdat$x2, subdat$pred, pmat=p110$res), col="red", lwd=3)
}


###################################################
### code chunk number 36: rockchalk.Rnw:766-768
###################################################
m4 <- lm (y4 ~ x1 * x2, data=dat)
m4s <- standardize(m4)


###################################################
### code chunk number 37: rockchalk.Rnw:776-777
###################################################
summary(m4s)


###################################################
### code chunk number 38: stdreg10
###################################################
outreg(list(m4,m4s), tight=F, modelLabels=c("Not Standardized","Standardized"))


###################################################
### code chunk number 39: rockchalk.Rnw:835-837
###################################################
m4mc <- meanCenter(m4)
summary(m4mc)


###################################################
### code chunk number 40: rockchalk.Rnw:910-912
###################################################
m4rc <- residualCenter(m4)
summary(m4rc)


###################################################
### code chunk number 41: rockchalk.Rnw:983-989
###################################################
dat2 <- genCorrelatedData(N=400, rho=.4, stde=300, beta=c(2,0.1,0.1,0.2))

m6linear <- lm (y ~ x1 + x2, data=dat2)
m6int <- lm (y ~ x1 * x2, data=dat2)
m6mc <- meanCenter(m6int)
m6rc <- residualCenter(m6int)


###################################################
### code chunk number 42: mcenter10
###################################################
outreg(list(m6linear, m6int, m6mc, m6rc), tight=F, modelLabels=c("Linear", "Interaction","Mean-centered","Residual-centered"))


###################################################
### code chunk number 43: mcenter50
###################################################
op <- par(no.readonly = TRUE)
par(mfcol=c(2,2))
par(mar=c(2,2,2,1))
plotPlane(m6linear, plotx1="x1", plotx2="x2", plotPoints=FALSE, main="Linear", ticktype="detailed")
plotPlane(m6int, plotx1="x1", plotx2="x2", plotPoints=FALSE, main="Interaction: Not Centered", ticktype="detailed")
plotPlane(m6mc, plotx1="x1c", plotx2="x2c", plotPoints=FALSE, main="Mean-centered", ticktype="detailed")
plotPlane(m6rc, plotx1="x1", plotx2="x2", plotPoints=FALSE, main="Residual-centered", ticktype="detailed")
par(op)


###################################################
### code chunk number 44: rcenter40
###################################################
dat3 <- centerNumerics(dat2)
##m6mcpred <- fitted(m6mc) ##
m6mcpred <- predict(m6mc, newdata=dat3)
##m6rcpred <- fitted(m6rc) ##
m6rcpred <- predict(m6rc, newdata=dat3)
##m6intpred <- fitted(m6int) ##
m6intpred <- predict(m6int, newdata=dat3)
op <- par(no.readonly = TRUE)
par(mfcol=c(1,2))
##plot(fitted(m6rc), predict(m6rc, newdata=dat3))
##plot(fitted(m6mc), predict(m6mc, newdata=dat3))
plot(m6intpred, m6rcpred, main="", xlab="Predictions of Uncentered Interaction", ylab="Residual-centered Predictions")
predcor <- round(cor(m6intpred, m6rcpred),3)
legend("topleft", legend=c(paste("Correlation=", predcor)))
plot(m6mcpred, m6rcpred, main="", xlab="Mean-centered Predictions", ylab = "Residual-centered Predictions")
predcor <- round(cor(m6mcpred, m6rcpred),3)
legend("topleft", legend=c(paste("Correlation=", predcor)))
par(op)


