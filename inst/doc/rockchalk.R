### R code from vignette source 'rockchalk.Rnw'

###################################################
### code chunk number 1: rockchalk.Rnw:18-19
###################################################
  if(exists(".orig.enc")) options(encoding = .orig.enc)


###################################################
### code chunk number 2: Roptions
###################################################
options(device = pdf)
options(width=80, prompt=" ", continue="  ")
options(useFancyQuotes = FALSE) 


###################################################
### code chunk number 3: rockchalk.Rnw:188-189
###################################################
library(rockchalk)


###################################################
### code chunk number 4: rockchalk.Rnw:195-198
###################################################
library(carData)
data(Chile)
(summChile <- summarize(Chile))


###################################################
### code chunk number 5: rockchalk.Rnw:212-213
###################################################
centralValues(Chile)


###################################################
### code chunk number 6: rockchalk.Rnw:230-231
###################################################
m1 <- lm(statusquo ~ age + income + population + region + sex, data = Chile)


###################################################
### code chunk number 7: rockchalk.Rnw:237-239
###################################################
m1pred <- predictOMatic(m1)
m1pred


###################################################
### code chunk number 8: rockchalk.Rnw:270-276
###################################################
mypred2 <- predictOMatic(m1, predVals = c("age", "region"), n = 3)
mypred2
mypred3 <- predictOMatic(m1, predVals = c(age = "std.dev.", region = "table"), n = 3)
mypred3
mypred4 <- predictOMatic(m1, predVals = list(age = c(18, 30, 45), region = c("SA", "C","N")), n = 3)
mypred4


###################################################
### code chunk number 9: rockchalk.Rnw:297-299
###################################################
mynewdf <- newdata(m1, predVals = c("age","region"), n = 3)
mynewdf


###################################################
### code chunk number 10: rockchalk.Rnw:302-304
###################################################
mynewdf2 <- newdata(m1, predVals = list(age = "std.dev.", region = c("SA", "C","N")))
mynewdf2


###################################################
### code chunk number 11: rockchalk.Rnw:307-309
###################################################
mynewdf3 <- newdata(m1, predVals = list(age = c(20, 30, 40), region = c("SA", "C","N")))
mynewdf3


###################################################
### code chunk number 12: rockchalk.Rnw:314-316
###################################################
mynewdf <- newdata(m1, predVals = list(age = getFocal(Chile$age, n = 3), region = getFocal(Chile$region, n = 3)))
mynewdf


###################################################
### code chunk number 13: rockchalk.Rnw:340-345
###################################################
df <- data.frame(ldose = rep(0:5, 2), sex = factor(rep(c("M", "F"), c(6, 6))), 
	SF.numdead = c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16))      
df$SF.numalive <-  20 - df$SF.numdead
budworm.lg <- glm(cbind(SF.numdead, SF.numalive) ~ sex*ldose, data = df,  family = binomial)
predictOMatic(budworm.lg, predVals = c(ldose = "std.dev.", sex = "table"), interval = "confidence")  


###################################################
### code chunk number 14: rockchalk.Rnw:361-362
###################################################
descriptiveTable(m1, digits=6)


###################################################
### code chunk number 15: createdata1
###################################################
set.seed(1234)
dat <- genCorrelatedData3(N = 100, means = c(x1 = 0, x2 = 10, x3 = 0), sds = c(1, 2, 1), rho = c(0, 0, 0), stde = 10, beta = c(0, -3, 4, 0), verbose = FALSE)


###################################################
### code chunk number 16: createdata1
###################################################
m1 <- lm(y ~ x1 + x2, data = dat)
m2 <- lm(y ~ x2, data = dat)
m3 <- lm(y ~ x1 + x2 + x3, data = dat)
## Create categorical variant
myilogit <- function(x) exp(x)/(1 + exp(x))
dat$y3 <- rbinom(100, size = 1, p = myilogit(scale(dat$y)))
gm1 <- glm(y3 ~ x1 + x2, data = dat)


###################################################
### code chunk number 17: outreg10
###################################################
or <- outreg(list(m1, m2, m3), centering = "siunitx")


###################################################
### code chunk number 18: outreg20
###################################################
or1 <- outreg(list("The First Model with a Long Title" = m1, "Another Model" = m3), tight = FALSE, alpha = c(0.05, 0.01, 0.001), centering = "siunitx")


###################################################
### code chunk number 19: outreg70
###################################################
or2 <- outreg(list(m1,gm1), modelLabels = c("OLS:y","GLM: Categorized y"))


###################################################
### code chunk number 20: ps05
###################################################
getOption("SweaveHooks")[["fig"]]()
m1ps <- plotSlopes(m1, plotx = "x2", xlab = "x2 from model m1", interval = "confidence", opacity = 80, col = "red",
ylim = c(-70, 15), legendArgs = list(x="topright"))


###################################################
### code chunk number 21: rockchalk.Rnw:514-515 (eval = FALSE)
###################################################
## m1ps <- plotSlopes(m1, plotx = "x2", xlab = "x2 from model m1", interval = "confidence", opacity = 80, col = "red",
## ylim = c(-70, 15), legendArgs = list(x="topright"))


###################################################
### code chunk number 22: rockchalk.Rnw:529-530
###################################################
m1ps$newdata[1:3, ]


###################################################
### code chunk number 23: ps09
###################################################
dat$y4 <- 1 + 0.1 * dat$x1 - 6.9 * dat$x2 + 0.5 * dat$x1*dat$x2 + 0.2 * dat$x3 + rnorm(100, m = 0, sd = 10)
m4 <- lm(y4 ~ x1*x2 + x3, data = dat)


###################################################
### code chunk number 24: ps10
###################################################
getOption("SweaveHooks")[["fig"]]()
par(mfcol=c(2,1))
m4psa <- plotSlopes(m4, plotx = "x1", modx = "x2", xlab = "x1 is a fun plotx")
m4psb <- plotSlopes(m4, plotx = "x2", modx = "x1", modxVals = "std.dev.", xlab = "x2 is plotx", ylim = c(-120, 15),
legendArgs = list(x = "topright"))
par(mfcol=c(1,1))


###################################################
### code chunk number 25: rockchalk.Rnw:568-569 (eval = FALSE)
###################################################
## par(mfcol=c(2,1))
## m4psa <- plotSlopes(m4, plotx = "x1", modx = "x2", xlab = "x1 is a fun plotx")
## m4psb <- plotSlopes(m4, plotx = "x2", modx = "x1", modxVals = "std.dev.", xlab = "x2 is plotx", ylim = c(-120, 15),
## legendArgs = list(x = "topright"))
## par(mfcol=c(1,1))


###################################################
### code chunk number 26: rockchalk.Rnw:585-589
###################################################
fourCat <- gl(4,25, labels=c("East","West","South", "Midwest"))
dat$x4 <- sample(fourCat, 100, replace = TRUE)
dat$y5 <- 1 + 0.1 * dat$x1 + contrasts(dat$x4)[dat$x4, ] %*% c(-1,1,2) + rnorm(100,0, sd=10)
m5 <- lm (y5 ~ x1*x4 + x3, data=dat)


###################################################
### code chunk number 27: ps20
###################################################
getOption("SweaveHooks")[["fig"]]()
m5psa <- plotSlopes(m5, plotx = "x1", modx = "x4", xlab = "x1 is a Continuous Predictor", xlim = magRange(dat$x1, c(1.2,1)))


###################################################
### code chunk number 28: ps21
###################################################
getOption("SweaveHooks")[["fig"]]()
m5psb <- plotSlopes(m5, plotx = "x1", modx = "x4", modxVals = c("West","East"), xlab = "x1 is a Continuous Predictor", xlim=magRange(dat$x1, c(1.2,1)), interval = "conf")


###################################################
### code chunk number 29: rockchalk.Rnw:613-614 (eval = FALSE)
###################################################
## m5psa <- plotSlopes(m5, plotx = "x1", modx = "x4", xlab = "x1 is a Continuous Predictor", xlim = magRange(dat$x1, c(1.2,1)))


###################################################
### code chunk number 30: rockchalk.Rnw:625-626 (eval = FALSE)
###################################################
## m5psb <- plotSlopes(m5, plotx = "x1", modx = "x4", modxVals = c("West","East"), xlab = "x1 is a Continuous Predictor", xlim=magRange(dat$x1, c(1.2,1)), interval = "conf")


###################################################
### code chunk number 31: ts10
###################################################
getOption("SweaveHooks")[["fig"]]()
m4psats <- testSlopes(m4psa)
plot(m4psats)


###################################################
### code chunk number 32: rockchalk.Rnw:736-737
###################################################
dat$y5 <- with(dat, -3*x1 + 15*log(0.1 + x2 - min(x2)) + 1.1*x2 + 8.2 *x1 * x2 + 10*rnorm(100))


###################################################
### code chunk number 33: rockchalk.Rnw:742-743
###################################################
m5 <- lm(y5 ~ log(x2) + x1 * x2, data = dat)


###################################################
### code chunk number 34: pcps20a (eval = FALSE)
###################################################
getOption("SweaveHooks")[["fig"]]()
## m5pc <- plotCurves(m5, plotx = "x2", modx = "x1", main = "plotCurves output", ylim = c(-210, 330),
## legendArgs = list(x = "topleft", title = "x1", cex = 0.8))


###################################################
### code chunk number 35: psps20b (eval = FALSE)
###################################################
getOption("SweaveHooks")[["fig"]]()
## m5ps <- plotSlopes(m5, plotx = "x2", modx = "x1", main = "plotSlopes outout", ylim = c(-210, 330),
## legendArgs = list(x = "topleft", title = "x1", cex = 0.8))


###################################################
### code chunk number 36: rockchalk.Rnw:764-765
###################################################
getOption("SweaveHooks")[["fig"]]()
m5pc <- plotCurves(m5, plotx = "x2", modx = "x1", main = "plotCurves output", ylim = c(-210, 330),
legendArgs = list(x = "topleft", title = "x1", cex = 0.8))


###################################################
### code chunk number 37: rockchalk.Rnw:768-769
###################################################
getOption("SweaveHooks")[["fig"]]()
m5ps <- plotSlopes(m5, plotx = "x2", modx = "x1", main = "plotSlopes outout", ylim = c(-210, 330),
legendArgs = list(x = "topleft", title = "x1", cex = 0.8))


###################################################
### code chunk number 38: pp100
###################################################
getOption("SweaveHooks")[["fig"]]()
p100 <- plotPlane(m4, plotx1 = "x1", plotx2 = "x2", phi = 10, theta = -80, lcol = gray(.70))


###################################################
### code chunk number 39: rockchalk.Rnw:808-809 (eval = FALSE)
###################################################
## p100 <- plotPlane(m4, plotx1 = "x1", plotx2 = "x2", phi = 10, theta = -80, lcol = gray(.70))


###################################################
### code chunk number 40: pp111
###################################################
getOption("SweaveHooks")[["fig"]]()
ppm5 <- plotPlane(m5, plotx1 = "x2", plotx2 = "x1", phi = 0, npp = 15, lcol = gray(.80))
addLines(from = m5pc, to = ppm5, col = m5pc$col)


###################################################
### code chunk number 41: rockchalk.Rnw:857-859
###################################################
m4 <- lm (y4 ~ x1 * x2, data = dat)
m4s <- standardize(m4)


###################################################
### code chunk number 42: rockchalk.Rnw:865-866
###################################################
summary(m4s)


###################################################
### code chunk number 43: stdreg10
###################################################
or10 <- outreg(list(m4, m4s), tight = F, modelLabels = c("Not Standardized","Standardized"))


###################################################
### code chunk number 44: rockchalk.Rnw:913-915
###################################################
m4mc <- meanCenter(m4)
summary(m4mc)


###################################################
### code chunk number 45: rockchalk.Rnw:949-951
###################################################
m4rc <- residualCenter(m4)
summary(m4rc)


###################################################
### code chunk number 46: rockchalk.Rnw:1093-1098
###################################################
dat2 <- genCorrelatedData3(N=400, rho=.4, stde=300, beta=c(2,0.1,0.1,0.2))
m6linear <- lm (y ~ x1 + x2, data=dat2)
m6int <- lm (y ~ x1 * x2, data=dat2)
m6mc <- meanCenter(m6int)
m6rc <- residualCenter(m6int)


###################################################
### code chunk number 47: mcenter10
###################################################
or11 <- outreg(list(m6linear, m6int, m6mc, m6rc), tight=F, modelLabels=c("Linear", "Interaction","Mean-centered","Residual-centered"), alpha = c(0.05, 0.01))


###################################################
### code chunk number 48: pscenter
###################################################
getOption("SweaveHooks")[["fig"]]()
par(mfcol = c(2, 1))
plotSlopes(m6int, plotx = "x1", modx = "x2", modxVals = "std.dev.",  n = 2, interval = "confidence", main= "Not Centered")
plotSlopes(m6mc, plotx = "x1c", modx = "x2c", modxVals = "std.dev.", n = 2, interval = "confidence", main = "Mean Centered")
par(mfcol = c(1, 1))


###################################################
### code chunk number 49: mcenter50
###################################################
getOption("SweaveHooks")[["fig"]]()
op <- par(no.readonly = TRUE)
par(mfcol=c(2,2))
par(mar=c(2,2,2,1))
plotPlane(m6linear, plotx1="x1", plotx2="x2", plotPoints=FALSE, main="Linear", ticktype="detailed")
plotPlane(m6int, plotx1="x1", plotx2="x2", plotPoints=FALSE, main="Interaction: Not Centered", ticktype="detailed")
plotPlane(m6mc, plotx1="x1c", plotx2="x2c", plotPoints=FALSE, main="Mean-centered", ticktype="detailed")
plotPlane(m6rc, plotx1="x1", plotx2="x2", plotPoints=FALSE, main="Residual-centered", ticktype="detailed")
par(op)


###################################################
### code chunk number 50: rcenter40
###################################################
getOption("SweaveHooks")[["fig"]]()
dat3 <- centerNumerics(dat2)
##m6mcpred <- fitted(m6mc) ##
m6mcpred <- predict(m6mc, newdata=dat3)
##m6rcpred <- fitted(m6rc) ##
m6rcpred <- predict(m6rc, newdata=dat3)
##m6intpred <- fitted(m6int) ##
m6intpred <- predict(m6int, newdata=dat3)
op <- par(no.readonly = TRUE)
par(mfcol=c(1,2))
plot(m6intpred, m6rcpred, main="", xlab="Predictions of Uncentered Interaction", ylab="Residual-centered Predictions")
predcor <- round(cor(m6intpred, m6rcpred),3)
legend("topleft", legend=c(paste0("Correlation=", predcor)))
plot(m6mcpred, m6rcpred, main="", xlab="Mean-centered Predictions", ylab = "Residual-centered Predictions")
predcor <- round(cor(m6mcpred, m6rcpred),3)
legend("topleft", legend=c(paste0("Correlation=", predcor)))
par(op)


