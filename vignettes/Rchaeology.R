### R code from vignette source '/home/pauljohn/tmp/lyxtmp/lyx_tmpdir.T11044/lyx_tmpbuf0/Rchaeology.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: Rchaeology.Rnw:27-28
###################################################
  if(exists(".orig.enc")) options(encoding = .orig.enc)


###################################################
### code chunk number 2: Rchaeology.Rnw:157-158
###################################################
dir.create("graphics", showWarnings=T)


###################################################
### code chunk number 3: Roptions
###################################################
options(width=100, continue="  ")
options(useFancyQuotes = FALSE) 
set.seed(12345)
#op <- par() 
#pjmar <- c(5.1, 5.1, 1.5, 2.1) 
#pjmar <- par("mar")
#options(SweaveHooks=list(fig=function() par(mar=pjmar, ps=12)))
pdf.options(onefile=F,family="Times",pointsize=12)


###################################################
### code chunk number 4: Rchaeology.Rnw:329-341
###################################################
myfn <- function(x){
    if (x < 7)
    {
        print("x is less than 7")
    }
    else
    {
        print("x is excessive")
    }
}
myfn(3)
myfn(88)


###################################################
### code chunk number 5: Rchaeology.Rnw:726-733
###################################################
x <- runif(1000, min = 0, max = 100)
xf <- cut(x, breaks = c(-1, 20, 50, 80, 101), labels = c("cold", "luke", "warm", "hot"))
xfdummies <- contrasts(xf, contrasts = FALSE )[xf,]
colnames(xfdummies) <-  paste("xf", c("cold", "luke", "warm", "hot"), sep="")
rownames(xfdummies) <- names(x)
dat <- data.frame(x, xf, xfdummies)
head(dat)


###################################################
### code chunk number 6: Rchaeology.Rnw:741-752
###################################################
set.seed(12345)
x1 <- rnorm(200, m = 300, s = 140)
x2 <- rnorm(200, m = 80, s = 30)
y <- 3 + 0.2 * x1 + 0.4 * x2 + rnorm(200, s=400)
dat <- data.frame(x1, x2, y); rm(x1,x2,y)
m1 <- lm (y ~ x1 + x2, data = dat)
m1summary <- summary(m1)
(m1se <- m1summary$sigma)
(m1rsq <- m1summary$r.squared)
(m1coef <- m1summary$coef)
(m1aic <- AIC(m1))


###################################################
### code chunk number 7: ps10
###################################################
library(rockchalk)
dat$y2 = with(dat, 3 + 0.02 * x1 + 0.05 * x2 + 2.65 * x1 *x2 + rnorm(200, s=4000))
par(mfcol=c(1,2))
m1 <- lm(y2 ~ x1 + x2, data = dat)
m1i <- lm(y2 ~ x1 * x2, data = dat)
m1ps <- plotSlopes(m1, plotx = "x1", modx = "x2")
m1ips <- plotSlopes(m1i, plotx = "x1", modx = "x2")	


###################################################
### code chunk number 8: Rchaeology.Rnw:773-776
###################################################
m1imc <- meanCenter(m1i)
m1irc <- residualCenter(m1i)
outreg(list(m1, m1i, m1imc, m1irc), tight = TRUE, modelLabels = c("Linear", "Interaction", "Mean Centered", "Residual Centered"))


###################################################
### code chunk number 9: Rchaeology.Rnw:801-812
###################################################
dat <- data.frame(x1=rnorm(100,m=50), x2=rnorm(100,m=50),
x3=rnorm(100,m=50), x4 = rnorm(100, m=50), y=rnorm(100))
m2 <- lm(y ~ log(x1) + x2*x3, data=dat)
suffixX <- function(fmla, x, s){
upform <- as.formula(paste(". ~ .", "-", x, "+", paste(x, s, sep=""), sep=""))
update.formula(fmla, upform)
}
newFmla <- formula(m2)
newFmla
suffixX(newFmla, "x2", "c")
suffixX(newFmla, "x1", "c")


###################################################
### code chunk number 10: newFla10
###################################################
newFmla
newFmla[[1]]
newFmla[[2]]
newFmla[[3]]
newFmla[[3]][[2]]
newFmla[[3]][[2]][[2]]


###################################################
### code chunk number 11: Rchaeology.Rnw:896-902
###################################################
m1 <- lm(y ~ x1*x2, data=dat)
coef(m1)
regargs <- list(formula = y ~ x1*x2, data= quote(dat))
m2 <- do.call("lm", regargs)
coef(m2)
all.equal(m1, m2)


###################################################
### code chunk number 12: Rchaeology.Rnw:986-991
###################################################
m3 <- lm(y ~ x1*x2, data=dat)
coef(m3)
regargs2 <- expression(y ~ x1*x2, data = dat)
m4 <- lm(eval(regargs2))
coef(m4)


###################################################
### code chunk number 13: Rchaeology.Rnw:1000-1003
###################################################
f1 <- y ~ x1 + x2 + x3 + log(x4)
class(f1)
m5 <- lm(f1, data = dat)


###################################################
### code chunk number 14: Rchaeology.Rnw:1011-1017
###################################################
f1[[1]]
f1[[2]]
f1[[3]]
f1[[3]][[1]]
f1[[3]][[2]]
f1[[3]][[3]]


###################################################
### code chunk number 15: Rchaeology.Rnw:1033-1036
###################################################
f1exp <- expression(y ~ x1 + x2 + x3 + log(x4))
class(f1exp)
m6 <- lm(eval(f1exp), data=dat)


###################################################
### code chunk number 16: Rchaeology.Rnw:1041-1046
###################################################
f1expeval <- eval(f1exp)
class(f1expeval)
all.equal(f1expeval, f1)
m7 <- lm(f1expeval, data=dat)
all.equal(coef(m5), coef(m6), coef(m7))	


###################################################
### code chunk number 17: Rchaeology.Rnw:1172-1175
###################################################
plot(1:10, seq(1,5, length.out=10), type = "n", main="Illustrating Substitute with plotmath", xlab="x", ylab="y")
text(5, 4, substitute(gamma + x1mean, list(x1mean = mean(dat$x1))))
text(5, 2, expression(paste(gamma, " is the mean of x1")))


###################################################
### code chunk number 18: Rchaeology.Rnw:1198-1199
###################################################
sublist <- list(x1 = "alphabet", x2 = "zoology")


###################################################
### code chunk number 19: Rchaeology.Rnw:1206-1207
###################################################
substitute(expression(x1 + x2 + log(x1) + x3), sublist)


###################################################
### code chunk number 20: Rchaeology.Rnw:1220-1222
###################################################
sublist <- list(x1 = as.name("alphabet"), x2 = as.name("zoology"))
substitute(expression(x1 + x2 + log(x1) + x3), sublist)


###################################################
### code chunk number 21: Rchaeology.Rnw:1234-1237
###################################################
dat <- data.frame(x1=1:10, x2=10:1, x3=rep(1:5,2), x4=gl(2,5))
colnames(dat)
names(dat)


###################################################
### code chunk number 22: Rchaeology.Rnw:1243-1246
###################################################
newnames <- c("whatever","sounds","good","tome")
colnames(dat) <- newnames
colnames(dat)


###################################################
### code chunk number 23: Rchaeology.Rnw:1254-1256
###################################################
dat2 <- setNames(data.frame(x1=rnorm(10), x2=rnorm(10), x3=rnorm(10), x4=gl(2,5)), c("good","names","tough","find"))
head(dat2, 2)


###################################################
### code chunk number 24: Rchaeology.Rnw:1265-1269
###################################################
newnames <- c("iVar", "uVar", "heVar", "sheVar")
datcommand <- expression(data.frame(x1=1:10, x2=10:1, x3=rep(1:5,2), x4=gl(2,5)))
eval(datcommand)
dat3 <- setNames(eval(datcommand), newnames)


