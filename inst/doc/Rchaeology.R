### R code from vignette source 'Rchaeology.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: Rchaeology.Rnw:26-27
###################################################
  if(exists(".orig.enc")) options(encoding = .orig.enc)


###################################################
### code chunk number 2: Rchaeology.Rnw:159-160
###################################################
dir.create("plots", showWarnings=F)


###################################################
### code chunk number 3: Roptions
###################################################
options(width=100, continue="  ")
options(useFancyQuotes = FALSE) 
set.seed(12345)
pdf.options(onefile=F,family="Times",pointsize=12)


###################################################
### code chunk number 4: Rchaeology.Rnw:277-288
###################################################
dat <- data.frame(x1 = rnorm(100, m = 50), x2 = rnorm(100, m = 50),
  x3 = rnorm(100, m = 50), x4 = rnorm(100, m=50), y = rnorm(100))
m2 <- lm(y ~ log(x1) + x2*x3, data = dat)
suffixX <- function(fmla, x, s){
    upform <- as.formula(paste(". ~ .", "-", x, "+", paste(x, s, sep = ""), sep=""))
    update.formula(fmla, upform)
}
newFmla <- formula(m2)
newFmla
suffixX(newFmla, "x2", "c")
suffixX(newFmla, "x1", "c")


###################################################
### code chunk number 5: newFla10
###################################################
newFmla
newFmla[[1]]
newFmla[[2]]
newFmla[[3]]
newFmla[[3]][[2]]
newFmla[[3]][[2]][[2]]


###################################################
### code chunk number 6: Rchaeology.Rnw:381-387
###################################################
m1 <- lm(y ~ x1*x2, data = dat)
coef(m1)
regargs <- list(formula = y ~ x1*x2, data = quote(dat))
m2 <- do.call("lm", regargs)
coef(m2)
all.equal(m1, m2)


###################################################
### code chunk number 7: Rchaeology.Rnw:470-475
###################################################
m3 <- lm(y ~ x1*x2, data=dat)
coef(m3)
regargs2 <- expression(y ~ x1*x2, data = dat)
m4 <- lm(eval(regargs2))
coef(m4)


###################################################
### code chunk number 8: Rchaeology.Rnw:485-488
###################################################
f1 <- y ~ x1 + x2 + x3 + log(x4)
class(f1)
m5 <- lm(f1, data = dat)


###################################################
### code chunk number 9: Rchaeology.Rnw:496-502
###################################################
f1[[1]]
f1[[2]]
f1[[3]]
f1[[3]][[1]]
f1[[3]][[2]]
f1[[3]][[3]]


###################################################
### code chunk number 10: Rchaeology.Rnw:518-521
###################################################
f1exp <- expression(y ~ x1 + x2 + x3 + log(x4))
class(f1exp)
m6 <- lm(eval(f1exp), data=dat)


###################################################
### code chunk number 11: Rchaeology.Rnw:526-531
###################################################
f1expeval <- eval(f1exp)
class(f1expeval)
all.equal(f1expeval, f1)
m7 <- lm(f1expeval, data=dat)
all.equal(coef(m5), coef(m6), coef(m7))	


###################################################
### code chunk number 12: Rchaeology.Rnw:660-663
###################################################
plot(1:10, seq(1,5, length.out=10), type = "n", main="Illustrating Substitute with plotmath", xlab="x", ylab="y")
text(5, 4, substitute(gamma + x1mean, list(x1mean = mean(dat$x1))))
text(5, 2, expression(paste(gamma, " is the mean of x1")))


###################################################
### code chunk number 13: Rchaeology.Rnw:686-687
###################################################
sublist <- list(x1 = "alphabet", x2 = "zoology")


###################################################
### code chunk number 14: Rchaeology.Rnw:697-698
###################################################
substitute(expression(x1 + x2 + log(x1) + x3), sublist)


###################################################
### code chunk number 15: Rchaeology.Rnw:713-715
###################################################
sublist <- list(x1 = as.name("alphabet"), x2 = as.name("zoology"))
substitute(expression(x1 + x2 + log(x1) + x3), sublist)


###################################################
### code chunk number 16: Rchaeology.Rnw:727-730
###################################################
dat <- data.frame(x1=1:10, x2=10:1, x3=rep(1:5,2), x4=gl(2,5))
colnames(dat)
names(dat)


###################################################
### code chunk number 17: Rchaeology.Rnw:736-739
###################################################
newnames <- c("whatever","sounds","good","tome")
colnames(dat) <- newnames
colnames(dat)


###################################################
### code chunk number 18: Rchaeology.Rnw:747-750
###################################################
dat2 <- setNames(data.frame(x1 = rnorm(10), x2 = rnorm(10),
    x3 = rnorm(10), x4 = gl(2,5)), c("good", "names", "tough", "find"))
head(dat2, 2)


###################################################
### code chunk number 19: Rchaeology.Rnw:759-763
###################################################
newnames <- c("iVar", "uVar", "heVar", "sheVar")
datcommand <- expression(data.frame(x1=1:10, x2=10:1, x3=rep(1:5,2), x4=gl(2,5)))
eval(datcommand)
dat3 <- setNames(eval(datcommand), newnames)


###################################################
### code chunk number 20: Rchaeology.Rnw:889-901
###################################################
biVec1 <- function(n = 3, n1s = 1) {
    c(rep(1, n1s), rep(0, n - n1s))
}

biVec2 <- function(n = 3, n1s = 1){
    x <- numeric(length = n)
    x[1:n1s] <- 1
    x
}

(corr <- biVec1(n = 8, n1s = 3))
(corr <- biVec2(n = 8, n1s = 3))


###################################################
### code chunk number 21: Rchaeology.Rnw:913-921
###################################################
biVec3 <- function(n = 3, n1s = 1) {
    X <- c(rep(1, n1s), rep(0, n - n1s))
    xnam <- paste("corr.", 1:8, "0", sep = "")
    for(i in 1:n) assign(xnam[i], X[i], envir = .GlobalEnv) 
}
ls()
biVec3(n = 8, n1s = 3)
ls()


###################################################
### code chunk number 22: Rchaeology.Rnw:947-949
###################################################
biVec1(3.3, 1.8)
biVec2(3.3, 1.8)


###################################################
### code chunk number 23: Rchaeology.Rnw:957-968
###################################################
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
 abs(x - round(x)) < tol
}

biVec1 <- function(n = 3, n1s = 1) {
    if(!(is.wholenumber(n) & is.wholenumber(n1s)))
        stop("Both n and n1s must be whole numbers (integers)")
    if(n1s > n)
        stop("n must be greater than or equal to n1s")
    c(rep(1, n1s), rep(0, n - n1s))
}


