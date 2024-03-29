### R code from vignette source 'Rchaeology.Rnw'

###################################################
### code chunk number 1: Rchaeology.Rnw:24-25
###################################################
  if(exists(".orig.enc")) options(encoding = .orig.enc)


###################################################
### code chunk number 2: Rchaeology.Rnw:158-159
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
### code chunk number 4: Rchaeology.Rnw:274-285
###################################################
dat <- data.frame(x1 = rnorm(100, m = 50), x2 = rnorm(100, m = 50),
  x3 = rnorm(100, m = 50), x4 = rnorm(100, m=50), y = rnorm(100))
m2 <- lm(y ~ log(x1) + x2*x3, data = dat)
suffixX <- function(fmla, x, s){
    upform <- as.formula(paste(". ~ .", "-", x, "+", paste(x, s, sep = ""), sep="", collapse=" "))
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
### code chunk number 6: Rchaeology.Rnw:371-377
###################################################
m1 <- lm(y ~ x1*x2, data = dat)
coef(m1)
regargs <- list(formula = y ~ x1*x2, data = quote(dat))
m2 <- do.call("lm", regargs)
coef(m2)
all.equal(m1, m2)


###################################################
### code chunk number 7: Rchaeology.Rnw:504-509
###################################################
m3 <- lm(y ~ x1*x2, data = dat)
coef(m3)
mycall <- call("lm", quote(y ~ x1*x2), data = quote(dat))
m4 <- eval(mycall)
coef(m4)


###################################################
### code chunk number 8: Rchaeology.Rnw:535-539
###################################################
f1 <- y ~ x1 + x2 + x3 + log(x4)
class(f1)
m5 <- lm(f1, data = dat)
coef(m5)


###################################################
### code chunk number 9: Rchaeology.Rnw:546-552
###################################################
f1[[1]]
f1[[2]]
f1[[3]]
f1[[3]][[1]]
f1[[3]][[2]]
f1[[3]][[3]]


###################################################
### code chunk number 10: Rchaeology.Rnw:567-570
###################################################
f1exp <- expression(y ~ x1 + x2 + x3 + log(x4))
class(f1exp)
m6 <- lm(eval(f1exp), data = dat)


###################################################
### code chunk number 11: Rchaeology.Rnw:575-580
###################################################
f1expeval <- eval(f1exp)
class(f1expeval)
all.equal(f1expeval, f1)
m7 <- lm(f1expeval, data=dat)
all.equal(coef(m6), coef(m7))	


###################################################
### code chunk number 12: Rchaeology.Rnw:587-590
###################################################
f1[[3]][[2]] <- quote(x1 + log(x2))
m8 <- lm(f1, data = dat)
coef(m8)


###################################################
### code chunk number 13: Rchaeology.Rnw:715-718
###################################################
plot(1:10, seq(1,5, length.out=10), type = "n", main="Illustrating Substitute with plotmath", xlab="x", ylab="y")
text(5, 4, substitute(gamma + x1mean, list(x1mean = mean(dat$x1))))
text(5, 2, expression(paste(gamma, " plus the mean of x1")))


###################################################
### code chunk number 14: Rchaeology.Rnw:741-742
###################################################
sublist <- list(x1 = "alphabet", x2 = "zoology")


###################################################
### code chunk number 15: Rchaeology.Rnw:752-753
###################################################
substitute(expression(x1 + x2 + log(x1) + x3), sublist)


###################################################
### code chunk number 16: Rchaeology.Rnw:768-770
###################################################
sublist <- list(x1 = as.name("alphabet"), x2 = as.name("zoology"))
substitute(expression(x1 + x2 + log(x1) + x3), sublist)


###################################################
### code chunk number 17: Rchaeology.Rnw:781-784
###################################################
dat <- data.frame(x1=1:10, x2=10:1, x3=rep(1:5,2), x4=gl(2,5))
colnames(dat)
names(dat)


###################################################
### code chunk number 18: Rchaeology.Rnw:790-793
###################################################
newnames <- c("whatever","sounds","good","tome")
colnames(dat) <- newnames
colnames(dat)


###################################################
### code chunk number 19: Rchaeology.Rnw:801-804
###################################################
dat2 <- setNames(data.frame(x1 = rnorm(10), x2 = rnorm(10),
    x3 = rnorm(10), x4 = gl(2,5)), c("good", "names", "tough", "find"))
head(dat2, 2)


###################################################
### code chunk number 20: Rchaeology.Rnw:813-817
###################################################
newnames <- c("iVar", "uVar", "heVar", "sheVar")
datcommand <- expression(data.frame(x1=1:10, x2=10:1, x3=rep(1:5,2), x4=gl(2,5)))
eval(datcommand)
dat3 <- setNames(eval(datcommand), newnames)


###################################################
### code chunk number 21: Rchaeology.Rnw:926-938
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
### code chunk number 22: Rchaeology.Rnw:950-958
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
### code chunk number 23: Rchaeology.Rnw:983-985
###################################################
biVec1(3.3, 1.8)
biVec2(3.3, 1.8)


###################################################
### code chunk number 24: Rchaeology.Rnw:993-1004
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


###################################################
### code chunk number 25: Rchaeology.Rnw:1104-1107 (eval = FALSE)
###################################################
## dat <- data.frame(myx = c(1, 2, 3), myy = c(10, 5, 1))
## debug(plot.default)
## plot(dat$myx, dat$myy)


###################################################
### code chunk number 26: Rchaeology.Rnw:1263-1265
###################################################
formals(plot)
formals(plot.default)


###################################################
### code chunk number 27: Rchaeology.Rnw:1272-1273
###################################################
formals(graphics:::plot.histogram)


###################################################
### code chunk number 28: Rchaeology.Rnw:1411-1420
###################################################
plotme <- function(x, y, data, ...){
    if(missing(data) | !is.data.frame(data)) 
        stop(paste0("plotme: the object you supplied as data: '", 
        deparse(substitute(data)) , "' is not a data.frame"))
    plot(as.formula(paste(y,  "~",  x, collapse=" ")), data = data, ...)
}
myx <- rnorm(10)
myy <- rnorm(10)
dat <- data.frame(myx, myy)


###################################################
### code chunk number 29: Rchaeology.Rnw:1425-1426 (eval = FALSE)
###################################################
## plotme("myx", "myy", data = dat)


###################################################
### code chunk number 30: Rchaeology.Rnw:1453-1458
###################################################
plotme <- function(x, y, z){
    plot(x, y, col = z)
}
myx <- rnorm(10)
myy <- rnorm(10)


###################################################
### code chunk number 31: Rchaeology.Rnw:1465-1467 (eval = FALSE)
###################################################
## mycol <- 1:10
## plotme(myx, myy, z = mycol)


###################################################
### code chunk number 32: Rchaeology.Rnw:1470-1472 (eval = FALSE)
###################################################
## mycol <- rainbow(10)
## plotme(myx, myy, z = mycol)


###################################################
### code chunk number 33: Rchaeology.Rnw:1475-1477 (eval = FALSE)
###################################################
## mycol <- gray.colors(10)
## plotme(myx, myy, z = mycol)


###################################################
### code chunk number 34: Rchaeology.Rnw:1483-1485 (eval = FALSE)
###################################################
## mycol <- rnorm(10)
## plotme(myx, myy, z = mycol)


###################################################
### code chunk number 35: Rchaeology.Rnw:1494-1498 (eval = FALSE)
###################################################
## plotme <- function(x, y, z){
##     if (missing(z) | is.null(z)) z <- rep(2, length(x))   
##     plot(x, y, col = z)
## }


###################################################
### code chunk number 36: Rchaeology.Rnw:1510-1512 (eval = FALSE)
###################################################
## m1 <- lm(y ~ x, data = dat)
## plot(y ~ x, data = dat)


###################################################
### code chunk number 37: Rchaeology.Rnw:1525-1529 (eval = FALSE)
###################################################
## plotme <- function(x, y, z = rainbow(length(x))){     
##     plot(x, y, col = z)
## }
## plotme(x = rnorm(100), y = rnorm(100))


###################################################
### code chunk number 38: Rchaeology.Rnw:1535-1540 (eval = FALSE)
###################################################
## plotme <- function(x, y, z){     
##   if(missing(z)) mycol <- rainbow(length(x))
##   plot(x, y, col = mycol)
## }
## plotme(x = rnorm(100), y = rnorm(100))


###################################################
### code chunk number 39: Rchaeology.Rnw:1551-1559 (eval = FALSE)
###################################################
## plotme <- function(x, y, z = getAColor(length(x)))
## {
##     getAColor <- function(n){
##              gray.colors(n)
##     }
##     plot(x, y, col = z, cex = 5)
## }
## plotme(rnorm(20), rnorm(20))


