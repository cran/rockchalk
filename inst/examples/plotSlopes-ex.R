## Manufacture some predictors
set.seed(12345)

dat <- genCorrelatedData2 (N = 100, means = rep(0,4), sds = 1, rho = 0.2,
                           beta = c(0.3, 0.5, -0.45, 0.5, -0.1, 0, 0.6),
                           stde = 2)

dat$xcat1 <- gl(2, 50, labels = c("M", "F"))
dat$xcat2 <- cut(rnorm(100), breaks = c(-Inf, 0, 0.4, 0.9, 1, Inf),
                 labels = c("R", "M", "D", "P", "G"))
## incorporate effect of categorical predictors
dat$y <- dat$y + 1.9 * dat$x1 * contrasts(dat$xcat1)[dat$xcat1] +
           contrasts(dat$xcat2)[dat$xcat2 , ] %*% c(0.1, -0.16, 0, 0.2)

m1 <- lm(y ~ x1 * x2 + x3 + x4 + xcat1* xcat2, data = dat)
summary(m1)

## New in rockchalk 1.7.x. No modx required:
plotSlopes(m1, plotx = "x1")
## Confidence interval, anybody?
plotSlopes(m1, plotx = "x1", interval = "conf")

## Prediction interval.
plotSlopes(m1, plotx = "x1", interval = "pred")

plotSlopes(m1, plotx = "x1", modx = "xcat2", modxVals = c("R", "M"))

plotSlopes(m1, plotx = "x1", modx = "xcat2",  interval = "pred")

plotSlopes(m1, plotx = "xcat1", modx = "xcat2",  interval = "conf", space = c(0,1))

plotSlopes(m1, plotx = "xcat1", modx = "xcat2", 
           modxVals = c("Print R" = "R" , "Show M" = "M"), gridArgs = "none")

## Now experiment with a moderator variable
## let default quantile algorithm do its job
plotSlopes(m1, plotx = "xcat2", interval = "none")
plotSlopes(m1, plotx = "xcat1", modx = "xcat2", interval = "none")
plotSlopes(m1, plotx = "xcat1", modx = "xcat2", interval = "confidence",
           legendArgs = list(title = "xcat2"), ylim = c(-3, 3), lwd = 0.4)
plotSlopes(m1, plotx = "xcat1", modx = "xcat2", interval = "confidence",
           legendArgs = list(title = "xcat2"), ylim = c(-3, 3), lwd = 0.4, width = 0.25)
m1.ps <- plotSlopes(m1, plotx = "xcat1", modx = "xcat2", interval = "prediction")
m1.ps <- plotSlopes(m1, plotx = "xcat1", modx = "xcat2", interval = "prediction", space=c(0,2))
plotSlopes(m1, plotx = "xcat1", modx = "xcat2", interval = "prediction", gridArgs = "none")

plotSlopes(m1, plotx = "xcat2", modx = "xcat1", interval = "confidence", ylim = c(-3, 3))
plotSlopes(m1, plotx = "xcat1", modx = "xcat2", interval = "confidence", 
           col = c("black", "blue", "green", "red", "orange"), lty = c(1, 4, 6, 3))

plotSlopes(m1, plotx = "xcat1", modx = "xcat2", interval = "confidence", 
           col = gray.colors(4, end = 0.5), lty = c(1, 4, 6, 3), legendArgs = list(horiz=TRUE))


plotSlopes(m1, plotx = "xcat1", modx = "xcat2", interval = "confidence", 
           col = c("pink", "orange"))

plotSlopes(m1, plotx = "xcat1", interval = "confidence", 
           col = c("black", "blue", "green", "red", "orange"))

plotSlopes(m1, plotx = "xcat1", modx = "xcat2", interval = "confidence", 
           col = c("black", "blue", "green", "red", "orange"),
           gridlwd = 0.2)

## previous uses default equivalent to
## plotSlopes(m1, plotx = "x1", modx = "x2", modxVals = "quantile")
## Want more focal values?
plotSlopes(m1, plotx = "x1", modx = "x2", n = 5)
## Pick focal values yourself?
plotSlopes(m1, plotx = "x1", modx = "x2", modxVals = c(-2, 0, 0.5))
## Alternative algorithm?
plotSlopes(m1, plotx = "x1", modx = "x2", modxVals = "std.dev.",
           main = "Uses \"std.dev.\" Divider for the Moderator",
           xlab = "My Predictor", ylab = "Write Anything You Want for ylab")

## Will catch output object from this one
m1ps <- plotSlopes(m1, plotx = "x1", modx = "x2", modxVals = "std.dev.", n = 5,
                 main = "Setting n = 5 Selects More Focal Values for Plotting")

m1ts <- testSlopes(m1ps)

plot(m1ts)

### Examples with categorical Moderator variable

m3 <- lm (y ~ x1 + xcat1, data = dat)
summary(m3)
plotSlopes(m3, modx = "xcat1", plotx = "x1")
plotSlopes(m3, modx = "xcat1", plotx = "x1", interval = "predict")
plotSlopes(m3, modx = "x1", plotx = "xcat1", interval = "confidence",
           legendArgs = list(x = "bottomright", title = ""))

m4 <- lm (y ~ x1 * xcat1, data = dat)
summary(m4)
plotSlopes(m4, modx = "xcat1", plotx = "x1")
plotSlopes(m4, modx = "xcat1", plotx = "x1", interval = "conf")


m5 <- lm (y ~ x1 + x2 + x1 * xcat2, data = dat)
summary(m5)
plotSlopes(m5, modx = "xcat2", plotx = "x1")
m5ps <- plotSlopes(m5, modx = "xcat2", plotx = "x1", interval = "conf")

testSlopes(m5ps)



## Now examples with real data. How about Chilean voters?
library(carData)
m6 <- lm(statusquo ~ income * sex, data = Chile)
summary(m6)
plotSlopes(m6, modx = "sex", plotx = "income")
m6ps <- plotSlopes(m6, modx = "sex", plotx = "income", col = c("orange", "blue"))

testSlopes(m6ps)

m7 <- lm(statusquo ~ region * income, data= Chile)
summary(m7)
plotSlopes(m7, plotx = "income", modx = "region")

plotSlopes(m7, plotx = "income", modx = "region", plotPoints = FALSE)
plotSlopes(m7, plotx = "income", modx = "region", plotPoints = FALSE,
           interval = "conf")
plotSlopes(m7, plotx = "income", modx = "region", modxVals = c("SA","S", "C"),
           plotPoints = FALSE, interval = "conf")
## Same, choosing 3 most frequent values
plotSlopes(m7, plotx = "income", modx = "region", n = 3, plotPoints = FALSE,
           interval = "conf")


m8 <- lm(statusquo ~ region * income + sex + age, data= Chile)
summary(m8)
plotSlopes(m8, modx = "region", plotx = "income")


m9 <- lm(statusquo ~ income * age + education + sex + age, data = Chile)
summary(m9)
plotSlopes(m9, modx = "income", plotx = "age")

m9ps <- plotSlopes(m9, modx = "income", plotx = "age")
m9psts <- testSlopes(m9ps)
plot(m9psts) ## only works if moderator is numeric

## Demonstrate re-labeling
plotSlopes(m9, modx = "income", plotx = "age", n = 5,
           modxVals = c("Very poor" = 7500,  "Rich" = 125000), 
           main = "Chile Data", legendArgs = list(title = "Designated Incomes"))

plotSlopes(m9, modx = "income", plotx = "age", n = 5, modxVals = c("table"), 
           main = "Moderator: mean plus/minus 2 SD")

## Convert education to numeric, for fun
Chile$educationn <- as.numeric(Chile$education)
m10 <- lm(statusquo ~ income * educationn + sex + age, data = Chile)
summary(m10)
plotSlopes(m10, plotx = "educationn",  modx = "income")

## Now, the occupational prestige data. Please note careful attention
## to consistency of colors selected
data(Prestige)
m11 <- lm(prestige ~ education * type, data = Prestige)

plotSlopes(m11, plotx = "education", modx = "type", interval = "conf")
dev.new()
plotSlopes(m11, plotx = "education", modx = "type",
           modxVals = c("prof"), interval = "conf")
dev.new()
plotSlopes(m11, plotx = "education", modx = "type",
           modxVals = c("bc"), interval = "conf")
dev.new()
plotSlopes(m11, plotx = "education", modx = "type",
           modxVals = c("bc", "wc"), interval = "conf")
