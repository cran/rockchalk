## Paul Johnson
## pauljohn@ku.edu 2011-11-07
##
## One more bit in the "centered variables" don't really make a difference
## in interaction regressions.

## Here the idea is the following. The centered b's and se's "seem" different,
## but they are actually reflecting the EXACT SAME fitted plane. The
## difference is that the notcentered model has the y axis positioned at
## x1=0,x2=0, while in the centered model it is instead at
## x1=meanx1, x2=meanx2.

## Thus it should be possible to take the estimates from the
## notcentered regression and calculate the slopes at x1=meanx1,
## x2=meanx2, and re-produce them.  AND I CAN!! Yay.  The last dragon
## is slaked.


library(rockchalk)
set.seed(222233)
dat3 <- genCorrelatedData(rho = .31, stde = 80, beta=c(0.1, 0.2, 0.3, -0.2))

nointeract <-  mcGraph3(dat3$x1, dat3$x2, dat3$y, theta=-10, interaction = FALSE)
summary(nointeract[[1]])

## First, fit the model without the interaction term.
##
##
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 507.1200    49.3584   10.27   <2e-16 ***
## x1          -10.1333     0.8849  -11.45   <2e-16 ***
## x2           -9.6156     0.8127  -11.83   <2e-16 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 79.18 on 97 degrees of freedom
## Multiple R-squared: 0.813,	Adjusted R-squared: 0.8091 
## F-statistic: 210.8 on 2 and 97 DF,  p-value: < 2.2e-16
##
## Yeah, it is "significant"
##
## Add an interaction. Watch, it ruins everything!
##
noncentered <-  mcGraph3(dat3$x1, dat3$x2, dat3$y, theta=-10, interaction = TRUE)
summary(noncentered[[1]])

## Booh, the model's "no good" without centering
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)  
## (Intercept) 115.64608  185.93130   0.622   0.5354  
## x1           -2.28641    3.70149  -0.618   0.5382  
## x2           -1.70238    3.71525  -0.458   0.6478  
## x1:x2        -0.15641    0.07172  -2.181   0.0316 *
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 77.69 on 96 degrees of freedom
## Multiple R-squared: 0.8218,	Adjusted R-squared: 0.8162 
## F-statistic: 147.6 on 3 and 96 DF,  p-value: < 2.2e-16 


meanx1 <- mean(dat3$x1)
meanx2 <- mean(dat3$x2)

x1c <- dat3$x1 - mean(dat3$x1)
x2c <- dat3$x2 - mean(dat3$x2)


centered <-  mcGraph3(x1c, x2c, dat3$y, theta=-10, interaction = TRUE)
summary(centered[[1]])

## Hooray, centering "saved the day". 

## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -485.77517    8.17964 -59.388   <2e-16 ***
## x1           -10.25618    0.87004 -11.788   <2e-16 ***
## x2            -9.55114    0.79799 -11.969   <2e-16 ***
## x1:x2         -0.15641    0.07172  -2.181   0.0316 *  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 77.69 on 96 degrees of freedom
## Multiple R-squared: 0.8218,	Adjusted R-squared: 0.8162 
## F-statistic: 147.6 on 3 and 96 DF,  p-value: < 2.2e-16 

## Its just a mirage. 

## Take noncenterd model and calculate slopes at mean

coef(noncentered[[1]])["x1"] + coef(noncentered[[1]])["x1:x2"] * meanx2
##       x1 
##-10.25618 
##

coef(noncentered[[1]])["x2"] + coef(noncentered[[1]])["x1:x2"] * meanx1

##      x2 
##-9.551136 
##

## Now reproduce the standard errors in centered model from noncentered model

V <- vcov(noncentered[[1]])

sqrt(V["x1","x1"] + meanx2^2 * V["x1:x2","x1:x2"] + 2 * meanx2 * V["x1","x1:x2"])

## [1] 0.8700422

sqrt(V["x2","x2"] + meanx1^2 * V["x1:x2","x1:x2"] + 2 * meanx1 * V["x2","x1:x2"])

##[1] 0.7979912
##
##Bingo, Fits exactly. The estimates of the centered model are reproduced
## exactly from the notcentered model once the correct co-ordinate
## translation is put in place. The EXACT same t ratios, etc.

## Centering has NO EFFECT whatsoever on multicollinearity.
