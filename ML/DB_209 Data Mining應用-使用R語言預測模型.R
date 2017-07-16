# title: 資料探勘技術應用 2-預測模型
# author: Ming-Chang Lee
# email: alan9956@gmail.com
# RWEPA: http://rwepa.blogspot.tw/
# encoding: UTF-8

# 大綱 -----
# 1. 預測模型簡介
# 2. R在線性模型的應用
# 3. R在非線性模型的應用

# 1. 預測模型簡介 -----

# R在移動平均法的應用 -----
library(TTR)
?SMA
# try: SMA(1:10,2)
# 資料檢視
data(ttrc)
str(ttrc)
ttrc.actual <- tail(ttrc[,"Close"],100)
plot(ttrc.actual, type="l", ylab="Close")

ttrc.forecast <- SMA(ttrc.actual, 5)
lines(ttrc.forecast, col="red")

# 移動平均法模型評估
number.observations <- length(ttrc.actual)-sum(is.na(ttrc.forecast)) # 96

mad.error <- sum(abs(ttrc.actual-ttrc.forecast), na.rm=TRUE)/number.observations # 0.357

mse.error <- sum((ttrc.actual-ttrc.forecast)^2, na.rm=TRUE)/(number.observations-1) # 0.217

mape.error <- sum(abs(ttrc.actual-ttrc.forecast)/ttrc.actual, na.rm=TRUE)*100/number.observations # 0.743%

mad.error
mse.error
mape.error

# 指數平滑法

# forecat(t) = forecast(t-1) + alpha.value*[actual(t-1) - forecat(t-1)]

alpha.value <- 0.05
ttrc.forecast.es <- vector(mode="numeric", length(ttrc.actual))
ttrc.forecast.es[1] <- NA
ttrc.forecast.es[2] <- ttrc.actual[1]
for (i in 3:length(ttrc.actual)) {
  ttrc.forecast.es[i] <- ttrc.forecast.es[i-1] + alpha.value*(ttrc.actual[i-1] - ttrc.forecast.es[i-1])
}
ttrc.forecast.es

plot(ttrc.actual, type="l", ylab="Close")
lines(ttrc.forecast.es, col="blue")

# 2. R在線性模型的應用 -----

#  線性迴歸模型 lm 函數的使用
# Simple linear regression
?lm
# my.lm <- lm(formula, data="xxx")
# formula: y ~ x1 + x2 + ... +xn

# women: Average Heights and Weights for American Women
# y: weight
# x: height
fit.lm <- lm(weight ~ height, data=women)
summary(fit.lm)
# weight = -87.52+3.45*height

# Leverage measure, Cook's distance
op <- par(mfrow=c(2,2))
plot(fit.lm)
par(op)

# verify residuals
names(fit.lm)
women$weight   # actual
fitted(fit.lm) # predicted
residuals(fit.lm) # residual=actual-predicted
women$weight - fitted(fit.lm)

# plot data
plot(women$height,women$weight, xlab="Height (in inches)", ylab="Weight (in pounds)", main="Average Heights and Weights for American Women")
abline(fit.lm, col="red")
# end

# 二次式模型
fit.poly.lm <- lm(weight ~ height + I(height^2), data=women)
summary(fit.poly.lm)
# weight = 261.88 - 7.35*height + 0.083*height^2

# plot data with polynomial regression
plot(women$height,women$weight, main="Polynomial regression", xlab="Height (in inches)", ylab="Weight (in lbs)")
lines(women$height, fitted(fit.poly.lm), col="blue")
# end

# 三次式模型 cubic polynomial regression
fit.cubic.lm <- lm(weight ~ height + I(height^2) +I(height^3), data=women)
summary(fit.cubic.lm)

# plot data with cubic polynomial regression
plot(women$height,women$weight, main="Cubic polynomial regression", xlab="Height (in inches)", ylab="Weight (in lbs)")
lines(women$height, fitted(fit.cubic.lm), col="blue")
# end

# 補充篇:繪製散佈圖與盒鬚圖
# scatterplot{car}
library(car)
scatterplot(weight ~ height, data=women, pch=19, spread=FALSE,
            lty=2, # lty=2, dashed line in linear model
            main="Women Age 30-39",
            xlab="Height (inches)",
            ylab="Weight (lbs.)")
# end

# compare of lm, Resistant, robust -----
# example - house price
library(MASS)
library(nutshell)
data(shiller) # nutshell package
hpi.lm <- lm(Real.Home.Price.Index~Year, data=shiller.index)
hpi.rlm <- rlm(Real.Home.Price.Index~Year, data=shiller.index)
hpi.lqs <- lqs(Real.Home.Price.Index~Year, data=shiller.index)
plot(Real.Home.Price.Index~Year, pch=19, cex=0.3, data=shiller.index)
abline(reg=hpi.lm, lty=1)
abline(reg=hpi.rlm, lty=2, col="red")
abline(reg=hpi.lqs, lty=3, col="green")
legend(x=1900, y=200, legend=c("lm", "rlm", "lqs"), lty=c(1, 2, 3), col=c(1,2,3))

# 3. R在非線性模型的應用 -----

# nonlinear regression
# example - population growth
library(car)
data(USPop)
attach(USPop)
plot(year, population)

# nls demo
time <- 0:21
pop.mod <- nls(population ~ beta1/(1 + exp(beta2 + beta3*time)),
               start=list(beta1 = 350, beta2 = 4.5, beta3 = -0.3),
               trace=TRUE)
summary(pop.mod)

lines(year, fitted.values(pop.mod), lwd=2, col="red")

plot(year, residuals(pop.mod), type="b")
abline(h=0, lty=2)
# end

# lasso -----

library(lars)
# http://web.ydu.edu.tw/~alan9956/rdata/prostate.csv
prostate <- read.csv("prostate.csv")
prostate[1:3,]
m1 <- lm(lcavol~.,data=prostate)
summary(m1)

## the model.matrix statement defines the model to be fitted
x <- model.matrix(lcavol~age+lbph+lcp+gleason+lpsa, data=prostate)
x
## stripping off the column of 1s as LASSO includes the intercept
x <- x[,-1]

## lasso on all data
lasso <- lars(x=x, y=prostate$lcavol,trace=TRUE)

## trace of lasso (standardized) coefficients for varying penalty
plot(lasso)

lasso
coef(lasso,s=c(.25,.50,0.75,1.0), mode="fraction")

## cross-validation using 10 folds
cv.lars(x=x,y=prostate$lcavol,K=10)

# glm -----

# example - Logistic regression

# install.packages("AER")
# library(AER)
# ?Affairs
data(Affairs, package="AER")
summary(Affairs)
table(Affairs$affairs)
head(Affairs)

# add new column
Affairs$ynaffair[Affairs$affairs > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair,
                           levels=c(0,1),
                           labels=c("No","Yes"))
table(Affairs$ynaffair)

# logistic regression- consider all variables
fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children + religiousness + education + occupation + rating, data=Affairs, family=binomial())

summary(fit.full)

# logistic regression- droped 4 variables
fit.reduced <- glm(ynaffair ~ age + yearsmarried 
                   + religiousness 
                   + rating, 
                   data=Affairs, family=binomial())

summary(fit.reduced)

# model comparision
anova(fit.reduced, fit.full, test="Chisq")

# interpreting the model parameters
coef(fit.reduced)

# exponentiate to put the results on an odds scale
exp(coef(fit.reduced))

# confidence interval
exp(confint(fit.reduced))

# sensitivity analysis for rating
testdata <- data.frame(rating=c(1, 2, 3, 4, 5), age=mean(Affairs$age),
                       yearsmarried=mean(Affairs$yearsmarried),
                       religiousness=mean(Affairs$religiousness))
testdata
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata

# sensitivity analysis for age
testdata <- data.frame(rating=mean(Affairs$rating),
                       age=seq(17, 57, 10),
                       yearsmarried=mean(Affairs$yearsmarried),
                       religiousness=mean(Affairs$religiousness))
testdata
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata
# end