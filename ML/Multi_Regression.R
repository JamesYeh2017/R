#載入資料
library(ggplot2)
data(mtcars)

#觀察資料
head(mtcars)
#*****相關係數(Correlation coefficient)*****
cor(mtcars)[1,]

#mpg: Miles / (US) gallon 英哩/每加侖
#cyl: Number of cylinders 汽缸數
#disp: Displacement(cu.in.) 單汽缸排氣量
#hp: Gross horsepower 馬力
#wt: Weight(lb / 1000) 車重
#am: Transmission(0 = automatic, 1 = manual) 自手排

#整理資料
mycars <- mtcars
#調整欄位值
mycars$am <- ifelse(mtcars$am == 0, "automatic", "manual");
#增加總排氣量
mycars$tdisp <- mtcars$cyl * mtcars$disp
#整理資料
mycars <- mtcars
#調整欄位值
mycars$am <- ifelse(mtcars$am == 0, "automatic", "manual");
#增加總排氣量
mycars$tdisp <- mtcars$cyl * mtcars$disp

#散佈圖 車重 vs 油耗 
ggplot(mycars, aes(x = wt, y = mpg)) + geom_point(aes(color = am))
#散佈圖 馬力 vs 油耗 
ggplot(mycars, aes(x = hp, y = mpg)) + geom_point(aes(color = hp))
#箱型圖觀察 自手排 vs 油耗
ggplot(mycars, aes(x = am, y = mpg)) + geom_boxplot(aes(fill = am))

#*****複迴歸模型*****
mlm <- lm(mpg ~ wt + hp + am + tdisp, data = mycars)
summary(mlm)
#adj R^2 : 0.4~0.7   0.7~1

#Y = a + Bwt*x1 + Bhp*x2 +Bam*x3 + Btdisp*x4
#Y = 34.50+(-3.10)*x1+(-0.04)*x2+(2.1884623)*x3 + (0.0003938)*x4
#args = Estimate

#wt 車重, hp 馬力, am 自/手排, tdisp 排氣量
df <- data.frame(wt = 3,hp=245,am="automatic",tdisp=1548)
result <- predict(mlm, newdata = df)
result




