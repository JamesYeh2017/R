# 自行產生藥劑量與感冒痊癒天數資料
x= c(3,3,4,3,6,8,8,9)#藥劑量
y= c(22,25,18,20,16,9,12,5) #感冒痊癒天數
# 建立一個線性迴歸模型
new_x = data.frame(x=5)    #預測當x=5時的痊癒天數
train = data.frame(x=x,y=y)
imTrain = lm(formula = y~x,data=train)
#預測當x=5時的痊癒天數
predicted = predict(imTrain,new = new_x)
summary(LmTrain)
plot(y~x,main="醫藥劑量預測痊癒天數",xlab="藥劑量",ylab="感冒痊癒天數",family="STHeiti")
points(x=new_x, y=predicted, col="green", cex=2, pch=16)
abline(reg=imTrain$coefficients,col="red",lwd=1)


# 自行產生藥劑量、平均每日睡眠時間與感冒痊癒天數資料
x1 <- c(3,3,4,3,6,8,8,9) #藥劑量
x2 <- c(3,1,6,4,9,10,8,11) #平均每日睡眠時數
y <- c(22,25,18,20,16,9,12,5) #感冒痊癒天數
#新患者資料
New_x1 <- 5 #預測當x=5時的痊癒天數
New_x2 <- 7 #每日睡眠時數
New_data <- data.frame(x1 = 5, x2=7)
# 建立一個線性迴歸模型
Train <- data.frame(x1 = x1, x2=x2, y = y)
lmTrain <- lm(formula = y ~., data = Train)
#預測新患者感冒痊癒天數
predicted <- predict(lmTrain , newdata = New_data)
predicted     
# 模型摘要
summary(lmTrain )