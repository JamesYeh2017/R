library(readxl)
data = read_excel("C:/Users/BIG DATA/Desktop/DL/0722.xlsx")
View(data)
plot(x = data$years, y = data$mileage)
plot(x = data$years, y = log(data$mileage))
summary(data$tag_price)        # tag_price range is too large

#FORD Brand DataTest
ford = as.data.frame(filter(data, brand == 'FORD'))
View(ford)
summary(ford$tag_price)
count(filter(data, brand == 'FORD'))
mean(ford$tag_price) / sd(ford$tag_price)
hist(ford$tag_price)
hist(log(ford$tag_price))

#parser Data
parserData = subset(data, select = c(usingyears, mileage, residual_value))
cor(parserData)

logP = log(parserData$residual_value)
hist(logP)
logP <- abs(log(ddd$currentPrice/ddd$tag_price))
data = cbind(data, logP)
parserData2 = subset(data, select = c(usingyears, mileage, residual_value, logP))
cor(parserData2)

formu3 <- logP~log(usingyears) + mileage
asd3 <- glm(formula = formu3, data = ddd, family = 'Gamma'(link = 'inverse'))

# 1 - (y-y~ / y-mean(y)) = R^2