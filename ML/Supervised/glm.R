library(dplyr)
ddd <- read.csv('C:\\Users\\BIG DATA\\Desktop\\DL\\0722.csv')
#glm(formula, data, family = 'gamma', link = "power")

currentPrice <- ddd$tag_price * ddd$residual_value / 100
ddd <- cbind(ddd, currentPrice)
aaa <- subset(ddd, select = c(usingyears, tag_price, mileage, residual_value, currentPrice, logP))
cor(aaa)

ford <- as.data.frame(filter(ddd, brand == 'FORD'))
summary(ford$tag_price)

mean(ford$tag_price) / sd(ford$tag_price)  

plot(ford$tag_price)
hist(ford$tag_price)

logP <- abs(log(ddd$currentPrice/ddd$tag_price))
ddd <- cbind(ddd, logP)

formu <- logP~usingyears 
asd <- glm(formula = formu, data = ddd, family = 'Gamma'(link = 'inverse'))

formu2 <- logP~usingyears + mileage
asd2 <- glm(formula = formu2, data = ddd, family = 'Gamma'(link = 'inverse'))
summary(asd2)

formu3 <- logP~log(usingyears) + mileage
asd3 <- glm(formula = formu3, data = ddd, family = 'Gamma'(link = 'inverse'))
summary(asd3)

formu4 <- logP~usingyears + log(mileage)
asd4 <- glm(formula = formu4, data = ddd, family = 'Gamma'(link = 'inverse'))
summary(asd4)

formu5 <- residual_value~log(usingyears) + mileage
asd5 <- glm(formula = formu5, data = ddd, family = 'Gamma'(link = 'inverse'))
summary(asd5)
