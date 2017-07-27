#P30 example : find MaxPrice In RangeDate
getwd()
tw2330 = read.csv("data/2330.csv", header=TRUE)
tw2330$Date = as.Date(tw2330$Date)   #先將Date的type轉成Datete
str(tw2330)
max(tw2330$Close)

#new data.frame  混合型資料型態
stock2 = tw2330[(tw2330$Date >= '2014-03-01' & tw2330$Date <'2014-08-31'),]  
stock2$Close
summary(stock2$Close)   #取摘要

#new data.frame
ordered_stock2 = stock2[order(stock2$Close, decreasing = T),]  
class(ordered_stock2)
ordered_stock2[1,]                       #find max price 
ordered_stock2[nrow(ordered_stock2),]    #find min price 
nrow(ordered_stock2)
ordered_stock2[1,"Close"]
ordered_stock2[nrow(ordered_stock2),"Close"]
