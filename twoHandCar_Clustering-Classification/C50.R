#透過已經分群玩的2016-2017Data、再對2008-2015年的車款進行預測(群)
# install.packages("C50")
library(C50)
library("grid")
library("DMwR")
setwd("C:\\Users\\BIG DATA\\Desktop\\analysis\\OriginalData")
OriginalData17<-read.csv("ClusteredData_2017_0725.csv", header=T, sep=",")
#選擇欄位
part_data17 = OriginalData17[,c(2,4,9,11,13,15,17,27,30,35,43,49)]
#factor轉成numeric
part_data17$max_torque_N=as.numeric(part_data17$max_torque_N)
#補值
car17 <- knnImputation(part_data17)
#分類的結果(y)必須轉成factor型態
car17$carGroup = as.factor(car17$carGroup)

set.seed(200)
#7成訓練集、3成測試集
ind<-sample(1:2, size=nrow(car17), replace=T, prob=c(0.7, 0.3))
trainset=car17[ind==1,]
testset=car17[ind==2,]

#C5.0設定末枝節點至少20個樣本數、除了carGroup，其它當作參數跑決策樹
c=C5.0Control(minCases = 20)
c50.model = C5.0(carGroup ~ . , data=trainset,control = c)
summary(c50.model)
plot(c50.model,subtree=1)

#驗證
c50.predict = predict(c50.model,testset)
table(c50.predict, testset$carGroup)

#混淆矩陣
library('caret')
library('e1071')
# install.packages("e1071")
confusionMatrix(table(c50.predict, testset$carGroup,row.names()))


#用來預測的2008-2015車款data
OriginalData08<-read.csv("C:\\Users\\BIG DATA\\Desktop\\analysis\\randomFroest\\randomforest_Original0726_08.csv", header=T, sep=",")
part_data08 = OriginalData08[,c(2,4,9,11,13,15,17,27,30,35,43)]
part_data08$max_torque_N=as.numeric(part_data08$max_torque_N)
car08 <- knnImputation(part_data08)


#predict
c50.predict08 = predict(c50.model,car08)

#write
tag_table08=cbind(OriginalData08,c50.predict08)
write.csv(tag_table08,file="C:\\Users\\BIG DATA\\Desktop\\predict08.csv")


