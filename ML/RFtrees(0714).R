install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("AER")
install.packages("randomForest")
library(rpart)
library(rpart.plot)
library(rattle)
library(AER)
library(randomForest)

# 
# randomForest(formula, data=NULL, ..., subset, na.action=na.fail)
#formula : (Y ~.)表示除了Y屬性之外，其他屬性皆為模型之引數
#data  : 表示模型中含有變數的一組資料
#subset : 表示選出的訓練集資料為第幾筆(此參數的資料格式為向量)
#na.action : 表示遺漏值之處理，na.fail表示不能出現遺漏值
#
# 
# randomForest(x, y=NULL, xtest=NULL, ytest=NULL, ntree=500,
#             mtry=if (!is.null(y) && !is.factor(y))
#               max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))),
#             replace=TRUE, classwt=NULL, cutoff, strata,
#             sampsize = if (replace) nrow(x) else ceiling(.632*nrow(x)),
#             nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
#             maxnodes = NULL,
#             importance=FALSE, localImp=FALSE, nPerm=1,
#             proximity, oob.prox=proximity,
#             norm.votes=TRUE, do.trace=FALSE,
#             keep.forest=!is.null(y) && is.null(xtest), corr.bias=FALSE,
#             keep.inbag=FALSE, ...)
# x : 訓練的解釋變數
# y : 訓練反應變數
# xtest : 測試的解釋變數
# ytest : 測試的反應變數
# ntree : 表示森林中的樹木數量
# mtry = if (!is.null(y) && !is.factor(y)) 
#         max(floor(ncol(x)/3), 1) 
#         else floor(sqrt(ncol(x))) : 解釋變數候選數目
# replace=TRUE : 隨機樣本是否取出放回
# classwt=NULL : 
# cutoff : 獲選比率
# strata : 隨機分層抽樣
# sampsize = if (replace) nrow(x) else ceiling(.632*nrow(x)) : 訓練集百分比
# nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1 : 節點內最少樣本數
# maxnodes = NULL : 節點內最多樣本數
# importance=FALSE : 是否計算每個模型中各屬性之重要值，資料型態為布林
# localImp=FALSE : 是否存新計算
# nPerm=1 : 重新排列重要性
# proximity : 是否計算模型的鄰近矩陣，此參數搭配函數MDSplot()使用，資料型態為布林
# oob.prox=proximity : 是否依據out-of-bag計算鄰近度
# norm.votes=TRUE : 是否顯示節點內資料數
# do.trace=FALSE : 是否顯示詳細結果
# keep.forest=!is.null(y) && is.null(xtest) : 
# corr.bias=FALSE : 是否執行迴歸的偏異校正
# keep.inbag=FALSE : 

for(i in 1:ncol(traindata)){
    if(is.na(traindata$residual_value[i])){
      print(i)
    }
}


library(RevoUtilsMath)
#set multiThread = 4
setMKLthreads(4)
#config multThread = 4
getMKLthreads()
memory.size(max=32710)

library(readxl)
data<- read_excel("C:/Users/BIG DATA/Desktop/DL/0722.xlsx")

#install.packages("DMwR")
library("DMwR")
NewData <- knnImputation(data)   #knnInputation
tail(data)

car <- subset(data, select = c(brand,years,usingyears,model,type,tag_price,cc,engine,gasoline,max_hp,compression,transmission,trans_level,front_suspen,
                                  back_suspen,brake,tire,kind,doors,price,source,color,mileage,location,posttime,type_number,certificate,residual_value))

# 測試模型
# 取得總筆數
n <- nrow(car)
n
# 設定隨機數種子
set.seed(101)
# 將數據順序重新排列
car <- car[sample(n),]

# 取出樣本數的idx
t_idx <- sample(seq_len(n), size = round(0.7 * n))

# 訓練資料與測試資料比例: 70%建模，30%驗證
traindata <- car[t_idx,]
testdata <- car[ - t_idx,]

# (2)跑隨機樹森林模型
# importance=TRUE:是否計算每個模型中各屬性之重要值，資料型態為布林
# proximity=TRUE:是否計算模型的鄰近矩陣，此參數搭配函數MDSplot()使用，資料型態為布林
# ntree=500:表示森林中的樹木數量
x = subset(car, select = c(brand,years,usingyears,model,type,tag_price,cc,engine,gasoline,
                            max_hp,compression,transmission,trans_level,front_suspen,
                            back_suspen,brake,tire,kind,doors,price,source,color,mileage,
                            location,posttime,type_number,certificate))

car$brand <- as.factor(car$brand)
car$model <- as.factor(car$model)
car$type <- as.factor(car$type)
car$engine <- as.factor(car$engine)
car$gasoline <- as.factor(car$gasoline)
car$max_hp <- as.factor(car$max_hp)
car$transmission <- as.factor(car$transmission)
car$trans_level <- as.factor(car$trans_level)
car$front_suspen <- as.factor(car$front_suspen)
car$back_suspen <- as.factor(car$back_suspen)
car$brake <- as.factor(car$brake)
car$tire <- as.factor(car$tire)
car$kind <- as.factor(car$kind)
car$source <- as.factor(car$source)
car$color <- as.factor(car$color)
car$location <- as.factor(car$location)
car$posttime <- as.factor(car$posttime)

# randomforestM <- randomForest(x = x, 
#                               y=car$residual_value,
#                               replace=TRUE,
#                               importane = T, 
#                               proximity = T, 
#                               do.trace = 100, 
#                               ntree = 500)
f <- residual_value~brand+years+usingyears+model+type+tag_price+cc+engine+gasoline+max_hp+compression+transmission+trans_level+front_suspen+back_suspen+brake+tire+kind+doors+price+source+color+mileage+location+posttime+type_number+certificate
randomforestM <- randomForest(formula = f,
                              data = traindata,
                              replace=TRUE,
                              importane = T, 
                              proximity = T, 
                              do.trace = 100, 
                              ntree = 500,
                              na.action=na.omit)
randomforestM

for(i in 1:nrow(traindata)){
  if(traindata$residual_value[i] > 0| traindata$residual_value[i] < 5){
    traindata$residual_value[i] <- 0
  }else if(traindata$residual_value[i] > 5| traindata$residual_value[i] < 10){
    traindata$residual_value[i] <- 1
  }else if(traindata$residual_value[i] > 10| traindata$residual_value[i] < 15){
    traindata$residual_value[i] <- 2
  }else if(traindata$residual_value[i] > 15| traindata$residual_value[i] < 20){
    traindata$residual_value[i] <- 3
  }else if(traindata$residual_value[i] > 20| traindata$residual_value[i] < 25){
    traindata$residual_value[i] <- 4
  }else if(traindata$residual_value[i] > 25| traindata$residual_value[i] < 30){
    traindata$residual_value[i] <- 5
  }else if(traindata$residual_value[i] > 30| traindata$residual_value[i] < 35){
    traindata$residual_value[i] <- 6
  }else if(traindata$residual_value[i] > 35| traindata$residual_value[i] < 40){
    traindata$residual_value[i] <- 7
  }else if(traindata$residual_value[i] > 40| traindata$residual_value[i] < 45){
    traindata$residual_value[i] <- 8
  }else if(traindata$residual_value[i] > 45| traindata$residual_value[i] < 50){
    traindata$residual_value[i] <- 9
  }else if(traindata$residual_value[i] > 50| traindata$residual_value[i] < 55){
    traindata$residual_value[i] <- 10
  }else if(traindata$residual_value[i] > 55| traindata$residual_value[i] < 60){
    traindata$residual_value[i] <- 11
  }else if(traindata$residual_value[i] > 60| traindata$residual_value[i] < 65){
    traindata$residual_value[i] <- 12
  }else if(traindata$residual_value[i] > 65| traindata$residual_value[i] < 70){
    traindata$residual_value[i] <- 13
  }else if(traindata$residual_value[i] > 70| traindata$residual_value[i] < 75){
    traindata$residual_value[i] <- 14
  }else if(traindata$residual_value[i] > 75| traindata$residual_value[i] < 80){
    traindata$residual_value[i] <- 15
  }else if(traindata$residual_value[i] > 80| traindata$residual_value[i] < 85){
    traindata$residual_value[i] <- 16
  }else if(traindata$residual_value[i] > 85| traindata$residual_value[i] < 90){
    traindata$residual_value[i] <- 17
  }else if(traindata$residual_value[i] > 90| traindata$residual_value[i] < 95){
    traindata$residual_value[i] <- 18
  }else if(traindata$residual_value[i] > 95| traindata$residual_value[i] < 100){
    traindata$residual_value[i] <- 19
  }
}

# 錯誤率 : 利用OOB(Out Of Bag)運算出來的
plot(randomforestM)

# 衡量每一個變數對Y值的重要性，取到小數點第二位
round(importance(randomforestM), 2)

# (3)預測
result <- predict(randomforestM, newdata = testdata)
result_Approved <- ifelse(result > 0.6, 1, 0)

# (4)建立混淆矩陣(confusion matrix)觀察模型表現
cm <- table(testdata$card, result_Approved, dnn = c("實際", "預測"))
cm

# (5)正確率
# 計算核準卡正確率
cm[4] / sum(cm[, 2])

# 計算拒補件正確率
cm[1] / sum(cm[, 1])

# 整體準確率(取出對角/總數)
accuracy <- sum(diag(cm)) / sum(cm)
accuracy

