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
  if(traindata$residual_value[i] > | traindata$residual_value[i] < ){
    traindata$residual_value[i] <- 0
  }else if(traindata$residual_value[i] > | traindata$residual_value[i] < ){
    traindata$residual_value[i] <- 1
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




# 讀取檔案
stud_math = read.csv("E:\\Desk\\r_test\\data\\student-mat.csv", sep=";", header=TRUE) 
summary(stud_math)

# 第33個屬性G3(也就是最終成績)，把此分數分成A,B,C,D,F五個等級
tmp=0 
for(i in 1:395){
  if(stud_math[i,33] > 15)tmp[i] = "A"
  else if(stud_math[i,33] > 13)tmp[i] = "B"
  else if(stud_math[i,33] > 11)tmp[i] = "C"
  else if(stud_math[i,33] > 9)tmp[i] = "D"
  else tmp[i]="F"
}

# 把分好等級之結果存進stud_math
stud_math[,33] = factor(tmp) 
# colnames(stud_math)[colnames(stud_math) == "V34"] <- "Grade"
names(stud_math)[33]<-"Grade"
# 印出Grade屬性中各等級的資料筆數
summary(stud_math$Grade) 
# 取90%為訓練資料
num_train_data = ceiling(0.9*nrow(stud_math)) 
# 印出訓練資料之筆數
num_train_data 

# 隨機選取
stud_math.train = sample(1:nrow(stud_math),num_train_data) 
# 印出選到第幾筆為訓練資料
print(stud_math.train) 

# 一個參數(Grade~.):表示除了Grade屬性之外，其他屬性皆為模型之引數(因為我們要預測Grade呀~)
# 第二個參數(data=stud_math):表示模型中含有變數的一組資料
# 第三個參數(importance=TRUE):是否計算每個模型中各屬性之重要值，資料型態為布林
# 第四個參數(proximity=TRUE):是否計算模型的鄰近矩陣，此參數搭配函數MDSplot()使用，資料型態為布林
# 第五個參數(ntree=500):表示森林中的樹木數量
# 第六個參數(subset=stud_math.train):表示選出的訓練集資料為第幾筆(此參數的資料格式為向量)
# 第七個參數(na.action = na.fail):表示遺漏值之處理，na.fail表示不能出現遺漏值
stud_math.rf=randomForest(Grade~., 
                          data=stud_math,
                          importance=TRUE,
                          proximity=TRUE,
                          ntree=500,
                          subset=stud_math.train, 
                          na.action = na.fail)

print(stud_math.rf)
importance(stud_math.rf)

MDSplot(stud_math.rf, stud_math$Grade)
MDSplot(stud_math.rf, stud_math$Grade, palette = rep(1,5))
MDSplot(stud_math.rf, stud_math$Grade, pch = as.numeric(stud_math$Grade))
MDSplot(stud_math.rf, stud_math$Grade, palette = rep(1,5), pch = as.numeric(stud_math$Grade))
round(importance(stud_math.rf), 2)

# 利用importance()函數，
# 得到MeanDecreaseAccuracy與MeanDecreaseGini，
# 值愈高就表示該屬性對於該模型的判別影響愈大，
# 可以做為後續利用其他演算法建模時刪減屬性的依據。



# 增加mtry參數，隨機選取分隻屬性個數
# 取33-1=32個屬性，即扣掉要預測的G3(Grade)
n = ncol(stud_math) - 1 
model_err_rate = 1
for(i in 1:n){
  result = randomForest(Grade~.,
  data = stud_math,
  mtry = i,
  importance = TRUE,
  ntree = 500)
  model_err_rate[i] = mean(result$err.rate)
  cat("第",i,"個模型:",model_err_rate[i],"\n")
}

mtry_value = which.min(model_err_rate)
cat("mtry參數設定為:", mtry_value)

stud_math.rf = randomForest(Grade~.,
                          data = stud_math,
                          mtry = mtry_value,
                          importance = TRUE,
                          proximity = TRUE,
                          ntree = 500)

print(stud_math.rf)
importance(stud_math.rf)

MDSplot(stud_math.rf, stud_math$Grade)
MDSplot(stud_math.rf, stud_math$Grade, palette = rep(1,5))
MDSplot(stud_math.rf, stud_math$Grade, pch = as.numeric(stud_math$Grade))
MDSplot(stud_math.rf, stud_math$Grade, palette = rep(1,5), pch = as.numeric(stud_math$Grade))
round(importance(stud_math.rf), 2)

# 利用importance()函數，
# 得到MeanDecreaseAccuracy與MeanDecreaseGini，
# 值愈高就表示該屬性對於該模型的判別影響愈大，
# 可以做為後續利用其他演算法建模時刪減屬性的依據。











