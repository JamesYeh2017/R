# title: 資料探勘技術應用 1:非監督式與監督式學習
# author: Ming-Chang Lee
# email: alan9956@gmail.com
# RWEPA: http://rwepa.blogspot.tw/
# encoding: UTF-8

# 大綱 -----
# 1. 資料探勘跨產業標準流程 (CRISP-DM) 與模型評估
# 2. R在非監督式學習的應用
# 3. R在監督式學習的應用

# 資料探勘跨產業標準流程 (CRISP-DM) 與模型評估 -----
# 視窗化使用者介面 rattle 套件
install.packages("rattle")
library(rattle)
rattle()

# scale 範例
x <- matrix(1:10, ncol = 2)
x
apply(x, 2, mean) # (matrix, 1列2行, ?)  3,8
apply(x, 2, sd)
(x.scale1 <- scale(x, scale=FALSE))  #X-mean(X)
(x.scale <- scale(x)) # -2/1.581139=-1.264911     (X-平均數)/標準差

# cut 範例
cut(iris$Sepal.Length[1:10], breaks=3)
x <- cut(iris$Sepal.Length[1:10], 
         breaks=3, 
         labels=c("A1","A2","A3"))
str(x)

# sample範例
set.seed(168)
sample(5, 3)
sample(5, 10)
sample(5, 10, replace=TRUE) # bootstrap

# 訓練資料與測試資料範例
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
ind
traindata <- iris[ind==1,]
dim(traindata)     #106列5行
head(traindata)

testdata <- iris[ind==2,]
dim(testdata)
head(testdata)

# R在模型評估準則1 混淆矩陣(Confusion matrix)
# p: Positive 陽性
# n: Negative 陰性
actual <- matrix(c("n", "p", "n", "p", "p", "p", "n", "p", "p", "n"), ncol=1)
pred <- matrix(c("n", "n", "n", "p", "p", "p", "p", "n", "p", "n"), ncol=1)
table(pred, actual)
#3/4為n類別預測的正確率

# R在模型評估準則2 ROC曲線(ROC curve)
library(ROCR)   #內建svm&nn類神經網路
data(ROCR.hiv)
attach(ROCR.hiv)
#SVM
pred.svm <- prediction(hiv.svm$predictions, hiv.svm$labels)#預測
perf.svm <- performance(pred.svm, "tpr", "fpr")#混淆矩陣
#nn類神經網路
pred.nn <- prediction(hiv.nn$predictions, hiv.svm$labels)
perf.nn <- performance(pred.nn, "tpr", "fpr")

plot(perf.svm, lty=3, col="red", 
     main="HIV-1 coreceptor-SVM與NN ROC curve")
plot(perf.nn, lty=3, col="blue", add=TRUE)
plot(perf.svm, avg="vertical", lwd=3, col="red", 
     spread.estimate="stderror",plotCI.lwd=2,add=TRUE)
plot(perf.nn, avg="vertical", lwd=3, col="blue", 
     spread.estimate="stderror",plotCI.lwd=2,add=TRUE)
legend(0.8,0.2, c("SVM","NN"),col=c("red","blue"),lwd=3)

# Apriori演算法範例操作 -----
# adult datset: http://archive.ics.uci.edu/ml/datasets/Adult

library(arules)
?Adult
data("Adult")
str(Adult)

data("AdultUCI")#將資料匯入
str(AdultUCI)#了解資料結構
head(AdultUCI)

# find frequent items
# inspect(Adult)[1:3]
itemFrequency(Adult, type="relative")
itemFrequency(Adult, type="absolute")

# Select people with a large income
Adult.largeIncome <- Adult[Adult %in% "income=large"]

# simple plot
itemFrequencyPlot(Adult.largeIncome)

# plot with the averages of the population plotted as a line 
# for first 30 variables/items)
itemFrequencyPlot(Adult.largeIncome[,1:30],
                  population=Adult[,1:30])

# plot lift ratio (frequency in x / frequency in population)
# for items with a support of 20% in the population
itemFrequencyPlot(Adult.largeIncome, 
                  population=Adult, support=0.2, 
                  lift=TRUE, horiz=TRUE)

# mine association rules
rules <- apriori(Adult, parameter=list(supp = 0.5, conf = 0.9, target = "rules"))
#建立關連規則   使用apriori演算法(supp,信賴值,target)
summary(rules)
#
# display results
inspect(rules)

# display top 5 association rules
inspect(rules[1:5])

# 關聯規則的視覺化 -----
library(arulesViz) # 自動載入 arules 套件
data(Groceries) # 9835*169, Groceries{arules}
Groceries
inspect(Groceries[1:3])
summary(Groceries)

# association rule - scatter plot
# apriori algorithm
Groceries.ar <- apriori(Groceries, 
                        parameter=list(support=0.001, confidence=0.5))

# scatter plot
?plot.rules
plot(Groceries.ar)

# quality measures
head(quality(Groceries.ar))

# scatter plot with customerized axes
plot(Groceries.ar, measure=c("support", "lift"), 
     shading="confidence")

# interactive plot
sel <- plot(Groceries.ar, measure=c("support", "lift"), 
            shading="confidence", 
            interactive=TRUE)
sel
inspect(sel)

#----- K-means演算法範例操作 -----
?iris

# 認識 kmeans 函數
?kmeans

irisnew <- iris
irisnew$Species <- NULL

irisnew.kcluster <- kmeans(irisnew, 3)  #資料集 分群個數
irisnew.kcluster     #  50, 38, 62  分群個數

irisnew.kcluster$totss      #整體平方合
irisnew.kcluster$withinss   #各群的組內平方合
irisnew.kcluster$tot.withinss  #越小越好
irisnew.kcluster$betweenss     #越大越好

plot(irisnew[c("Sepal.Length", "Sepal.Width")], 
     col=irisnew.kcluster$cluster) # col: 1黑,2紅,3綠
points(irisnew.kcluster$centers[,c("Sepal.Length", "Sepal.Width")], 
       col=1:3, pch=8, cex=3)        #畫出中心點  pch選擇point的圖案  cex放大

# 階層式演算法範例操作 -----
?hclust
idx <- sample(1:dim(iris)[1], 40)  #母體  樣本數
irisSample <- iris[idx,]
irisSample$Species <- NULL

iris.hc <- hclust(dist(irisSample), method="ave")  #dist計算相鄰矩陣
names(iris.hc)  #了解物件名稱
iris.hc$height  #各物件相關特性

plot(iris.hc , labels=iris$Species[idx])  #

plot(iris.hc , hang = -1, labels=iris$Species[idx])  #hang -1  顯示直排文字對齊
rect.hclust(iris.hc, k=3, border="red")   #3個矩形

# 決策樹 rpart 套件 -----

# source: http://archive.ics.uci.edu/ml/
# Statlog (German Credit Data)

credit <- read.csv("http://web.ydu.edu.tw/~alan9956/rdata/credit.csv")
names(credit)
str(credit)
head(credit, n=3)
#連續型轉成離散型
levels(credit$checkingstatus1) <- c("<= 0 DM","1-200 DM", "> 200 DM", "unknown")
table(credit$checkingstatus1)

levels(credit$savings) <- c("< 100 DM","100 <= saving < 500 DM", "500 <= saving < 1000 DM", "<= 1000 DM", "no saving")
table(credit$savings)

summary(credit$duration)   #利用summary查看col分布
summary(credit$amount)

credit$Default <- as.factor(credit$Default)  #label轉換成因子
levels(credit$Default) <- c("no","yes")      
table(credit$Default)                        #利用table查看data分布

# split into training and test subsets
set.seed(168)
ind <- sample(2, nrow(credit), replace=TRUE, prob=c(0.7, 0.3))#2顆球重複抽
trainData <- credit[ind==1,]
testData <- credit[ind==2,]

# train a decision tree
library(rpart)
myFormula <- Default ~ .
credit_rpart <- rpart(myFormula, data=credit, control=rpart.control(minsplit = 10)) #10個data集才能分割
attributes(credit_rpart)  #屬性名稱

print(credit_rpart)       #cp錯誤比例  xerror交叉驗證
print(credit_rpart$cptable)
plot(credit_rpart)
text(credit_rpart)       #補上值名稱
text(credit_rpart, use.n=TRUE)

# prune the tree
opt <- which.min(credit_rpart$cptable[,"xerror"])
cp.prune <- credit_rpart$cptable[opt, "CP"]
cp.prune    
credit_prune <- prune(credit_rpart, cp=cp.prune)  #prune修剪

print(credit_prune)
plot(credit_prune)
text(credit_prune, use.n=TRUE)

# 條件推論樹 party 套件 -----

str(iris)
set.seed(1234)

ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7,0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]

library(party)
# model formula
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

# build tree

iris_ctree <- ctree(myFormula, data=trainData)

# check the prediction
table(predict(iris_ctree), trainData$Species)

# plot tree
print(iris_ctree)
plot(iris_ctree)
plot(iris_ctree, type="simple")

# predict on test data
testPred <- predict(iris_ctree, newdata = testData)
table(testPred, testData$Species)
# end

# 隨機森林 randomForest 套件 -----

library(randomForest)
# split into training and test subsets
set.seed(168)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]

iris.rf <- randomForest(Species ~ ., data=trainData, ntree=100, proximity=TRUE)
table(predict(iris.rf), trainData$Species)

print(iris.rf)
attributes(iris.rf)

# Error rates with various number of trees
plot(iris.rf)

# Extract variable importance measure
importance(iris.rf) # Gini index
varImpPlot(iris.rf)

# Test the built random forest on test data
irisPred <- predict(iris.rf, newdata=testData)
table(irisPred, testData$Species)
# end
