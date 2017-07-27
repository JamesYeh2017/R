#當訓練資料集內的數目太少，而變數太多時，分類的效果會變差
#決策樹在分類上屬於固定的路徑，沒辦法像類神經在分類過程有容錯能力，
#決策樹結合類神經網路(Artificial neural network)
#或是隨機森林(Random Forest)作預測

#import library
library(RevoUtilsMath)
#set multiThread = 4
setMKLthreads(4)
#config multThread = 4
getMKLthreads()

#載入library 
library("rpart")
library("rpart.plot")
library("rattle")

#載入AER Package(AER: Applied Econometrics with R)
#install.packages("AER")
library(AER)

#(1)載入creditcard資料集(包含1,319筆觀察測試，共有12個變數)
data(CreditCard)

#假設我們只要以下欄位(card:是否核准卡片、信用貶弱報告數、年齡、收入(美金)、自有住宅狀況、往來年月)
bankcard <- subset(CreditCard, select = c(card, reports, age, income, owner, months))

#將是否核准卡片轉換為0/1數值
bankcard$card <- ifelse(bankcard$card == "yes", 1, 0);

#(2)測試模型
#取得總筆數
n <- nrow(bankcard)
#設定隨機數種子
set.seed(1117)
#將數據順序重新排列
newbankcard <- bankcard[sample(n),]

#取出樣本數的idx
t_idx <- sample(seq_len(n), size = round(0.7 * n))

#訓練資料與測試資料比例: 70%建模，30%驗證
traindata <- newbankcard[t_idx,]
testdata <- newbankcard[ - t_idx,]

#(3)建立決策樹模型      #CART
dtreeM <- rpart(formula = card ~ ., data = traindata, method = "class", control = rpart.control(cp = 0.01))

#(4)用rattle畫出厲害的決策樹(Rx: rxDTree)
fancyRpartPlot(dtreeM)
plot(dtreeM)

#混淆矩陣(confusion matrix)及準確率(Accuracy)觀察模型表現

#(5)預測
result <- predict(dtreeM, newdata = testdata, type = "class")
#(6)建立混淆矩陣(confusion matrix)觀察模型表現
cm <- table(testdata$card, result, dnn = c("實際", "預測"))
cm

#(7)正確率
#計算核準卡正確率
cm[4] / sum(cm[, 2])
#計算拒補件正確率
cm[1] / sum(cm[, 1])

#整體準確率(取出對角/總數)
accuracy <- sum(diag(cm)) / sum(cm)
accuracy                            #與邏輯斯迴歸(82%)差不多


#*****條件推論樹(Conditional Inference Tree)*****
#install.packages("party") 
library(party)

ct <- ctree(Species ~ ., data = iris)
plot(ct, main = "條件推論樹")
table(iris$Species, predict(ct))


#*****隨機森林(Random Forests)*****
#多個決策樹的分類器來作預測
#隨機森林是一個包含多個決策樹的分類器，並且其輸出的類別是由個別樹輸出的類別的眾數而定。 Leo Breiman和Adele Cutler發展出推論出隨機森林的演算法 。而"Random Forests "是他們的商標。這個術語是1995年由貝爾實驗室的Tin Kam Ho所提出的隨機決策森林（random decision forests）而來的。這個方法則是結合Breimans的" Bootstrap aggregating "想法和Ho的" random subspace method " 以建造決策樹的集合。
#簡單的理解就是利用隨機(重新抽樣)的方法種植出許多決策樹，樹的集合就是森林，接著從決策樹們的投票結果中選出票數最多的候選人作為本屆選舉結果
#整體學習（Ensemble learning）可以將數個分類器的預測結果綜合考慮

#載入隨機樹森林package
#install.packages("randomForest")
library(randomForest)
set.seed(1117)

#(2)跑隨機樹森林模型
randomforestM <- randomForest(
  card ~ ., data = traindata, importane = T, proximity = T, do.trace = 100, ntree= 300)
randomforestM

#錯誤率: 利用OOB(Out Of Bag)運算出來的
plot(randomforestM)

#(3)預測
result <- predict(randomforestM, newdata = testdata)     
result_Approved <- ifelse(result > 0.6, 1, 0)             #將預測結果轉成0&1

#(4)建立混淆矩陣(confusion matrix)觀察模型表現
cm <- table(testdata$card, result_Approved, dnn = c("實際", "預測"))
cm

#(5)正確率
#計算核準卡正確率
cm[4] / sum(cm[, 2])

#計算拒補件正確率
cm[1] / sum(cm[, 1])

#整體準確率(取出對角/總數)
accuracy <- sum(diag(cm)) / sum(cm)
accuracy                             #82.3%


