#*****羅吉斯模型及預測*****

install.packages("AER")
library(AER)

###(1)載入creditcard資料集(包含1,319筆觀察測試，共有12個變數)
data(CreditCard)
#觀察資料欄位
head(CreditCard)

#假設我們只要以下欄位(card:是否核准卡片、信用貶弱報告數、年齡、收入(美金)、自有住宅狀況、往來時間)
bankcard <- subset(CreditCard, select = c(card, reports, age, income, owner,months))

#將是否核准卡片轉換為0/1數值
bankcard$card <- ifelse(bankcard$card == "yes", 1, 0);

###(2)跑羅吉斯模型
card_glm <- glm(formula = card ~ ., family = "binomial", data = bankcard)

###(3)單筆資料預測
#30歲無信用不良紀錄，收入10萬美金，有自有住宅
new <- data.frame(reports = 0, age = 30, income = 10, owner = "yes", months = 50)
result <- predict(card_glm, newdata = new, type = "response")
result

#30歲有信用不良紀錄，收入5萬美金，無自有住宅
new <- data.frame(reports = 1, age = 30, income = 5, owner = "no", months = 50)
result <- predict(card_glm, newdata = new, type = "response")
result


#*****混淆矩陣(confusion matrix)及準確度(Accuracy)*****

###(4)測試模型
#將資料分為訓練與測試組

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

# 重新建立羅吉斯迴歸模型
card_glm2 <- glm(formula = card ~ ., family = "binomial", data = traindata)#建模
result <- predict(card_glm2, newdata = testdata, type = "response")#模型預測結果

#(5)建立混淆矩陣(confusion matrix)觀察模型表現

#r假設我們認定核準率60%以上才視為核卡，其餘是為拒件或補件
result_Approved <- ifelse(result > 0.6, 1, 0)      #result  is  continue
                                                 #but  result  is T/F
cm <- table(testdata$card, result_Approved, dnn = c("實際", "預測"))
cm             #confusion matrix

#(6)準確率
#計算核準卡正確率
cm[4] / sum(cm[, 2])   #84%
#計算拒補件正確率
cm[1] / sum(cm[, 1])   #71%

#整體準確率(對角線元素總和/所有觀察值總和)
accuracy <- sum(diag(cm)) / sum(cm)        #82%
accuracy                

#TPR = TP/(TP+FN) 命中率   cm[4] / sum(cm[, 2])  預測核卡(col)  實際核卡
#FPR = FP/(FP+TN) 假警報率 cm[2] / sum(cm[, 1])  預測沒過(col)  實際核卡

#畫ROC曲線
library("ROCR")
pred <- prediction(result, testdata$card)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#計算AUC
auc <- performance(pred, "auc")

#畫圖
plot(perf, col = rainbow(7), main = "ROC curve", xlab = "Specificity(FPR)", ylab = "Sensitivity(TPR)")
#AUC = 0.5
abline(0, 1)
#實際AUC值
text(0.5, 0.5, as.character(auc@y.values[[1]]))

#ROC曲線下方的面積稱為AUC(Area under the Curve of ROC)
#中間的直線就是AUC=0.5，模型的AUC值越高，正確率越高


