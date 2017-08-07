library(e1071)

url = "https://storage.googleapis.com/2017_ithome_ironman/data/kaggle_titanic_train.csv"
titanic_train <- read.csv(url)
titanic_train$Survived <- factor(titanic_train$Survived)

# 將 Age 遺漏值以 median 填補
age_median <- median(titanic_train$Age, na.rm = TRUE)
new_Age <- ifelse(is.na(titanic_train$Age), age_median, titanic_train$Age)
titanic_train$Age <- new_Age

# 切分訓練與測試資料
n <- nrow(titanic_train)
shuffled_titanic <- titanic_train[sample(n), ]
train_indices <- 1:round(0.7 * n)
train_titanic <- shuffled_titanic[train_indices, ]
test_indices <- (round(0.7 * n) + 1):n
test_titanic <- shuffled_titanic[test_indices, ]

# 建立模型
svm_fit <- svm(Survived ~ Pclass + Age + Sex, data = train_titanic)

# 預測
test_titanic_predicted <- predict(svm_fit, test_titanic)

# 績效
conf_matrix <- table(test_titanic_predicted, test_titanic$Survived)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy
