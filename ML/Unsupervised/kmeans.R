# (1)直接用亞種結果畫分佈(花瓣的長寬) 
plot(formula = Petal.Length ~ Petal.Width, data = iris, col = iris$Species)


#(2)分群畫圖 
# 建立一個分群模型 
data <- iris[-5] #去除第5個資料行    
##分3群，nstart=10 defaut執行10次  收斂資料區 
km <- kmeans(data, centers = 3, nstart = 10)

#跑分群之後畫分佈 
plot(formula = Petal.Length ~ Petal.Width, data = data, col = km$cluster, main = "將鳶尾花做分群", xlab = "花瓣寬度", ylab = "花瓣長度")

ggplot(data, aes(x = Petal.Length, y = Petal.Width)) +
geom_point(aes(color = factor(km$cluster))) +
stat_density2d(aes(color = factor(km$cluster)))

(WSS <- km$tot.withinss)
(BSS <- km$betweenss)
(TSS <- BSS + WSS)
(ratio <- WSS / TSS)

library(ggplot2)
klist <- seq(1:10)
knnFunction <- function(x) {
  kms <- kmeans(data, centers = x, nstart = 1)
  ratio <- kms$tot.withinss / (kms$tot.withinss + kms$betweenss)
}
ratios <- sapply(klist, knnFunction)

# k value與準確度視覺化
df <- data.frame(
  kv = klist, KMratio = accuracies)

ggplot(df, aes(x = kv, y = KMratio, label = kv, color = KMratio)) +
geom_point(size = 5) + geom_text(vjust = 2)

