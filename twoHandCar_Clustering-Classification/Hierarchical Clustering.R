#階層式集群分析法Hierarchical Clustering
#其中聚合式因為"群間"的距離就有"ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "centroid".
#              "群內"的距離就有"歐式"、"曼哈頓"
#所以這種排列組合有很多可以嘗試


library(ggplot2)
#open R's thread
library(factoextra)
library(RevoUtilsMath)
#set multiThread = 4
setMKLthreads(4)
#config multThread = 4
getMKLthreads()
memory.size(max=32710)

#Load 2008-2017 year Car Data
Original_data<-read.csv("C:\\Users\\BIG DATA\\Desktop\\analysis\\hclust\\hclust0723_1Original.csv", header=T, sep=",")


#require(DMwR)
library("DMwR")
library("grid")
#factor轉為numeric
Original_data$max_torque_N=as.numeric(Original_data$max_torque_N)
# str(Original_data)

# KNN補值法
NewData <- knnImputation(New)
# choose column
New=Original_data[,c(4,9,13,15,17,35,43)]
#數值變數作正規化
NewData =scale(NewData[,-1])
?scale


#聚合式 群間距離:沃德法
?hclust
hc0=hclust(dist(NewData), method="ward.D2")


#選擇5~6個cluster
g24 <- cutree(hc0, k = c(5,6))
hc0_new <- cutree(hc0, k = c(5,6))
#查看5、6群樣本數落點
table(grp5 = g24[,"5"], gr6 = g24[,"6"])
plot(hc0_new,cex=0.7)
plot(hc0_new, ask = FALSE, which.plots = NULL)

#================視覺化==========================

library("factoextra")
res.hc <- eclust(NewData, "hclust", k = 6, graph = FALSE) 
# Visualize
fviz_dend(res.hc, rect = TRUE, main = "階層式集群分析法",show_labels = FALSE)
fviz_silhouette(res.hc)



#==============================================

#聚合式 群間距離:單一連結法
hc1=hclust(dist(data), method="single")
g24 <- cutree(hc1, k = c(5,6))
table(grp5 = g24[,"5"], gr6 = g24[,"6"])
plot(hc1_new, cex=0.7)
text(hc1_new)


#聚合式 群間距離:完全聯結法
hc2=hclust(dist(data), method="complete")
hc2_new=cutree(hc2, k = 6)
g24 <- cutree(hc2, k = c(5,6))
table(grp5 = g24[,"5"], gr6 = g24[,"6"])
plot(hc2_new, cex=0.7)


#聚合式 群間距離:平均聯結法
hc3=hclust(dist(data), method="average")
hc3_new=cutree(hc3, k = 6)
g24 <- cutree(hc3, k = c(5,6))
table(grp5 = g24[,"5"], gr6 = g24[,"6"])
plot(hc3_new, cex=0.7)

#聚合式 群間距離:相似聯結法
hc4=hclust(dist(data), method="mcquitty")
g24 <- cutree(hc4, k = c(5,6))
table(grp5 = g24[,"5"], gr6 = g24[,"6"])
hc4_new=cutree(hc4, k = 6)
plot(hc4_new, cex=0.7)

#聚合式 群間距離:median
hc5=hclust(dist(data), method="median")
g24 <- cutree(hc5, k = c(5,6))
table(grp5 = g24[,"5"], gr6 = g24[,"6"])
hc5_new=cutree(hc5, k = 6)
plot(hc5_new, cex=0.7)

#聚合式 群間距離:中心點連結法
hc6=hclust(dist(data), method="centroid")
g24 <- cutree(hc6, k = c(5,6))
table(grp5 = g24[,"5"], gr6 = g24[,"6"])
hc6_new=cutree(hc6, k = 6)
plot(hc6_new, cex=0.7)


#==================================================
#cutree(事後修剪)
fit =cutree(hc0, k =4)
fit
table(fit)
plot(hc0)
rect.hclust(hc0, k =4, border="red")
rect.hclust(hc0, k =6, border="blue")
rect.hclust(hc0, k = 6 , which =4, border="purple")
?rect.hclust
#==================================================
#分裂式 
install.packages('cluster')
library(cluster)
?diana
#根據歐式距離分裂
dv1 =diana(NewData, metric ="euclidean")
summary(dv1)
plot(dv1)

#根據曼哈頓距離分裂
dv2 =diana(NewData, metric ="manhattan")
summary(dv2)
plot(dv2)

