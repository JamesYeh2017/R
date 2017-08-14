#根據2008-2017年新車資訊中，用K-means、K-medoids下，找到最適分群數

#open R's thread
library(RevoUtilsMath)
#set multiThread = 4
setMKLthreads(4)
#config multThread = 4
getMKLthreads()
memory.size(max=32710)
#Load Data
data<-read.csv("C:\\Users\\BIG DATA\\Desktop\\analysis\\hclust\\hclust0723_1Original.csv", header=T, sep=",")

#require(DMwR)
library(grid)
library("DMwR")
data$max_torque_N=as.numeric(data$max_torque_N)
# NewData = data[,c(2,4,6,8,9,11,13,15,17,18,21,23,27,30,35,36,43)]
part_data = data[,c(2,4,9,11,13,15,17,27,30,35,43)]
NewData <- knnImputation(part_data)
NewData =scale(NewData[,-1])
tail(NewData)

#Optimal Number Clusters
library(factoextra)
library(ggplot2)
#Elbow Method

#用Average Silhouette Method找K-means最適分群數
fviz_nbclust(NewData, FUNcluster = kmeans, method = "silhouette", k.max = 12) +
  labs(title="FUNcluster = kmeans, method = 'silhouette'") +geom_vline(xintercept = 2, linetype = 2)
#用WSS找K-means最適分群數
fviz_nbclust(NewData, FUNcluster = kmeans, method = "wss", k.max = 12) +
  labs(title="FUNcluster = kmeans") +geom_vline(xintercept = 2, linetype = 2)
#用WSS找K-medoids最適分群數
fviz_nbclust(NewData, FUNcluster = pam, method = "wss", k.max = 12) +
  labs(title="FUNcluster = pam") +geom_vline(xintercept = 2, linetype = 2)


#======================================================================================
#K-means
library("cluster")
set.seed(22)
fit =kmeans(NewData, centers=6)

#plot
barplot(t(fit$centers), beside =TRUE,xlab="cluster", ylab="value")
clusplot(NewData,fit$cluster,colot = T,shadow = T)
plot(fit$cluster,NewData$tag_price,ask = FALSE, which.plots = NULL)
fit$cluster

#write
NewData=cbind(data,fit$cluster)
write.table(NewData, file = "C:\\Users\\BIG DATA\\Desktop\\K-means0723_1.csv", sep = ",")
