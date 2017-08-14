#K-Medoid

library(cluster)
library(RevoUtilsMath)
#set multiThread = 4
setMKLthreads(4)
#config multThread = 4
getMKLthreads()
memory.size(max=32710)

data<-read.csv("C:\\Users\\BIG DATA\\Desktop\\analysis\\hclust\\hclust0723_1Original.csv", header=T, sep=",")

#require(DMwR)
library(grid)
library("DMwR")
#factor轉換成數值
data$max_torque_N=as.numeric(data$max_torque_N)
# NewData = data[,c(2,4,6,8,9,11,13,15,17,18,21,23,27,30,35,36,43)]
#選擇欄位
part_data = data[,c(2,4,9,11,13,15,17,27,30,35,43)]
#補值
NewData <- knnImputation(part_data)
#正規化
NewData =scale(NewData[,-1])
tail(NewData)

# pam = Partitioning Around Medoids
kmedoid.cluster <- pam(NewData, k=6) 

# 群內的變異數
kmedoid.cluster$objective
kmedoid.cluster$clustering

#write to New_CSV
kmedoid_Data=cbind(data,kmedoid.cluster$clusterin)
write.csv(kmedoid_Data, file = "C:\\Users\\BIG DATA\\Desktop\\Kmedoid0724_1.csv")
