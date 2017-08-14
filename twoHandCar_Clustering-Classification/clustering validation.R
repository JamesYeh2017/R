#which model is the best to cluster ?
#how many groups are the best to cluster?
# install.packages("kohonen")
#install.packages("clValid")
library(clValid)
library(kohonen)
library(class)
library(MASS)
# Load the data
data<-read.csv("C:\\Users\\BIG DATA\\Desktop\\analysis\\hclust\\hclust0723_1Original.csv", header=T, sep=",")

#require(DMwR)
library(grid)
library("DMwR")
##factor轉成numeric
data$max_torque_N=as.numeric(data$max_torque_N)
#選擇欄位
part_data = data[,c(2,4,9,11,13,15,17,27,30,35,43)]
#補值
NewData <- knnImputation(part_data)
#正規化
NewData =scale(NewData[,-1])
tail(NewData)

#================================================

# Compute clValid
clmethods <- c("hierarchical", "kmeans", "diana", "pam")
intern <- clValid(NewData, nClust = 2:6,
                  clMethods = clmethods, validation = "internal")
# Summary
summary(intern)

plot(intern)

#================================================
clmethods <- c("hierarchical", "kmeans", "diana", "pam")
stab <- clValid(NewData, nClust = 2:6, clMethods = clmethods,
                validation = "stability")
# Display only optimal Scores
optimalScores(stab)
summary(stab)
plot(stab)

#================================================
clmethods <- c("hierarchical","kmeans","pam")
stab <- clValid(NewData, nClust = 2:6, clMethods = clmethods,
                validation = "biological",annotation = fc)
# Display only optimal Scores
optimalScores(stab)




