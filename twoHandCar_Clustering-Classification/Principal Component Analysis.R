#主成分分析、針對2008-2017年新車去相關分群分析
#缺點:主成分分析非常難解釋他的參數
#所以最後沒有使用主成分分析

library('psych')
library('graphicsQC')
library('devtools')
library('MASS')
library('RevoUtilsMath')
#open R's thread
#set multiThread = 4
setMKLthreads(4)
#config multThread = 4
getMKLthreads()
memory.size(max=32710)
#load Data
data<-read.csv("yahoo_F.csv", header=T, sep=",")

#require(DMwR)
library("DMwR")
NewData <- knnImputation(data)
tail(NewData)


pca <- prcomp(formula = ~ .,data = data,scale = T,center=T)
#主成分分析(降維):選擇七個變數,並同時正規化
pca
#rotation特徵向量、各個主成分所對應的線性組合係數
#sd(降維後) = 1

#Screen plot
plot(pca,type="line",main="Scree Plot for Car") 
# 用直線連結每個點 # 主標題

# 用藍線標示出特徵值=1的地方
abline(h=1, col="blue") # Kaiser eigenvalue-greater-than-one rule
# 根據Kaiser定理 variance >1的主成分有選取的價值 =>選取前三個主成分

#================================================================================#

#經過主成分分析、選取新的主成分取代原本的選取資料吧!
top6_pca.data <- pca$x[, 1:6]
#選取前六名
top6_pca.data 

#主成份負荷 (主成份和原變數的關係)
#每一個主成份，都是原變數經過線性組合後產生的值。
#而要解釋主成份的話，就需要觀察主成份和原變數之間的關係，
#也就是觀察原變數在線性組合中的係數(特徵向量)，對主成份究竟是正面還是負面、具有多大的影響。
pca$rotation
#取得前三份主成分的特徵向量(標準差)
top6.pca.vector <- pca$rotation[, 1:6]
top6.pca.vector


first.pca <- top6.pca.vector[, 1]   #  第一主成份
second.pca <- top6.pca.vector[, 2]  #  第二主成份
third.pca <- top6.pca.vector[, 3]   #  第三主成份
fourth.pca <- top6.pca.vector[, 4]  #  第四主成份
fifth.pca <- top6.pca.vector[, 5]   #  第五主成份
sixth.pca <- top6.pca.vector[,6]    #  第六主成份



#繪製第一主成分(係數從大到小排序)
#PC1 和價格、max_hp、max_torque、cc數有正相關，感覺跟車子性能有關
first.pca[order(first.pca,decreasing = F)]
dotchart(first.pca[order(first.pca,decreasing = F)],main="loading plot of PC1",col = 'purple',xlab="Variable Loadings")

#繪製第二主成分(係數從大到小排序)
#PC2 和空間、重量、壓縮比、門數有關，感覺跟車子的大小空間有關。
second.pca[order(second.pca,decreasing = F)]
dotchart(second.pca[order(second.pca,decreasing = F)],main="loading plot of PC2",col = "orange",xlab="Variable Loadings")

#繪製第三主成分(係數從大到小排序)
#PC3 和year、油耗量、translevel、壓縮比，感覺和車子性能有關
third.pca[order(third.pca,decreasing = F)]
dotchart(third.pca[order(third.pca,decreasing = F)],main="loading plot of P3",col = "blue",xlab="Variable Loadings")

#繪製第四主成分(係數從大到小排序)
#PC4 和translevel、transmission、brand、years有關(無法理解這裡的關係?)
fourth.pca[order(fourth.pca,decreasing = F)]
dotchart(fourth.pca[order(fourth.pca,decreasing = F)],main="loading plot of P4",col = "red",xlab="Variable Loadings")

#繪製第五主成分(係數從大到小排序)
#和engine_3、transmission、brake、engine_4、engine_2 有關(無法理解這裡的關係?)
fifth.pca[order(fifth.pca,decreasing = F)]
dotchart(fifth.pca[order(fifth.pca,decreasing = F)],main="loading plot of P5",col = "green",xlab="Variable Loadings")

#繪製第六主成分(係數從大到小排序)
#和壓縮比、後懸吊、油耗、手自排、煞車有關
sixth.pca[order(sixth.pca,decreasing = F)]
dotchart(sixth.pca[order(sixth.pca,decreasing = F)],main="loading plot of P6",col = "navy",xlab="Variable Loadings")


# 選取 PC1 和 PC2 繪製主成份負荷圖
biplot(pca, choices=1:2)  

