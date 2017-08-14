#檢驗我們的2008-2017年新車Dataset是否有分群的意義

install.packages("clustertend")
install.packages("seriation")
library(factoextra)
library(clustertend)
library(seriation)


#install.packages("clValid")
library(clValid)
# Load the data
data<-read.csv("C:\\Users\\BIG DATA\\Desktop\\analysis\\hclust\\hclust0723_1Original.csv", header=T, sep=",")
#將cc數轉為numeric
data$cc=as.numeric(data$cc)
#只挑cc和modified price為X、Y軸
part_data = data[,c(13,9)]
#放棄遺漏值
part_data=na.omit(part_data)
tail(part_data)

library("ggplot2")
ggplot(part_data, aes(x=cc, y=modified_price)) +
  geom_point() +  # Scatter plot
  geom_density_2d() # Add 2d density estimation

#===========================================================================
#visualize the cluster tendency
library("seriation")
# faithful data: ordered dissimilarity image
df_scaled <- scale(part_data)
df_dist <- dist(df_scaled) 
dissplot(df_dist)
#===========================================================================
library(ggplot2)
library("factoextra")
# Cluster tendency
clustend <- get_clust_tendency(scale(part_data), 100)
# Hopkins statistic
clustend$hopkins_stat
# Customize the plot
clustend$plot + 
  scale_fill_gradient(low = "steelblue", high = "white")