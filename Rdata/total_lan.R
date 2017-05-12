getwd()
lan_count = read.csv("./Rdata/total_lan.csv" ,header = FALSE)
str(lan_count)

mat = matrix(lan_count$V2,1:length(lan_count$V1), byrow = TRUE)
colnames(mat) = lan_count$V1

#barplot
barplot(mat)
#pie
sum(lan_count$V2)
pct = round(lan_count$V2/sum(lan_count$V2)*100,1)
labels = paste(lan_count$V1,pct,"%")
pie(pct, labels = labels)

#geom_bar()
g_bygrp <- ggplot(lan_count,aes(x=V2,fill=genhlth))
g_bygrp + geom_bar()
par(mfrow=c(1,1))
g_bygrp + geom_bar(position='stack')
