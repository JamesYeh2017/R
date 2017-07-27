getwd()
setwd("./data/")
match_txt = read.table("./match.txt" ,header = FALSE, sep="|")  
match_txt
match_fun = function(match_txt=read.table("./match.txt" ,header = FALSE, sep="|")){
  mat = matrix(rep(-1,length(levels(match_txt[i,1]))^2), nrow=5)
  #return(mat)
  #rownames(mat) = c("A","B","C","D","E")
  #colnames(mat) = c("A","B","C","D","E")
  
  rownames(mat) = levels(match_txt[i,1])
  colnames(mat) = levels(match_txt[i,2])
  #return(mat)
  for (i in 1:nrow(match_txt)){
    mat[match_txt[i,1], match_txt[i,2]] = match_txt[i,3];
    #matrix可以用 idx[1,1];names[A,A] 取值
  }
  return(mat)
}
match_fun()


# 各種機率分配的中央極限定裡
CLT = function(x) {
  op<-par(mfrow=c(2,2)) # 設為 2*2 的四格繪圖版
  hist(x, breaks=50)     # 繪製 x 序列的直方圖 (histogram)。
  m2 <- matrix(x, nrow=2 )  # 將 x 序列分為 2*k 兩個一組的矩陣 m2。
  xbar2 <- apply(m2, 2, mean)   # 取每兩個一組的平均值 (x1+x2)/2 放入 xbar2 中。   col平均
  
  hist(xbar2, breaks=50)     # 繪製 xbar2 序列的直方圖 (histogram)。
  m10 <- matrix(x, nrow=10 )   # 將 x 序列分為 10*k 兩個一組的矩陣 m10。
  xbar10 <- apply(m10, 2, mean) # 取每10個一組的平均值 (x1+..+x10)/10 放入 xbar10 中。
  
  hist(xbar10, breaks=50)    # 繪製 xbar10 序列的直方圖 (histogram)。
  m20 <- matrix(x, nrow=20 )   # 將 x 序列分為 25*k 兩個一組的矩陣 m25。
  xbar20 <- apply(m20, 2, mean) # 取每20個一組的平均值 (x1+..+x20)/20 放入 xbar20 中。
  hist(xbar20, breaks=50)    # 繪製 xbar20 序列的直方圖 (histogram)。
}

CLT(rbinom(n=100000, size = 20, prob = 0.1)) # 用參數為 n=20, p=0.5 的二項分布驗證中央極限定理。
CLT(runif(n=100000,min = 0,max = 1)) # 用參數為 a=0, b=1 的均等分布驗證中央極限定理。
CLT(rpois(n=100000, lambda = 4)) # 用參數為 lambda=4 的布瓦松分布驗證中央極限定理。
CLT(rgeom(n=100000, prob = 0.7)) # 用參數為 p=0.5 的幾何分布驗證中央極限定理。
