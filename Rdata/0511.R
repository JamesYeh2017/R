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
