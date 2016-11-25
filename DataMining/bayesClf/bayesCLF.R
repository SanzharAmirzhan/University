dataAll <- read.csv("~/Desktop/University/DataMining/Labs/Lab9/iris.txt", header=FALSE)
labels = unique(dataAll[5])

pci <- vector(mode = "list")
mci <- vector(mode = "list")
vci <- vector(mode = "list")

naiveBayes <- function(){
  for(i in 1:dim(labels)[1]){
    nr = 0
    for(j in 1:dim(dataAll)[1]){
      if(dataAll[j,5] == labels[i,1]){
        nr <- nr + 1
      }
    }
    
    pci[[toString(labels[i,1])]] <<- nr/dim(dataAll)[1]
    
    di <- matrix(0L, nrow = nr, ncol = (dim(dataAll)[2]-1))
    ind <- 1
    for(j in 1:dim(dataAll)[1]){
      if(dataAll[j,5] == labels[i,1]){
        di[ind,] = as.matrix(dataAll[j,1:4])
        ind <- ind + 1
      }
    }
    rm(ind)
    
    curmean <- colMeans(di)
    mci[[toString(labels[i,1])]] <<- curmean
    
    onesMat <- matrix(1L, nrow = nr, ncol = 1)
    zi <- di - onesMat%*%t(as.matrix(curmean))
    
    curvar <- c()
    for(j in 1:(dim(dataAll)[2]-1)){
      curvar[length(curvar) + 1] = (1/nr) * (t(zi[,j])%*%zi[,j])
    }
    curvar <- as.matrix(curvar)
    curvar <- t(curvar)
    vci[[toString(labels[i,1])]] <<- t(curvar)
  }
}

getClass <- function(x){
  ans = labels[1,1]
  ansProb = -1
  for(i in 1:dim(labels)[1]){
    pr <- pci[[toString(labels[i,1])]]
    for(j in 1:(dim(dataAll)[2]-1)){
      pr <- pr * dnorm(as.numeric(x[j]), mci[[toString(labels[i,1])]][j], sqrt(vci[[toString(labels[i,1])]][j]))
    }
    if(ansProb < pr){
      ansProb <- pr
      ans = labels[i,1]
    }
  }
  return(ans)
}

test <- function(){
  cnt <- 0
  for(i in 1:dim(dataAll)[1]){
    x <- c(dataAll[i,1:4])
    if(getClass(x) == dataAll[i,5]){
      cnt <- cnt + 1
    }
  }
  print(cnt/dim(dataAll)[1])
}

naiveBayes()
test()