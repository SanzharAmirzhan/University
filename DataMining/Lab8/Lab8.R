dataAll <- read.csv("~/Desktop/University/DataMining/Labs/Lab8/iris.txt", header=FALSE)
dataAll = dataAll[1]

attSet <- c()
rx <- vector(mode = "list")
rxInd <- vector(mode = "list")

eps <- 0.0001
epsF <- 0.08
h <- 0.22

denclue <- function(){
  for(i in 1:dim(dataAll)[1]){
    tmpX <- findAttractor(dataAll[i,1], h, eps)
    tmpDenFun <- denFunction(tmpX, h)
    cat(sprintf("data - \"%f\" \"%f\"\n\n", dataAll[i,1], tmpX))
    if(tmpDenFun >= epsF){
      tmpX <- round(tmpX, digits = 2)
      attSet <<- union(attSet, tmpX)
      rx[[toString(tmpX)]] <<- union(rx[[toString(tmpX)]], dataAll[i,1])
      rxInd[[toString(tmpX)]] <<- union(rxInd[[toString(tmpX)]], i)
    }
  }
}

findAttractor <- function(x, h, eps){
  t <- 1
  ind <- 1
  xt <- c()
  xt[ind] <- x
  
  repeat{
    summ1 <- 0
    summ2 <- 0
    for(i in 1:dim(dataAll)[1]){
      summ1 <- summ1 + denStandNorm(xt[ind] - dataAll[i,1], h)*dataAll[i,1]
      summ2 <- summ2 + denStandNorm(xt[ind] - dataAll[i,1], h)
    }
    xt[ind+1] <- summ1/summ2
    t <- t + 1
    ind <- ind + 1
    if(abs(xt[ind] - xt[ind-1]) <= eps){
      break
    }
  }
  ans <- xt[t]
  rm(xt)
  return(ans)
}

denStandNorm <- function(x, h){
  return((1/sqrt(2*pi))*exp(-((x^2)/(2*h^2))))
}

denFunction <- function(x, h){
  summ <- 0
  for(i in 1:dim(dataAll)[1]){
    summ <- summ + denStandNorm(x - dataAll[i,1], h)
  }
  summ <- summ/(dim(dataAll)[1] * h)
  return(summ)
}

plotDensity <- function(){
  some <- unlist(dataAll)
  d <- density(some, kernel = "gaussian", bw = h)
  plot(d)
  polygon(d, col="red", border="blue")
}

plotClusters <- function(){
  colVec <- c("red", "green", "black", "orange", "gray")
  vecArr <- c()
  vecX <- c()
  ind <- 1
  curcol <- 1
  for(i in 1:length(rxInd)){
    for(j in 1:length(rxInd[[i]])){
      print(rxInd[[i]][j])
      vecX[ind] = dataAll[rxInd[[i]][j],1]
      vecArr[ind] = curcol 
      ind = ind + 1
      cat(sprintf("\"%f\" \"%f\"\n", ind, curcol))
    }
    curcol = curcol + 1
  }
  plot(unlist(vecX), col = vecArr)
}
