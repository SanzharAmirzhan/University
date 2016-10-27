dataAll <- read.csv("~/Desktop/University/DataMining/Labs/Lab7/iris2.txt", header=FALSE)
dataAll = dataAll[1:2]

w <- c()
meanVector <- c()
varVector <- c()
classProb <- c()
oldMeanVector <- c()

expectMax <- function(k, eps, colInd){
  meanVector <<- firstInit(meanVector, k, colInd)
  varVector <<- firstInitVar(varVector, k)
  classProb <<- firstInitClassProb(classProb, k)
  oldMeanVector <<- meanVector
  
  w <<- matrix(0L, nrow = k, ncol = dim(dataAll)[1])
  w <<- expectation(w, meanVector, varVector, classProb, k, colInd)
  
  iter = 0
  while(TRUE){
    oldMeanVector <<- meanVector
    w <<- expectation(w, meanVector, varVector, classProb, k, colInd)
    maximization(w, k, colInd)
    if(sse(oldMeanVector, meanVector, k) < eps){
      break
    }
    iter = iter + 1
  }
  whichClass(w, k, colInd)
  print(iter)
}

expectation <- function(w, meanVector, varVector, probVector, k, colInd){
  for(i in 1:k){
    for(j in 1:dim(dataAll)[1]){
      fir = dnorm(dataAll[j, colInd], meanVector[i], sqrt(varVector[i])) * probVector[i]
      sec = 0
      for(a in 1:k){
        tmp = dnorm(dataAll[j, colInd], meanVector[a], sqrt(varVector[a])) * probVector[a]
        sec = sec + tmp
      }
      w[i,j] = fir/sec
      rm(tmp, fir, sec)
    }
  }
  return(w)
}

maximization <- function(w, k, colInd){
  X <- as.matrix(dataAll[,colInd])
  onceMat <- matrix(1L, nrow = dim(dataAll)[1], ncol = 1)
  for(i in 1:k){
    wtmp <- as.matrix(w[i,])
    wtmp <- t(wtmp)
    fir <- wtmp %*% X
    meanVector[i] <<- fir/(wtmp %*% onceMat)
  }
  
  for(i in 1:k){
    z <- X - (oldMeanVector[i] * onceMat)
    z <- z ^ 2
    wtmp <- as.matrix(w[i,])
    wtmp <- t(wtmp)
    fir <- wtmp %*% z
    varVector[i] <<- fir/(wtmp %*% onceMat)
  }
  
  for(i in 1:k){
    wtmp <- as.matrix(w[i,])
    wtmp <- t(wtmp)
    classProb[i] <<- (wtmp %*% onceMat)/dim(X)[1]
  }
}

whichClass <- function(w, k, colInd){
  colVec <- c("red", "black", "green", "orange", "gray")
  vecArr <- c()
  vecX <- c()
  for(i in 1:dim(dataAll)[1]){
    cl = 0
    pr = -1
    wcol = -1
    for(j in 1:k){
      if(w[j, i] > pr){
        wcol <- colVec[j]
        cl = j
        pr = w[j,i]
      }
    }
    vecArr[i] = wcol
    vecX[i] = dataAll[i,colInd]
    cat(sprintf("\"%f\" \"%f\"\n", i, cl))
  }
  plot(vecX, col = vecArr)
}

sse <- function(old, newMean, k){
  res = 0
  for(i in 1:k){
    cur = abs(old[i] - newMean[i])
    res = res + cur
  }
  return(res)
}

firstInit <- function(meanVector, k, colInd){
  ind = 1
  cur = 1
  nowk = 1
  curMean = 0
  n = dim(dataAll)[1]/k
  for(i in 1:dim(dataAll)[1]){
    dataAll[i, 2] <<- nowk
    curMean = curMean + dataAll[i,colInd]
    
    if(cur >= n){
      cur = 0
      nowk = nowk + 1
      meanVector[ind] = curMean/n
      curMean = 0
      ind = ind + 1
    }
    cur = cur + 1
  }
  meanVector[ind] = curMean/n
  return(meanVector)
}

firstInitVar <- function(varVector, k){
  ind = 1
  for(i in 1:k){
    varVector[ind] = 1
    ind = ind + 1
  }
  return(varVector)
}

firstInitClassProb <- function(classProb, k){
  ind = 1
  for(i in 1:k){
    classProb[ind] = 1/k
    ind = ind + 1
  }
  return(classProb)
}
