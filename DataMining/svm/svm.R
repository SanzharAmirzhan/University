dataAll = read.table("/Users/sanzhar/Desktop/University/DataMining/Labs/Lab11/iris-slwc2.txt", sep = ',')

X = matrix(1L, ncol = dim(dataAll)[2], nrow = dim(dataAll)[1])
y = dataAll[3]
for(i in 1:dim(dataAll)[1]){
  X[i,1:2] = as.matrix(dataAll[i,1:2])
}

e = 0.0001
c = 10
k = 1

KMat <- as.matrix(X) %*% t(as.matrix(X))

nMat <- c()
for(i in 1:dim(X)[1]){
  nMat[i] = 1/KMat[i,i]
}

al <- matrix(0L, ncol = dim(dataAll)[1], nrow = 10000)

findDif <- function(ind){
  res = as.matrix(al[ind-1,]) - as.matrix(al[ind,])
  ans = t(res) %*% res
  return(sqrt(ans))
}

t <- 1
repeat{
  for(k in 1:dim(al)[2]){
    ans <- 0
    for(i in 1:dim(al)[2]){
      ans <- ans + al[t+1,i] * y[i,1] * KMat[i,k]
    }
    al[t+1,k] = al[t,k] + nMat[k] * (1 - y[k,1] * ans)
    if(al[t+1,k] < 0){
      al[t+1,k] = 0
    }
    if(al[t+1,k] > c){
      al[t+1,k] = c
    }
  }
  t <- t + 1
  findDifRes <- findDif(t)
  if(findDifRes <= e){
    break
  }
  if(t > 100){
    break
  }
}
print('\n')

for(i in 1:dim(al)[2]){
  if(al[t,i] != 0){
    print(al[t,i])
  }  
}

'
ansW <- 0
for(i in 1:dim(al)[2]){
  if(al[t,i] > 0){
    ansW <- ansW + al[t,i] * y[i,1] * as.matrix(X[i,1:2])
  }  
}

biasArray <- 0
for(i in 1:dim(al)[2]){
  if(al[t,i] > 0 && al[t,i] < c){
    biasArray[length(biasArray) + 1] <<- y[i,1] - as.matrix(ansW) %*% t(as.matrix(X[i,1:2]))
  }  
}

bias = mean(biasArray)
'
