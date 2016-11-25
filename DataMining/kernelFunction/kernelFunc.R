dataAll <- read.csv("~/Desktop/University/DataMining/Labs/Lab4/iris.txt", header=FALSE)
tableData <- dataAll[1:4]

#kernel function
kernel1 <- matrix(0L, nrow = dim(tableData)[1], ncol = dim(tableData)[1])
for(i in 1:dim(tableData)[1]){
  for(j in 1:dim(tableData)[1]){
    x = as.matrix(tableData[i,])
    y = as.matrix(tableData[j,])
    aq = x %*% t(y)
    kernel1[i,j] = aq*aq
  }
}

#centered matrix     
idenMatrix = diag(dim(tableData)[1])
onesMatrix <- matrix(1L, nrow = dim(tableData)[1], ncol = dim(tableData)[1])
onesMatrix = onesMatrix / dim(tableData)[1]
kernel1 = (idenMatrix - onesMatrix) %*% kernel1 %*% (idenMatrix - onesMatrix)

#normalize
w = diag(dim(tableData)[1])
w = w * kernel1
for(i in 1:dim(tableData)[1]){
  w[i,i] = 1 / sqrt(w[i,i]) 
}
kernel1 = w %*% kernel1 %*% w
"
for(i in 1:dim(tableData)[1]){
  for(j in 1:dim(tableData)[1]){
    kernel1[i,j] = kernel1[i,j]/sqrt(kernel1[i,i] * kernel1[j,j])
  }
}
"


#2
#transform to feature space
featSpace <- function(x){
  ans <- 0
  sq2 <- sqrt(2)
  ind = 1
  for(i in 1:dim(x)[2]){
    ans[ind] = x[i] * x[i]
    ind = ind + 1
  }
  for(i in 1:dim(x)[2]){
    for(j in 1:dim(x)[2]){
      if(j > i){
        ans[ind] = sq2 * x[i] * x[j]
        ind = ind + 1
      }
    }
  }
  ans = as.matrix(ans)
  return(t(ans))
}

#newTable feature space
newTable <- matrix(0L, nrow = dim(tableData)[1], ncol = dim(featSpace(tableData[1,]))[2])
for(i in 1:dim(newTable)[1]){
  newTable[i,] = unlist(featSpace(tableData[i,]))
}

#centralized
meanVector <- 0
for(i in 1:dim(newTable)[2]){
  meanVector[i] = sum(newTable[,i])/dim(newTable)[1]  
}
meanVector = as.matrix(meanVector)
for(i in 1:dim(newTable)[1]){
  newTable[i,] = newTable[i,] - meanVector
}

#normalized
for(i in dim(newTable)[1]){
  aw = t(newTable[i,]) %*% newTable[i,]
  newTable[i,] = newTable[i,] / sqrt(aw)
}

#kernel matrix
kernel2 <- matrix(, nrow = dim(tableData)[1], ncol = dim(tableData)[1])
for (i in 1:dim(kernel2)[1]){
  for(j in 1:dim(kernel2)[2]){
    tmp1 = sqrt(t(newTable[i,]) %*% newTable[i,])
    tmp2 = sqrt(t(newTable[j,]) %*% newTable[j,])
    kernel2[i,j] = (t(newTable[i,]) %*% newTable[j,]) / (tmp1 * tmp2)
  }
}


####
diff = 0
for(i in 1:dim(tableData)[1]){
  for(j in 1:dim(tableData)[1]){
    diff = diff + abs(kernel1[i,j] - kernel2[i,j])
  }
}

