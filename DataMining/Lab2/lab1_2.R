dataAll <- read.csv("~/Desktop/University/DataMining/Lab2/NumericDataAnalysis.txt", header=FALSE)
tableData = dataAll[1:10]

multMatrix <- function(a, b){
  if(dim(a)[2] != dim(b)[1]){
    print("size of matrices are diff")
    return(-1)
  }
  else{
    res <- matrix(, nrow = dim(a)[1], ncol = dim(b)[2])
    for(i in 1:dim(a)[1]){
      for(j in 1:dim(b)[2]){
        res[i, j] = 0
        for(k in 1:dim(a)[2]){
          res[i,j] = res[i,j] + a[i,k] * b[k, j]
        }
      }
    }
    return(res)
  }
}

transposeMatrix <- function(a){
  res <- matrix(, nrow = dim(a)[2], ncol = dim(a)[1])
  for(i in 1:dim(res)[1]){
    for(j in 1:dim(res)[2]){
      res[i,j] = a[j,i]
    }
  }
  return(res)
}

substractMatrix <- function(a, b){
  if((dim(a)[1] != dim(b)[1]) || (dim(a)[2] != dim(b)[2])){
    print("size of matrices are diff")
    return(-1)
  }
  else{
    res <- matrix(, nrow = dim(a)[1], ncol = dim(a)[2])
    for(i in 1: dim(a)[1]){
      for(j in 1:dim(a)[2]){
        res[i,j] = a[i,j] - b[i,j]
      }
    }
    return(res)
  }
}

additionMatrix <- function(a, b){
  if((dim(a)[1] != dim(b)[1]) || (dim(a)[2] != dim(b)[2])){
    print("size of matrices are diff")
    return(-1)
  }
  else{
    res <- matrix(, nrow = dim(a)[1], ncol = dim(a)[2])
    for(i in 1: dim(a)[1]){
      for(j in 1:dim(a)[2]){
        res[i,j] = a[i,j] + b[i,j]
      }
    }
    return(res)
  }
}

findCorrelation <- function(x, y){
  ans <- eOuter[x, y] / sqrt(eOuter[x,x] * eOuter[y,y])
  return(ans)
}

#1

meanMatrix <- matrix(, nrow = 1, ncol = dim(tableData)[2])
for (i in 1:10) {
  meanMatrix[1, i] = sum(tableData[i])/dim(tableData[i])[1]
}


#2

oneVector <- matrix(1L, nrow = dim(tableData)[1], ncol = 1)
ax = multMatrix(oneVector, meanMatrix)
z = substractMatrix(tableData, ax)
eOuter = multMatrix(transposeMatrix(z), z)/dim(tableData)[1]


#3

zTransp = transposeMatrix(z)
eInner <- matrix(0L, nrow = dim(tableData)[2], ncol = dim(tableData)[2])
for(i in 1:dim(tableData)[1]){
  vecq = as.matrix(zTransp[,i])
  multq = multMatrix(vecq, transposeMatrix(vecq))
  summ = additionMatrix(eInner, multq)
  eInner = summ
}
eInner = eInner / dim(tableData)[1]


#4
cor12 = findCorrelation(1,2)
#plot(tableData[,1], tableData[,1])

#5
plot(tableData[,1], dnorm(tableData[,1], mean = meanMatrix[1,1], sd = sqrt(sqrt(eOuter[1,1]))))

#6
varianceVector <- vector(mode = "list")

for(i in 1:10){
  varianceVector[[i]] = c((i), (eOuter[i,i]))
}
varianceVectorOrder = order(sapply(varianceVector, '[', 2))
varianceVector = varianceVector[varianceVectorOrder]


#7
covarianceVector <- vector(mode = "list")

ind = 1
for (i in 1:10) {
  for(j in (i+1):10){
    if(j <= 10 && i <= 10){
      covarianceVector[[ind]] = c((toString(i*1000 + j, width = 6)), (eOuter[i, j]))
      ind = ind + 1
    }
  }
}
covarianceVectorOrder = order(sapply(covarianceVector, '[', 2))
covarianceVector = covarianceVector[covarianceVectorOrder]







