#dataAll <- read.csv("~/Desktop/University/DataMining/Labs/Lab2/NumericDataAnalysis.txt", header=FALSE)
#tableData = dataAll[1:10]

#1
"
meanVector <- 0
for (i in 1:10) {
  meanVector[i] = sum(tableData[i])/dim(tableData[i])[1]
}
"
#2
"
oneVector = rep(1, dim(tableData)[1])
oneVector = t(oneVector)
ax = t(oneVector) %*% meanVector
z = tableData - ax
eOuter = (t(z) %*% as.matrix(z))/dim(tableData)[1]
"
#3
"
zTransp = t(z)
eInner = zTransp[,1] %*% t(zTransp[,1])
for(i in 2:19020){
  vector1 = zTransp[,i]
  eInner = eInner + vector1 %*% t(vector1)
}
eInner = eInner / dim(tableData)[1]
"

findCorrelation <- function(x, y){
  ans <- eOuter[x, y] / (eOuter[x,x] * eOuter[y,y])
  return(ans)
}

#4
#cor12 = findCorrelation(1,2)

#6
"
varVector <- 0
for(i in 1:10){
  varVector[i] = eOuter[i,i]
}
"

#7
"
covarVector <- 0
ind <- 1
for(i in 1:10){
  for(j in i:10){
    if(j > i){
      covarVector[ind] = eOuter[i, j]
      ind = ind + 1
    }
  }
}
"


