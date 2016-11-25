dataAll <- read.csv("~/Desktop/University/DataMining/Labs/Lab3/adult.data.txt", header=FALSE)
tableData = dataAll[, c(4, 9)]

edu <- list()
race <- list()
for(i in 1:dim(tableData)[1]){
  tmp = toString(tableData[i,1])
  tmp2 = toString(tableData[i,2])
  if(tmp %in% names(edu)){
    edu[[tmp]] = edu[[tmp]] + 1;
  }
  else{
    edu[[tmp]] = 1;
  }
  
  if(tmp2 %in% names(race)){
    race[[tmp2]] = race[[tmp2]] + 1;
  }
  else{
    race[[tmp2]] = 1;
  }
}

eduNames = names(edu)
raceNames = names(race)

contMatrix <- matrix(0L, nrow = length(edu), ncol = length(race))
for(i in 1:dim(tableData)[1]){
  x = -1
  y = -1
  tmp1 = toString(tableData[i,1])
  tmp2 = toString(tableData[i,2])
  for(j in 1:length(eduNames)){
    if(eduNames[j] == tmp1){
      x = j
      break
    }
  }
  for(j in 1:length(raceNames)){
    if(raceNames[j] == tmp2){
      y = j
      break
    }
  }
  contMatrix[x,y] = contMatrix[x,y] + 1
}


rowSum <- 0
for(i in 1:dim(contMatrix)[1]){
  rowSum[i] = sum(contMatrix[i,])
}

colSum = 0
for(i in 1:dim(contMatrix)[2]){
  colSum[i] = sum(contMatrix[,i])
}



eMatrix = matrix(0L, nrow = dim(contMatrix)[1], ncol = dim(contMatrix)[2])
for(i in 1:dim(eMatrix)[1]){
  for(j in 1:dim(eMatrix)[2]){
    eMatrix[i,j] = colSum[j]*rowSum[i]/dim(tableData)[1]
  }
}



xSquareSt = 0
for(i in 1:dim(contMatrix)[1]){
  for(j in 1:dim(contMatrix)[2]){
    ne = (contMatrix[i,j] - eMatrix[i,j])
    xSquareSt = xSquareSt + (ne*ne)/eMatrix[i,j]
  }
}


qDegree = (dim(eMatrix)[1] - 1) * (dim(eMatrix)[2] - 1)

pValue = 1 - pchisq(xSquareSt, qDegree)

z = qchisq(.99, qDegree)




