#dataAll <- read.csv("~/Desktop/University/DataMining/Lab5/mushroom.txt", sep = " ", header=FALSE)
#tableData = dataAll[1:23]

charmAlgorithm <- function(minsup){
  pVector <- vector(mode = "list")
  pVectorInd = 1
  for(i in 1:max(tableData)){
    freq = 0
    for(j in 1:dim(tableData)[1]){
      for(k in 1:dim(tableData)[2]){
        if(tableData[j,k] == i){
          freq = freq + 1
        }
      }
    }
    if(freq >= minsup){
      
    }
  }
}

charmAlgorithm(5000)