inf <- -10000000

itemsetsFilePath = "/Users/sanzhar/Desktop/University/DataMining/Labs/Lab6/itemsets.txt"
ndiFilePath = "/Users/sanzhar/Desktop/University/DataMining/Labs/Lab6/ndi.txt"

upBound <- -inf
lowBound <- inf

readItemsetsFile <- function(){
  freopen = file(itemsetsFilePath, "r")
  all = c()
  while (TRUE) {
    line = readLines(freopen, n=1)
    if(length(line) == 0){
      break
    }
    arSplit = unlist(strsplit(line, ' - '))
    itemset = as.numeric(unlist(strsplit(arSplit[1], ' ')))
    string = c()
    for (x in itemset) {
      string = union(string, x)
    }
    all <- union(all, string)
  }
  close(freopen)
  return(unique(all))
}

readNdiFile <- function(){
  freopen = file(ndiFilePath, "r")
  while (TRUE) {
    line = readLines(freopen, n = 1)
    if (length(line) == 0) {
      break
    }
    string = as.numeric(unlist(strsplit(line, ' ')))
    item = c()
    for (x in string) {
      item = union(item, x)
    }
    
    upBound <<- -inf
    lowBound <<- inf
    
    rule(c(), item)
    for (x in subsets) {
      rule(x, item)
    }
    cat(item, ": [", lowBound, upBound, "]", "\n")
  }
}

getSupport <- function(findItems){
  freopen = file(itemsetsFilePath, "r")
  while (TRUE) {
    line = readLines(freopen, n=1)
    if(length(line) == 0){
      break
    }
    tokens = unlist(strsplit(line, ' - '))
    string = as.numeric(unlist(strsplit(tokens[1], ' ')))
    support = as.numeric(unlist(strsplit(tokens[2], ' ')))[1]
    items = c()
    for(x in string){
      items = union(items, x)
    }
    if((length(items) == length(findItems)) && (items == findItems)){
      close(freopen)
      return(support)
    }
  }
  close(freopen)
  return(inf)
}

getAllSubsets <- function(itemset){
  ans = list()
  for(i in 1:length(itemset)){
    all = combn(itemset, i)
    for (j in 1:ncol(all)) {
      ans[length(ans)+1] <- list(all[,j])
    }
  }
  return(ans)
}

subsetOf <- function(x,y) {length(setdiff(x,y)) == 0}

rule <- function(X, I){
  Y = setdiff(X, I)
  subsetsI = getAllSubsets(I)
  supp = 0
  for(j in subsetsI){
    if(subsetOf(X, j) && !subsetOf(I, j)){
      curSupp = getSupport(j)
      if(curSupp == inf){
        return()
      }
      if(length(setdiff(I, j)) %% 2 == 0){
        supp = supp - curSupp
      }
      else{
        supp = supp + curSupp
      }
    }
  }
  if(length(X) == 0){
    supp = inf
  }
  if (supp <= 0) { return() }
  if(length(Y) %% 2 == 0) {
    if (lowBound < supp) {
      lowBound <<- supp
    }
  } 
  else{
    if(uprBound > supp) {
      upBound <<- supp
    }
  }
}

all = readItemsetsFile()
subsets = getAllSubsets(all)
readNdiFile()
























