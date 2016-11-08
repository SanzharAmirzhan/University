modelName = "EEE"
data = iris[1]
z = mclust::unmap(iris[1])
msEst <- mclust::mstep(modelName, data, z)
names(msEst)
modelName = m
