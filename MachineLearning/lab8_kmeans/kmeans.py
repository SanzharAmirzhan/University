import numpy as np
import scipy.io
import matplotlib.pyplot as plt
from random import randint
from operator import itemgetter
import math


class kMeans():

    def __init__(self, data):
        self.data = data
        self.dimension = data.shape[1]
        self.numOfPoints = data.shape[0]

    def kMeans(self, numOfCentroids):

        centroids = self.getRandomCentroids(numOfCentroids)
        oldCentroids = None

        labels = -1 * np.ones(self.numOfPoints)

        self.plotGraphWithCentroids(centroids, labels)

        for i in range(200):
            oldCentroids = centroids

            labels = self.getLabels(numOfCentroids, centroids, labels)
            centroids = self.getCentroids(numOfCentroids, centroids, labels)

        self.plotGraphWithCentroids(centroids, labels)

        return

    def getCentroids(self, numOfCentroids, centroids, labels):

        for i in range(numOfCentroids):
            X = []
            Y = []

            cnt = 0
            for j in range(self.numOfPoints):
                if labels[j] == i:
                    X.append(self.data[j][0])
                    Y.append(self.data[j][1])
                    cnt+=1

            if len(X) == 0 or len(Y) == 0:
                continue

            newX = 0
            newY = 0
            for j in range(cnt):
                newX+=X[j]
                newY+=Y[j]
            newX/=cnt
            newY/=cnt

            centroids[i][0] = newX
            centroids[i][1] = newY

        return centroids

    def getLabels(self, numOfCentroids, centroids, labels):
        for i in range(self.numOfPoints):

            distForEachCentroid = []
            #each centroid
            for j in range(numOfCentroids):
                distt = 0

                #each dimension
                for k in range(self.dimension):
                    distt += (centroids[j][k] - self.data[i][k])**2
                distt = math.sqrt(distt)
                distForEachCentroid.append((distt, j))

            distForEachCentroid.sort(key=itemgetter(0))

            labels[i] = distForEachCentroid[0][1]

        return labels

    def getRandomCentroids(self, numOfCentroids):
        #axis by column
        centroids = np.empty((numOfCentroids,0), float)
        for i in range(self.dimension):
            a = []

            for i in range(numOfCentroids):
                a.append(randint(-20, 100))

            centroids = np.c_[centroids, a]
        return centroids

    def plotGraph(self):
        if self.dimension != 2:
            print ("Error: dimension more than 2")
        else:
            plt.scatter(self.data[:,0], self.data[:,1])
            plt.show()

    def plotGraphWithCentroids(self, centroids, labels):
        if self.dimension != 2:
            print ("Error: dimension more than 2")
        else:
            dictt = ['m', 'y', 'r', 'g', 'k', 'b']
            colors = []
            for i in range(len(labels)):
                labels[i] += 1
                colors.append(dictt[int(labels[i])-1])
            plt.scatter(self.data[:,0], self.data[:,1], marker = 'x', c = colors)
            plt.scatter(centroids[:,0], centroids[:,1], marker = 'D', c = 'black')
            plt.show()

    def plotGraphWithLabelsAndCentroids(self, labels, centroids):
        if self.dimension != 2:
            print ("Error: dimension more than 2")
        else:
            difColorOfLabels = set(labels)
            numOfDifColors = len(difColorOfLabels)


#------------------------------#
def generateData(nDimension, numOfPoints):

    # axis by column

    data = np.empty((numOfPoints,0), float)

    for i in range(nDimension):
        a = []

        for i in range(numOfPoints):
            a.append(randint(-10, 100))

        data = np.c_[data, a]

    return data

# Main Function #
def main():
    
    print ("Enter number of points")
    n = int(input())
    
    print ("Enter number of centroids")
    k = int(input())
    
    data = generateData(2, n)

    clust = kMeans(data)

    clust.kMeans(k)

main()

