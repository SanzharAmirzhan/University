import matplotlib.pyplot as plt
import numpy, scipy
import operator
from operator import itemgetter
from math import log

class DecisionTree:

    def __init__(self, dataSet, labels):
        self.dataSet = dataSet
        self.labels = labels

    def findEntropy(self, data):
        n = len(data)
        cnt = {}
        entropy = 0.0
        for line in data:
            curl = line[-1]
            if curl not in cnt.keys():
                cnt[curl] = 0
            cnt[curl] += 1
        for key in cnt:
            prob = float(cnt[key])/n
            entropy -= prob * log(prob, 2)
        return entropy

    def splitDataset(self, data, axis, value):
        ansData = []
        for line in data:
            if line[axis] == value:
                featvec = line[:axis]
                featvec.extend(line[axis+1:])
                ansData.append(featvec)
        return ansData

    def bestFeature(self, data):
        n = len(data[0]) - 1
        baseEntropy = self.findEntropy(data)
        bestInfoGain = 0.0
        bestFeature = -1
        for i in range(n):
            featList = [line[i] for line in data]
            uniq = set(featList)
            newEntropy = 0.0
            for val in uniq:
                subDataSet = self.splitDataset(data, i, val)
                prob = len(subDataSet)/float(len(data))
                newEntropy += prob * self.findEntropy(subDataSet)
            infoGain = baseEntropy - newEntropy
            if infoGain > bestInfoGain:
                bestInfoGain = infoGain
                bestFeature = i
        return bestFeature

    def majorityCnt(self, classList):
        classCnt = {}
        for vote in classList:
            if vote not in classCnt.keys():
                classCnt[vote] = 0
            classCnt[vote] += 1
        sortedClassCnt = sorted(classCnt.iteritems(), key = operator.itemgetter(1), reverse=True)
        return sortedClassCnt[0][0]

    def buildTree(self, data, labels):
        classList = [line[-1] for line in data]
        if classList.count(classList[0]) == len(classList):
            return classList[0]
        if len(data[0]) == 1:
            return self.majorityCnt(classList)
        bestFeat = self.bestFeature(data)
        bestFeatLabel = labels[bestFeat]
        tree = {bestFeatLabel:{}}
        del(labels[bestFeat])
        featValues = [line[bestFeat] for line in data]
        uniq = set(featValues)
        for val in uniq:
            subLabels = labels[:]
            tree[bestFeatLabel][val] = self.buildTree(self.splitDataset(data, bestFeat, val), subLabels)
        return tree



################################################################
def main():
    fs = open('dectrees/monks-1.train', 'r')

    dataset = []
    labels = []

    for i in range(1, 8):
        labels.append(i)

    for line in fs:
        line = line.rstrip('\n')
        all = (line.split(' '))
        dataset.append([all[x] for x in range(1,8)])


    ################################
    tree = DecisionTree(dataset, labels)
    print tree.buildTree(tree.dataSet, tree.labels)

main()