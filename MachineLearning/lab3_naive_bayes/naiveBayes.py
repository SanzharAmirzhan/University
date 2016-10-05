import os
from math import log10, log

cnt = {}
spamCnt = {}
nonSpamCnt = {}

def parseEmail(allSpamFiles, allNonSpamFiles) :
    k = 0
    n = 0
    for file in allSpamFiles :
        f = open('ex6DataEmails/spam-train/' + file, 'r')
        #print file
        for line in f :
            allWords = []
            allWords = line.split(' ');
        for word in allWords :
            if word not in cnt.keys():
                cnt[word] = 0
            cnt[word] = cnt[word] + 1
            if word not in spamCnt.keys() :
                spamCnt[word] = 0
            spamCnt[word] = spamCnt[word] + 1
            k+=1

    for file in allNonSpamFiles:
        f = open('ex6DataEmails/nonspam-train/' + file, 'r')
        #print file
        for line in f :
            allWords = []
            allWords = line.split(' ');
        for word in allWords :
            if word not in cnt.keys():
                cnt[word] = 0
            cnt[word] = cnt[word] + 1
            if word not in nonSpamCnt.keys() :
                nonSpamCnt[word] = 0
            nonSpamCnt[word] = nonSpamCnt[word] + 1
            n+=1
    return k, n

def wordProbabilitySpam(spamCnt, k):
    probSpamWordDic = {}
    for keys in spamCnt.keys():
        probSpamWordDic[keys] = float(spamCnt[keys] + 1) / (k + len(cnt))
    return probSpamWordDic

def wordProbabilityNonSpam(nonSpamCnt, k):
    probSpamWordDic = {}
    for keys in nonSpamCnt.keys():
        probSpamWordDic[keys] = float(nonSpamCnt[keys] + 1)/(k + len(cnt))
    return probSpamWordDic

def checkTest(spamFiles, nonSpamFiles, spamWordsProb, nonSpamWordsProb):
    spamTrue = 0
    nonSpamTrue = 0
    for file in spamFiles :
        f = open('ex6DataEmails/spam-test/' + file, 'r')
        probp = log10(0.5)
        probn = log10(0.5)
        for line in f :
            allWords = []
            allWords = line.split(' ');
            for word in allWords:
                if word in spamWordsProb.keys():
                    probp += log(spamWordsProb[word])
            for word in allWords:
                if word in nonSpamWordsProb.keys():
                    probn += log(nonSpamWordsProb[word])
        if probp > probn:
                spamTrue+=1

    for file in nonSpamFiles :
        f = open('ex6DataEmails/nonspam-test/' + file, 'r')
        probp = log10(0.5)
        probn = log10(0.5)
        for line in f :
            allWords = []
            allWords = line.split(' ');
            for word in allWords:
                if word in spamWordsProb.keys():
                    probp += log(spamWordsProb[word])
            for word in allWords:
                if word in nonSpamWordsProb.keys():
                    probn += log(nonSpamWordsProb[word])
        if probn > probp:
            nonSpamTrue+=1
    return (spamTrue, nonSpamTrue)

def main() :
    allSpamFiles = []
    allNonSpamFiles = []

    allSpamFilesTest = []
    allNonSpamFilesTest = []

    for file in os.listdir('ex6DataEmails/spam-train'):
        if (file.endswith('.txt')) :
            allSpamFiles.append(file)

    for file in os.listdir('ex6DataEmails/nonspam-train'):
        if (file.endswith('.txt')) :
            allNonSpamFiles.append(file)

    for file in os.listdir('ex6DataEmails/spam-test'):
        if (file.endswith('.txt')) :
            allSpamFilesTest.append(file)

    for file in os.listdir('ex6DataEmails/nonspam-test'):
        if (file.endswith('.txt')) :
            allNonSpamFilesTest.append(file)

    k, n = parseEmail(allSpamFiles, allNonSpamFiles)
    spamProbWords = wordProbabilitySpam(spamCnt, k)
    nonSpamProbWords = wordProbabilityNonSpam(nonSpamCnt, n)

    ans = checkTest(allSpamFilesTest, allNonSpamFilesTest, spamProbWords, nonSpamProbWords)
    print ans
    print k, n

main()
