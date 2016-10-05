import math, matplotlib as mpl, matplotlib.pyplot as plt
from operator import itemgetter, attrgetter

def euclidDist(a, b):
    return math.sqrt((a[0] - b[0])**2 + (a[1] - b[1])**2 + (a[2] - b[2])**2 + (a[3] - b[3])**2)

f = open("iris.txt", "r")

testPoint = [4.9, 3.1, 1.9, 1.5]

data = []

xx = []
yy = []

for line in f:
    all = []
    line = line.rstrip('\n')
    all = (line.split(','))
    point = (float(all[0]), float(all[1]), float(all[2]), float(all[3]))
    dist = euclidDist(testPoint, point)
    data.append((dist, all[4]))
    xx.append(float(all[0]))
    yy.append(float(all[1]))

plt.scatter(xx, yy,color="red")
plt.scatter([testPoint[0]], [testPoint[1]], color="green")
plt.show()

data = sorted(data, key = itemgetter(0))

f, s, t = 0, 0, 0
for k in range(3):
    if data[k][1] == "Iris-virginica":
        f+=1
    elif data[k][1] == "Iris-versicolor":
        s+=1
    else:
        t+=1

print f, s, t

if(f > s and f > t):
    print 'Iris-virginica'
elif(s > f and s > t):
    print 'Iris-versicolor'
elif(t > f and t > s):
    print 'Iris-setosa'
else:
    print 'Undefined'





