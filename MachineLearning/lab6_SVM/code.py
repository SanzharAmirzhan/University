import numpy as np
import matplotlib.pyplot as plt
from matplotlib import style
style.use("ggplot")
from sklearn import svm
from sklearn.metrics import accuracy_score

y = []
XX = []


file = open("data.txt", "r")
for line in file:
    all = []
    line = line.rstrip('\n')
    all = line.split(',')
    y.append(float(all[len(all)-1]))
    
    temp = []
    for j in range(1, 10):
        if all[j] == '?':
            rtr = 5
        else:
            rtr = float(all[j])
        temp.append(rtr)
    XX.append(temp)


X = np.array(XX)
    
clf = svm.SVC(kernel='linear', C = 1.0)
clf.fit(X[0:300,:],y[0:300])

y_pred = clf.predict(X[-200:,:])
y_true = y[-200:]

print (accuracy_score(y_true, y_pred))


w = clf.coef_[0]
print(w)

a = -w[0] / w[1]

xx = np.linspace(0,12)
yy = a * xx - clf.intercept_[0] / w[1]

h0 = plt.plot(xx, yy, 'k-', label="non weighted div")

plt.scatter(X[:, 0], X[:, 1], c = y)
plt.legend()
plt.show()