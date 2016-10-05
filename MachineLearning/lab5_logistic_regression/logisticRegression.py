from numpy import *
import operator
import numpy as np
from math import exp
def file2matrix(filename):
  fr = open(filename)
  numberOfLines = len(fr.readlines())
  data = zeros((numberOfLines,3))
  fr = open(filename, 'r')
  index = 0

  output = open(filename, 'r') # See the r
  listFromLine = output.readlines()
  while index < len(listFromLine):
    line = listFromLine[index].split(',')
    data[index,:] = line
    index+=1

  return data

def getG(Z) :
	if (Z > 30) :
		return 0.99
	if (Z < -30) :
		return 0.1;
	return 1.0 / (1 + exp(-Z))

def computeCost(X, y, theta):
	J=0
	# ----Write your code here ----
	sizeX = len(X);
	for i in range(sizeX) :
		h0 = 0
		for Q in range(3) :
			h0 = h0 + 1.0 * theta[Q][0] * X[i][Q]
		h0 = float(h0)
		h0 = getG(h0)
		J += -1.0 * y[i] * log(h0) - 1.0 * (1 - y[i]) * log(1 - h0)
	# ---- end of code -----
	return J / m

def gradientDescent(X, y, theta, alpha, num_iters):
	m = len(y) # number of training examples
	J_history = zeros((num_iters, 1))

	for i in range(num_iters):
		J_history[i] = computeCost(X, y, theta)
		#---- Write your code here ----
		newT = theta
		for H in range(3) :
			SUM = 0
			for K in range(m) :
				h0 = 0
				for Q in range(3) :
					h0 = h0 + 1.0 * theta[Q][0] * X[K][Q]
				h0 = getG(h0)
				SUM += (h0 - y[K]) * X[K][H]
			newT[H][0] -= alpha * 1.0 / m * SUM
		theta = newT

		# ---- end of code
	return theta, J_history

#read from file
data = file2matrix('ex2data1.txt')

m = len(data)
data = np.insert(np.array(data),0,np.ones(m),axis=1) # Add a column of ones to x
y = data[:,1]
theta = zeros((3, 1)) # initialize fitting parameters
# Some gradient descent settings
X = data[:,0:3]
y = data[:,3]
# print (X)
# print (y)
# exit(0)
iterations = 150000
alpha = 0.001

# compute and display initial cost
J = computeCost(X, y, theta)
print ('Cost value is '+str(J))
#run gradient descent
theta, J_history = gradientDescent(X, y, theta, alpha, iterations)
#print (J_history)

print (computeCost(X, y, theta))

exit(0)

predict1 = getG(np.dot([1, 45,85], [theta[0][0],theta[1][0], theta[2][0]]))

print predict1

plottX = []
plottY = []

plottXX = []
plottYY = []

for i in range(len(y)):
	if y[i] == 0:
		plottX.append(X[i][1])
		plottY.append(X[i][2])
	else:
		plottXX.append(X[i][1])
		plottYY.append(X[i][2])

# code for visualizing your data
import matplotlib.pyplot as plt
import pylab
plt.scatter(plottX,plottY,color='red', marker='x')
plt.scatter(plottXX,plottYY,color='green', marker='x')
plt.ylabel('Profit in $10,000s');
plt.xlabel('Population of City in 10,000s');
plt.plot([5,25],[theta[1][0], theta[2][0]], color='blue',linewidth=2.0)
plt.show()

print(predict1)
iterations = 1500;
alpha = 0.01;
