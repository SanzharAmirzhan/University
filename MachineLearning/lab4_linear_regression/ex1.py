from numpy import *
import operator
import numpy as np


def file2matrix(filename):
    fr = open(filename)
    numberOfLines = len(fr.readlines())
    data = zeros((numberOfLines, 2))
    fr = open(filename, 'r')
    index = 0

    output = open(filename, 'r')  # See the r
    listFromLine = output.readlines()
    while index < len(listFromLine):
        line = listFromLine[index].split(',')
        data[index, :] = line
        index += 1

    return data


def computeCost(X, y, theta):
    J = 0
    # ----Write your code here ----
    sizeX = len(X);
    for i in range(sizeX):
        val = (theta[0][0] + theta[1][0] * X[i][1] - y[i])
        J = J + val * val
    J = J / (2 * sizeX)
    return J


def gradientDescent(X, y, theta, alpha, num_iters):
    m = len(y)  # number of training examples
    J_history = zeros((num_iters, 1))
    for i in range(num_iters):
        J_history[i] = computeCost(X, y, theta)
        # ---- Write your code here ----

        sum0 = 0
        sum1 = 0

        for j in range(m):
            sum0 += ((theta[0][0] + theta[1][0] * X[j][1]) - y[j]) * X[j][0]
            sum1 += ((theta[0][0] + theta[1][0] * X[j][1]) - y[j]) * X[j][1]

        theta[0][0] = theta[0][0] - float(alpha * (float(1) / m)) * sum0
        theta[1][0] = theta[1][0] - float(alpha * (float(1) / m)) * sum1


    return theta, J_history


# read from file
data = file2matrix('ex1data1.txt')

m = len(data)
data = np.insert(np.array(data), 0, np.ones(m), axis=1)  # Add a column of ones to x
y = data[:, 1]
theta = zeros((2, 1))  # initialize fitting parameters
# Some gradient descent settings
X = data[:, 0:2]
y = data[:, 2]
# print (X)
# print (y)
iterations = 1500
alpha = 0.01

# compute and display initial cost
J = computeCost(X, y, theta)
print ('Cost value is ' + str(J))
# run gradient descent
theta, J_history = gradientDescent(X, y, theta, alpha, iterations)
# print (theta)
predict1 = np.dot([1, 5], theta)
predict2 = np.dot([1, 25], theta)

# code for visualizing your data
import matplotlib.pyplot as plt
import pylab

plt.scatter(X[:, 1], y, color='red', marker='x')
plt.ylabel('Profit in $10,000s');
plt.xlabel('Population of City in 10,000s');
plt.plot([5, 25], [predict1, predict2], color='blue', linewidth=2.0)
plt.show()

print(predict1, predict2)
iterations = 1500;
alpha = 0.01;
