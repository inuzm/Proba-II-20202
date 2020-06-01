import numpy as np
import scipy as sp
import matplotlib.pyplot as plt
from scipy.stats import geom
from scipy.stats import gamma
from scipy.stats import poisson
from scipy.stats import binom

def vectores1(n, lambda1, p):
    x = poisson.rvs(lambda1, size = n)
    y = np.empty(shape = n)
    for i in range(n):
        y[i] = binom.rvs(x[i], p, size = 1)
    return np.column_stack((x, y))

V1 = vectores1(100000, 3.3, 0.6)

XcondY1 = V1[np.where(V1[:, 1] == 1)[0], 0]
np.mean(XcondY1)

plt.hist(
    XcondY1, density = True, 
    bins = np.arange(np.min(XcondY1)-0.5, np.max(XcondY1)+1.5)
)
plt.show()




def vector2(n, p, lambda1):
    N = geom.rvs(p, size = n)
    X = np.empty(shape = n)
    for i in range(n):
        X[i] = gamma.rvs(a = N[i], scale = 1/lambda1, size = 1)
    return np.column_stack((N, X))

V2 = vector2(1000000, 0.4, 3)
h1 = 1e-2
x1 = 0.83
NcondX = V2[np.intersect1d(np.where(V2[:, 1] > x1-h1)[0], np.where(V2[:, 1] < x1+h1)), 0]
xaux = np.unique(NcondX)

plt.hist(
    NcondX, density = True, 
    bins = np.arange(np.min(NcondX)-0.5, np.max(NcondX)+1.5)
)
plt.scatter(xaux, poisson.pmf(xaux-1, mu = 3*0.6*0.83), color = "red")
plt.show()


def sol_num(n, p1, p2, K):
    X = geom.rvs(p1, size = n)
    Y = geom.rvs(p2, size = n)
    proba = np.sum(X + Y > K) / n
    return proba

sol_num(1000000, 0.3, 0.4, 12)
sol_num(1000000, 0.1, 0.2, 20)





def personas_escogen_objetos(n, K, m):
    A = np.empty(shape = n)
    N = np.empty(shape = n)
    U = np.empty(shape = n)
    for i in range(n):
        j1 = np.random.choice(np.arange(1, K+1), size = m, replace = False)
        j2 = np.random.choice(np.arange(1, K+1), size = m, replace = False)
        A[i] = len(np.intersect1d(j1, j2))
        N[i] = K - len(np.union1d(j1, j2))
        U[i] = K - A[i] - N[i]
    return np.column_stack((A, N, U))

X1 = personas_escogen_objetos(100000, 10, 3)
np.mean(X1[:,0])
np.mean(X1[:,1])
np.mean(X1[:,2])