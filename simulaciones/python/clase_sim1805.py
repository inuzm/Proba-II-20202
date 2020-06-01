import numpy as np
import scipy as sp
import matplotlib.pyplot as plt
from scipy.stats import geom
from scipy.stats import gamma
from scipy.stats import poisson
from scipy.stats import binom

def simular_ej4(m, p, lambda1):
    N = geom.rvs(p, size = m)
    X = np.empty(shape = m)
    for i in range(m):
        X[i] = gamma.rvs(a = N[i], scale = 1/lambda1, size = 1)
    return np.column_stack((N,X))

Y = simular_ej4(100000, 0.1, 5.2)

plt.scatter(Y[:,0], Y[:,1], s = 0.1, alpha = 0.5)
plt.show()

xaux = np.linspace(0,np.max(Y[:,1]),num = 1000)

plt.hist(Y[:,1], density = True, bins = 100)
plt.plot(xaux, gamma.pdf(xaux, a = 1, scale = 1/(0.1 * 5.2)), lw = 2)
plt.show()

def simulacion2(n, p, lambda1):
    X = poisson.rvs(lambda1, size = n)
    Y = np.empty(shape = n)
    for i in range(n):
        Y[i] = binom.rvs(X[i], p, size = 1)
    return np.column_stack((X, Y))

Z = simulacion2(100000, 0.5, 5)

fig, axs = plt.subplots(2, 2)
axs[0,0].hist2d(
    Z[:,0], Z[:,1], 
    bins = [
        np.arange(np.min(Z[:,0])-0.5, np.max(Z[:,0])+1.5), 
        np.arange(np.min(Z[:,1])-0.5, np.max(Z[:,1])+1.5)
    ]
)
axs[1,0].hist(
    Z[:,0], 
    bins = np.arange(np.min(Z[:,0])-0.5, np.max(Z[:,0])+1.5), 
    density = True
)
axs[0,1].hist(
    Z[:,1], 
    bins = np.arange(np.min(Z[:,1])-0.5, np.max(Z[:,1])+1.5), 
    density = True, orientation = 'horizontal'
)
axs[1, 1].scatter(Z[:,0], Z[:,1], s = 0.1, alpha = 0.5)
plt.show()

X_condY0 = Z[np.where(Z[:,1] == 0)[0], 0]

plt.hist(
    X_condY0, density = True, 
    bins = np.arange(np.min(X_condY0)-0.5, np.max(X_condY0)+1.5)
)
plt.show()

X_condY2 = Z[np.where(Z[:,1] == 2)[0], 0]

plt.hist(
    X_condY2, density = True, 
    bins = np.arange(np.min(X_condY2)-0.5, np.max(X_condY2)+1.5)
)
plt.show()