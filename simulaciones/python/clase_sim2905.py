import numpy as np
import scipy as sp
import pandas as pd
import matplotlib.pyplot as plt
from scipy.stats import geom
from scipy.stats import gamma
from scipy.stats import poisson
from scipy.stats import binom
from scipy.stats import uniform


# El ejercicio de los pisos

def el_de_los_pisos(nsim, lambda1, N):
    resultado = np.empty(shape = nsim)
    for i in range(nsim):
        Y = poisson.rvs(lambda1, size = 1)
        U = np.ceil( uniform.rvs(size = Y) * N )
        resultado[i] = len(np.unique(U))
    return resultado

# Hagamos una prueba
x1 = el_de_los_pisos(10000, 10, 10)
10 * (1 - np.exp(-10/10))
np.mean(x1)

# Otra pruebita
x2 = el_de_los_pisos(10000, 50, 70)
70 * (1 - np.exp(-50/70))
np.mean(x2)


# El ejercicio de la suma de dados

def suma_dados(estr, nsim):
    ganancia = np.empty(shape = nsim)
    for i in range(nsim):
        suma = 0
        while (suma <= estr) & (suma != 7):
            suma = np.sum( np.ceil(6 * uniform.rvs(size = 2)) )
        if suma == 7:
            ganancia[i] = 0
        else:
            ganancia[i] = suma
    return ganancia

est2 = suma_dados(2, 10000)
est3 = suma_dados(3, 10000)
est4 = suma_dados(4, 10000)
est5 = suma_dados(5, 10000)
est6 = suma_dados(6, 10000)
est7 = suma_dados(7, 10000)
est8 = suma_dados(8, 10000)
est9 = suma_dados(9, 10000)
est10 = suma_dados(10, 10000)
est11 = suma_dados(11, 10000)
est12 = suma_dados(12, 10000)

np.mean(est2)
np.mean(est3)
np.mean(est4)
np.mean(est5)
np.mean(est6)
np.mean(est7)
np.mean(est8)
np.mean(est9)
np.mean(est10)
np.mean(est11)
np.mean(est12)

# No sé hacer la  tablita tan bonita como en R :(
# Creo que ya sé cómo
todo = np.concatenate((
    est2, est3, est4, est5, est6, est7, est8, est9, est10, est11, est12
))
ids = np.concatenate((
    np.full(10000, 2), np.full(10000, 3), np.full(10000, 4), 
    np.full(10000, 5), np.full(10000, 6), np.full(10000, 7), 
    np.full(10000, 8), np.full(10000, 9), np.full(10000, 10), 
    np.full(10000, 11), np.full(10000, 12)
))

tablita = pd.DataFrame({
    'ganancia' : todo,
    'estrategia' : ids
})
pd.crosstab(tablita.estrategia, tablita.ganancia, margins = True)

# Ahora es el de rachas

def rachas(nsim, p, k):
    resultado = np.empty(shape = nsim)
    for i in range(nsim):
        racha = 0
        lanzamientos = 0
        while racha < k:
            volado = (uniform.rvs(size = 1) < p)
            lanzamientos += 1
            if volado:
                racha += 1
            else:
                racha = 0
        resultado[i] = lanzamientos
    return resultado

rachas1 = rachas(10000, 1/3, 3)
# Comparemos la media teórica con la media de las simulaciones
3+9+27
np.mean(rachas1)

rachas2 = rachas(10000, 1/2, 5)
2 + 4 + 8 + 16 + 32
np.mean(rachas2)