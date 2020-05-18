# 18 de mayo
# INM

require(lattice)

# La siguiente función simula `m` vectores aleatorios (N,X) siguiendo el ejercicio 4
# de la tarea:
# N ~ geom(p)
# X | N = n ~ Gamma(n, lambda)
simular_ej4 <- function(m, p, lambda){
    # R simula geométricas que comienzan en 0 :(
    # Para simular la geométrica que comienza en 1, podemos simular
    # geométricas que comiencen en 0 y le sumamos 1.
    N <- rgeom(n = m, prob = p) + 1
    # X <- rgamma(n = m, shape = N, rate = lambda)
    X <- numeric(length = m)
    # Así, condicional en N[i], X[i] ~ Gamma(N[i], lambda)
    for(i in 1:m){
        X[i] <- rgamma(n = 1, shape = N[i], rate = lambda)
    }
    # Regresamos un data.frame con dos columnas, una correspondiente a N
    # y otra a X
    return(
        data.frame(
            n = N,
            x = X
        )
    )
}

# Hacemos cien mil simulaciones
Y <- simular_ej4(m = 1e5, p = 0.1, lambda = 5.2)
# Veamos los primeros 6 renglones de Y
head(Y)

# Una gráfica de las simulaciones de los vectores aleatorios
plot(Y, pch = 20, cex = 0.3)

# Aquí sucede algo mágico. Si solamente consideramos la columna que corresponde a 
# X, lo que obtenemos son simulaciones de 
# X ~ exp(p * lambda)
# que es la distribución marginal de X
x <- Y$x
# variables auxiliares para calcular la densidad de una exponencial
xaux <- seq(from = 0, to = max(x), length = 1e4)
yaux <- dexp(xaux, rate = 0.1 * 5.2)
# Hacemos un histograma y ponemos la densidad de la exponencial encima
hist(x, freq = FALSE, breaks = 30, ylab = "f(x)", main = "Densidad empírica y teórica de X")
lines(xaux,yaux, col = "darkorchid", lwd = 2)
mean(x)

# Ahora vamos a simular `n` vectores aleatorios (X,Y) tales que
# X ~ Poisson(lambda)
# Y | X = x ~ Binom(x, p)
simulacion2 <- function(n, p, lambda){
    # Primero simulamos X
    X <- rpois(n = n, lambda = lambda)
    # Podemos simular la condicional en una sola línea
    Y <- rbinom(n = n, size = X, prob = p)
    # Regresamos un data.frame con dos columnas, una que corresponde a X y
    # otra que corresponde a Y
    return(
        data.frame(
            x = X,
            y = Y
        )
    )
}

# Corramos un ejemplo
Z <- simulacion2(n = 1e5, p = 0.5, lambda = 5)
plot(Z, pch = 20, cex = 0.2) 
# Una opción es usar levelplot del paquete lattice
x_c <- cut(Z$x, breaks = seq(from = min(Z$x) - 0.5, to = max(Z$x) + 0.5, by = 1))
y_c <- cut(Z$y, breaks = seq(from = min(Z$y) - 0.5, to = max(Z$y) + 0.5, by = 1))
z_c <- table(x_c, y_c)
gridaux <- expand.grid(x = sort(unique(Z$x)), y = sort(unique(Z$y)))
gridaux$z <- as.vector(z_c)
levelplot(z ~ x*y, gridaux)

# Otra opción es usar heatmap de graphics
heatmap(
    z_c, Rowv = NA, Colv = NA, 
    labRow = sort(unique(Z$x)), labCol = sort(unique(Z$y)), 
    xlab = "Y", ylab = "X"
)

# Ahora veamos los histogramas de las marginales
hist(
    Z$x, freq = FALSE, 
    breaks = seq(from = min(Z$x) - 0.5, to = max(Z$x) + 0.5, by = 1),
    xlab = "n",
    ylab = "P(X = n)",
    main = "Marginal de X"
)
hist(
    Z$y, freq = FALSE, 
    breaks = seq(from = min(Z$y) - 0.5, to = max(Z$y) + 0.5, by = 1),
    xlab = "n",
    ylab = "P(Y = n)",
    main = "Marginal de Y"
)

# Si queremos ver la densidad de X, condicional en Y = 0, basta con obtener
# las entradas de X para las que Y = 0.
X_condY0 <- Z$x[Z$y == 0]
# Hacemos un histograma
hist(
    X_condY0, freq = FALSE,
    breaks = seq(from = min(X_condY0) - 0.5, to = max(X_condY0) + 0.5, by = 1),
    xlab = "n",
    ylab = "P(X = n | Y = 0)",
    main = "Distribución de X, condicional en Y = 0"
)

# Si queremos ver la densidad de X, condicional en Y = 0, basta con obtener
# las entradas de X para las que Y = 2.
X_condY2 <- Z$x[Z$y == 2]
# Hacemos un histograma
hist(
    X_condY2, freq = FALSE,
    breaks = seq(from = min(X_condY2) - 0.5, to = max(X_condY2) + 0.5, by = 1),
    xlab = "n",
    ylab = "P(X = n | Y = 2)",
    main = "Distribución de X, condicional en Y = 2"
)
