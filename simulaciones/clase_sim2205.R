# Ayudantía 22/05
# INM

library(dplyr)

# ¿Cómo podemos obtener numéricamente la distribución condicional de X dada Y = y si conocemos la 
# distribución de Y dada X = x y la de X?
# Vamos a suponer que X ~ Poisson(lambda) y Y | X = n ~ Binomial(n, p)
vectores1 <- function(n, lambda, p){
    x <- rpois(n = n, lambda = lambda)
    y <- numeric(length = n)
    for(i in 1:n){
        y[i] <- rbinom(n = 1, size = x[i], prob = p)
    }
    return(
        data.frame(
            x = x,
            y = y
        )
    )
}

V1 <- vectores1(n = 1e5, lambda = 3.3, p = 0.6)
# Queremos obtener la distribución de X condicional en Y = 1
XcondY1 <- V1 %>% 
    filter(y == 1)
summary(XcondY1)
hist(
    XcondY1$x, freq = FALSE,
    breaks = seq(from = min(XcondY1$x)-0.5, to = max(XcondY1$x)+0.5, by = 1),
    main = "Probabilidad condicional de X dado que Y = 1"
)

# Todo funciona bonito en el caso discreto pero ¿y el caso continuo?
# Un ejemplo es el ejercicio 4 de la tarea en el que N ~ Geometrica(p) y
# X | N = n ~ Gamma(n, lambda)
vector2 <- function(n, p, lambda){
    N <- rgeom(n = n, prob = p) + 1
    X <- numeric(length = n)
    for(i in 1:n){
        X[i] <- rgamma(n = 1, shape = N[i], rate = lambda)
    }
    return(
        data.frame(
            n = N,
            x = X
        )
    )
}

V2 <- vector2(n = 1e6, p = 0.4, lambda = 3)
# Para obtener cuántos valores "únicos" hay de x:
length(unique(V2$x))
# Para obtener la distribución condicional de N dado X = x no podemos usar la misma idea que
# en el caso discreto. Pero podemos tomar 0 < h << 1 y considerar la distribución condicional
# de N dado X en (x-h, x+h).
h1 <- 1e-2
x1 <- 0.83
NcondX <- V2 %>%
    filter(x > x1-h1 & x <x1+h1)
hist(
    NcondX$n, freq = FALSE, 
    breaks = seq(from = min(NcondX$n)-0.5, to = max(NcondX$n)+0.5, by = 1)
)
xaux <- 1:8
yaux <- dpois(x = xaux-1, lambda = 3*0.6*0.83)
points(xaux, yaux, pch = 20)

# Recordatorio  de %>%
c(1,2,3) %>% sum()

# Vamos a hacer el ejercicio 10 de forma numérica: obtener P(X + Y > K)
# cuando X ~ geom(p1) y Y ~ geom(p2)
# Observemos que P(X + Y > K) = E[ 1_{X + Y > K} ]
# Lo que podemos hacer es simular muchos valores de X y Y, y ver si son mayores que K 
# y generar las indicadoras para después promediarlas
sol_num <- function(n, p1, p2, K){
    X <- rgeom(n = n, prob = p1) + 1
    Y <- rgeom(n = n, prob = p2) + 1
    ind_aux <- as.numeric(X + Y > K)
    return(
        mean(ind_aux)
    )
}

sol_num(n = 1e6, p1 = 0.3, p2 = 0.4, K = 12)
sol_num(n = 1e6, p1 = 0.1, p2 = 0.2, K = 20)

# Resolvamos el ejercicio 18 de forma numérica
personas_escogen_objetos <- function(n, K, m){
    X <- data.frame(
        A = numeric(length = n),
        N = numeric(length = n),
        U = numeric(length = n)
    )
    for(i in 1:n){
        j1 <- sample(x = 1:K, size = m)
        j2 <- sample(x = 1:K, size = m)
        X$A[i] <- length(intersect(j1, j2))
        X$N[i] <- K - length(union(j1,j2))
        X$U[i] <- K - X$A[i] - X$N[i]
    }
    return(X)
}

X1 <- personas_escogen_objetos(n = 1e5, K = 10, m = 3)
# Summary nos da la información columna por columna, lo que nos importa son las medias
summary(X1)
