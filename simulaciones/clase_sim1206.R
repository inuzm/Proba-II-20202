# 12/06
# inm

# Ayer vieron un método para obtener simulaciones de vectores aleatorios
# (Z1, Z2) con entradas independientes cuya distribución es normal estándar
# a partir de dos variables aleatorias uniformes U_1 y U_2 ~ U(0,1)
# Z1 = sqrt(-2log(U_1)) * cos(2*pi*U_2)
# Z2 = sqrt(-2log(U_1)) * sin(2*pi*U_2)

box_muller <- function(n){
    u1 <- runif(n = n)
    u2 <- runif(n = n)
    z1 <- sqrt(-2*log(u1)) * cos(2*pi*u2)
    z2 <- sqrt(-2*log(u1)) * sin(2*pi*u2)
    return( data.frame(z1 = z1, z2 = z2) )
}

Z <- box_muller(n = 1e5)
colMeans(Z)
var(Z)

plot(Z, pch = '.', col = scales::alpha("black", 0.1)) # Hacemos más transparentes los puntos
contour(MASS::kde2d(Z$z1, Z$z2), nlevels = 10, col = "red", add = TRUE, drawlabels = FALSE) # Estimando la densidad 2-dimensional y graficando las curvas de nivel

xaux <- seq(from = -2, to = 2, length = 1e3)
yaux <- dnorm(xaux)
plot(xaux, yaux, type = 'l')

# Podemos modificar el método de Box-Muller para generar normales
# pero cuya varianza no sea 1. Para lambda_1, lambda_2 > 0 definimos
# X1 = sqrt(-log(U_1) / lambda_1) * cos(2*pi*U_2)
# X2 = sqrt(-log(U_1) / lambda_2) * sin(2*pi*U_2)
# Se puede ver que
# X1 = (2*lambda_1)^{-1/2} Z_1
# X2 = (2*lambda_2)^{-1/2} Z_2
# de donde X_1 y X_2 son independientes y además 
# X_i ~ N(0, (2*lambda_i)^{-1}).
# Esto último es consecuencia de que si Z ~ N(0,1) y Y = c Z,
# entonces Y ~ N(0, c^2).

box_muller_mod <- function(n, lambda1, lambda2){
    u1 <- runif(n = n)
    u2 <- runif(n = n)
    z1 <- sqrt(-log(u1)/lambda1) * cos(2*pi*u2)
    z2 <- sqrt(-log(u1)/lambda2) * sin(2*pi*u2)
    return( data.frame(z1 = z1, z2 = z2) )
}

Z2 <- box_muller_mod(n = 1e5, lambda1 = 0.3, lambda2 = 1.7)
(2*0.3)^-1; (2*1.7)^-1
var(Z2)

plot(Z2, pch = '.', xlim = c(-6, 6), ylim = c(-6, 6), col = scales::alpha("black", 0.1))
contour(MASS::kde2d(Z2$z1, Z2$z2), nlevels = 10, col = "red", add = TRUE, drawlabels = FALSE)

# Lo que vemos es que las curvas de nivel de una densidad normal
# bivariada son elipses y lo que va a pasar si quitamos el 
# supuesto de independencia es que nuestras elipses van a 
# ser rotadas

# Una consecuencia de Box—Muller es tener una fórmula "sencilla"
# para simular variables aleatorias Cauchy, pues si tenemos 
# Z_1 y Z_2 normales estándar independientes, entonces 
# Z_2/Z_1 ~ Cauchy(0,1)
# La densidad Cauchy(0,1) es 1/(π * (1+x^2))
# De Box-Muller Z_2 / Z_1 = tan(2*π*U_2)

generar_cauchy <- function(n){
    u <- runif(n = n)
    z <- tan(2*pi*u)
    return(z)
}

x1 <- generar_cauchy(n = 1e5)
mean(x1)
x2 <- generar_cauchy(n = 1e5)

plot(x1, x2, pch = '.', xlim = c(-5, 5), ylim = c(-5, 5), , col = scales::alpha("black", 0.1))
contour(MASS::kde2d(Z2$z1, Z2$z2), nlevels = 10, col = "red", add = TRUE, drawlabels = FALSE)
