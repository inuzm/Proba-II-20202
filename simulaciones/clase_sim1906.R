# Ejercicio 1
simula1 <- function(n){
    x <- runif(n = n)
    y <- runif(n = n)
    z <- runif(n = n)
    return(
        data.frame(
            w = x + y + z,
            u = x + y - z,
            v = x - y - z
        )
    )
}

X <- simula1(1e5)
par(mfrow = c(2,2))
hist(X$w, freq = FALSE, breaks = 50, main = "Densidad de X+Y+Z")
#
hist(X$u, freq = FALSE, breaks = 50)
hist(-X$v, freq = FALSE, breaks = 50, add = TRUE)
#
hist(X$u, freq = FALSE, breaks = 50, main = "Densidad de X+Y-Z")
hist(X$v, freq = FALSE, breaks = 50, main = "Densidad de X-Y-Z")
# Parece ser que la densidad de X-Y-Z es igual que la densidad de
# -(X+Y-Z). ¿Por qué pasa esto? Tip: las variables X, Y y Z tienen
# la misma distribución


# Ejercicio 2
# Z = X*Y
# W = X <- auxiliar
# ∫f_{W, Z}(w, z) dw
# {0 < x^3(w, z) < y(w, z) < √x(w, z) < 1}
kaux <- function(x){ x*sqrt(x) - x^4 }
simula2x <- function(n){
    x <- numeric(n)
    for(i in 1:n){
        y <- runif(2)
        while(y[2] >= kaux(y[1]) / kaux((3/8)^(2/5)) ){
            y <- runif(2)
        }
        x[i] <- y[1]
    }
    return(x)
}
simula2 <- function(n){
    x <- simula2x(n = n)
    y <- runif(n = n, min = x^3, max = sqrt(x))
    return(
        data.frame(
            x = x,
            y = y
        )
    )
}

X <- simula2(n = 1e5)
dev.off()
plot(X, pch = '.')
xy <- X$x * X$y
aux <- X$x
plot(xy, aux, pch = '.')
aux2 <- X$y
plot(xy, aux2, pch = '.')
hist(xy, freq = FALSE, breaks = 50)
lines(density(xy))

# Ejercicio 3
simula_unif_disco <- function(n){
    X <- data.frame(x = numeric(n), y = numeric(n))
    for(i in 1:n){
        x <- runif(2, min = -1, max = 1)
        while(sum(x^2) >= 1){
            x <- runif(2, min = -1, max = 1)
        }
        X[i,] <- x
    }
    return(X)
}
simula3 <- function(n){
    X <- simula_unif_disco(n = n)
    R <- sqrt(rowSums(X^2))
    Theta <- atan2(X$y, X$x)
    return(
        data.frame(
            r = R,
            theta = Theta
        )
    )
}

X <- simula3(n = 1e4)
plot(X, pch = 20, cex = 0.3)
hist(X$r, freq = FALSE, breaks = 50)
hist(X$theta, freq = FALSE, breaks = 50)

# Ejercicio 4
simula4 <- function(n, lambda){
    y <- rgamma(n = n, shape = 2, rate = lambda)
    x <- runif(n = n, min = 0, max = y)
    return(
        data.frame(
            x = x,
            y = y
        )
    )
}

X <- simula4(n = 1e5, lambda = 9)
plot(X, pch = '.')
y1 <- X$y
y2 <- X$x / (X$y - X$x)
plot(y1, y2, pch = '.')
cor(y1, y2)
par(mfrow = c(2,2))
plot(y1, 1/(y2 + 1), pch = '.')
#plot.new()
hist(y1, freq = FALSE, breaks = 50)
hist(1/(y2+1), freq = FALSE, breaks = 50)

#abline(a = 1, b = -1, col = "red", lwd = 2)
#abline(a = 0, b = 0, col = "red", lwd = 2)

# Ejercicio 5
# X = √(R^2-W^2) * cos(2π theta)
# Y = √(R^2-W^2) * sen(2π theta)
# Z = W
# Obtenemos la densidad de (W, R, theta) y hay que marginalizar theta
simula_unif_esfera <- function(n){
    X <- data.frame(x = numeric(n), y = numeric(n), z = numeric(n))
    for(i in 1:n){
        x <- runif(3, min = -1, max = 1)
        while(sum(x^2) >= 1){
            x <- runif(3, min = -1, max = 1)
        }
        X[i,] <- x
    }
    return(X)
}
simula5 <- function(n){
    X <- simula_unif_esfera(n = n)
    R <- sqrt(rowSums(X^2))
    return(
        data.frame(
            r = R,
            z = X$z
        )
    )
}

X <- simula5(1e5)
plot(X, pch = '.')
abline(0, 1, col = "cyan")
abline(0, -1, col = "cyan")

# Ejercicio 7
simula7 <- function(n){
    u <- runif(n = n, min = 0, max = 2*pi)
    z <- rexp(n = 1, rate = 1)
    return(
        data.frame(
            x = sqrt(2*z) * cos(u),
            y = sqrt(2*z) * sin(u)
        )
    )
}

# Ejercicio 8
simula_unif_elipse <- function(n, a = 3, b = 2){
    X <- data.frame(x = numeric(n), y = numeric(n))
    for(i in 1:n){
        x <- runif(2, min = -c(a, b), max = c(a, b))
        while(sum((x / c(a, b))^2) >= 1){
            x <- runif(2, min = -c(a, b), max = c(a, b))
        }
        X[i,] <- x
    }
    return(X)
}
simula8 <- function(n, a = 3, b = 2){
    X <- simula_unif_elipse(n = n, a = a, b = b)
    R <- sapply(1:n, function(k) sqrt(sum( (X[k,] / c(a,b))^2 )) )
    Theta <- atan2(X$y/b, X$x/a)
    return(
        data.frame(
            r = R,
            theta = Theta
        )
    )
}

# Ejercicio 10
simula10 <- function(n, rho){
    u <- rnorm(n = n)
    v <- rnorm(n = n)
    z <- rho * u + sqrt(1-rho^2) * v
    return(data.frame(
        u = u,
        v = v,
        z = z
    ))
}

X <- simula10(1e5, rho = -1/2)
hist(X$z, freq = FALSE, breaks = 50)
plot(X$u, X$z, pch = '.', col = scales::alpha("black", 0.1))
