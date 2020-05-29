# 29/05
# inm
# 

library(dplyr)
library(reshape2)
library(knitr)

# Tenemos N pisos en los que se pueden bajar personas que entren a un elevador y sabemos que 
# en la planta baja entran Y personas, que es una variable aleatoria Poisson(lambda).
# Primero vamos a simular Y, y después debemos simular Y variables aleatorias uniformes entre 1 y N
# y luego contamos cuantos valores distintos hubo, que sería el número de pisos en los que se bajaron personas.
el_de_los_pisos <- function(nsim, lambda, N){
    resultado <- numeric(length = nsim)
    for(i in 1:nsim){
        Y <- rpois(n = 1, lambda = lambda)
        U <- ceiling(runif(n = Y, min = 0, max = 1) * N)
        resultado[i] <- length(unique(U))
    }
    return(resultado)
}

x1 <- el_de_los_pisos(nsim = 1e4, lambda = 10, N = 10)
# El número esperado de pisos en el que se bajan las personas es N * (1 - exp(-lambda /N))
# Veamos si la media de las simulaciones se parece a la media teórica
10 * (1 - exp(-10/10))
summary(x1)

x2 <- el_de_los_pisos(nsim = 1e4, lambda = 50, N = 70)
70 * (1 - exp(-50/70))
mean(x2)

# Bajo la estrategia i, si la suma suma de dados es menor o igual que i, continuamos jugando y si la suma
# de dados es mayor que i, entonces paramos. Todo esto suponiendo que la suma no sea 7 porque en este caso
# se acaba el juego y ganamos 0.
suma_dados <- function(estr, nsim){
    ganancia <- numeric(length = nsim)
    for(i in 1:nsim){
        suma <- 0
        while(suma <= estr & suma != 7){
            suma <- sum(ceiling(runif(n = 2, min = 0, max = 1) * 6))
        }
        if(suma == 7){
            ganancia[i] <- 0
        } else {
            ganancia[i] <- suma
        }
    }
    return(ganancia)
}

# Vamos a obtener simulaciones de ganancias bajo posibles estrategias
est2 <- suma_dados(estr = 2, nsim = 1e4)
est3 <- suma_dados(estr = 3, nsim = 1e4)
est4 <- suma_dados(estr = 4, nsim = 1e4)
est5 <- suma_dados(estr = 5, nsim = 1e4)
est6 <- suma_dados(estr = 6, nsim = 1e4)
est7 <- suma_dados(estr = 7, nsim = 1e4)
est8 <- suma_dados(estr = 8, nsim = 1e4)
est9 <- suma_dados(estr = 9, nsim = 1e4)
est10 <- suma_dados(estr = 10, nsim = 1e4)
est11 <- suma_dados(estr = 11, nsim = 1e4)
est12 <- suma_dados(estr = 12, nsim = 1e4)

# Veamos la media de las estrategias
mean(est2)
mean(est3)
mean(est4)
mean(est5)
mean(est6)
mean(est7)
mean(est8)
mean(est9)
mean(est10)
mean(est11)
mean(est12)

# Vamos a hacer una tabla en la que tengamos la cantidad de veces que ganamos cierta cantidad
# para cada una de las estrategias que simulamos
tablita <- data.frame(
    ganancia = c(
        est2, est3, est4, est5, est6, est7, est8, est9, est10, est11, est12
    ),
    estrategia <- rep(2:12, each = 1e4)
)

tablita %>%
    acast(formula = estrategia ~ ganancia)

# Si queremos hacer una tablita para LaTeX:
tablita %>%
    acast(formula = estrategia ~ ganancia) %>%
    kable(format = "latex", booktabs = TRUE)

# Cadena de rachas
# p es la probabilidad de que salga águila y queremos contar cuántos lanzamientos nos vamos
# a tardar en promedio para que salga una racha de k águilas seguidas
rachas <- function(nsim, p, k){
    resultado <- numeric(length = nsim)
    for(i in 1:nsim){
        r <- 0
        lanzamientos <- 0
        while (r < k) {
            volado <- (runif(n = 1, min = 0, max = 1) < p)
            lanzamientos <- lanzamientos + 1
            if(volado){
                r <- r + 1
            } else {
                r <- 0
            }
        }
        resultado[i] <- lanzamientos
    }
    return(resultado)
}

# Simulemos la cantidad de lanzamientos necesarios para obtener 3 águilas seguidas si 
# la probabilidad de obtener un águila es 1/3
rachas1 <- rachas(nsim = 1e4, p = 1/3, k = 3)
# La cantidad esperada de lanzamientos es 1/p + 1/p^2 + 1/p^3
3 + 9 + 27
mean(rachas1)

rachas2 <- rachas(nsim = 1e4, p = 1/2, k = 5)
# En general si queremos k águilas seguidas, el promedio de lanzamientos que necesitamos hacer es de
# 1/p + 1/p^2 + ... + 1/p^k
2 + 4 + 8 + 16 + 32
mean(rachas2)
