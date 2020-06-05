# Ayudantía 05/06
# inm
#
#
# Dos personas y un banco lanzan un dado justo y la persona 1 le gana al banco si su tirada es mayor que 
# la del banco, y lo mismo para la persona 2. 
la_del_banco <- function(nsim, k){
    dado_persona_1 <- ceiling(k * runif(n = nsim))
    dado_persona_2 <- ceiling(k * runif(n = nsim))
    dado_banco <- ceiling(k * runif(n = nsim))
    gana_1 <- as.numeric(dado_persona_1 > dado_banco)
    gana_2 <- as.numeric(dado_persona_2 > dado_banco)
    aux <- cov(gana_1, gana_2)
    cat(
        sprintf(
            "La covarianza entre I_1 e I_2 es: %0.6f\n", aux
        )
    )
}

la_del_banco(1e5, 6)
35/432

# cov como argumentos o dos vectores y calcula la covarianza entre ellos, toma una matriz
# y calula la covarianza entre columnas y hace lo mismo con un data.frame


# En el ejercicio 1 tenemos que condicional en U_{k-1} = u, U_k ~ Uniforme(0, u) y queremos
# la esperanza y varianza de U_k.
uniformes_anidadas <- function(nsim, k){
    U <- rep(1, nsim)
    for(i in 1:k){
        U <- runif(n = nsim, max = U) # U_k ~ Unif(0, U_{k-1})
    }
    return(U)
}

U1 <- uniformes_anidadas(1e5, k = 10)
mean(U1)
2^-10
var(U1)
# Queda pendiente ver que sí coincide con la varianza teórica :(

# Si U ~ Uniforme(0,1), entones V = aU es tal que V ~ Uniforme(0, a)
# Si U ~ Uniforme(0,1), entones V = aU + b es tal que V ~ Uniforme(b, b+a)
# Si tomamos U_1 = u y vemos que U_2 ~ Uniforme(0, u) entonecs U_2 = u * V_2 con
# V_2 ~ Uniforme(0,1), entonces parece que U_2 = V_2 * U_1 con V2 independiente de U_1.
# Si tomamos U_2 = u y vemos que U_3 ~ Uniforme(0, u) entonecs U_3 = u * V_3 con 
# V_3 ~ Uniforme(0,1)
# Bajo este razonamiento, U_3 = U_2 * V_3 = V_3 * V_2 * U_1.
# Parece que U_k es el producto de k variables aleatorias independientes uniformes(0,1)
# ¿Es esto cierto? Podemos darnos una idea con simulaciones

producto_de_uniformes <- function(nsim, k){
    U <- rep(1, nsim)
    for(i in 1:k){
        U <- U * runif(n = nsim)
    }
    return(U)
}

U2 <- producto_de_uniformes(nsim = 1e5, k = 10)
mean(U1)
mean(U2)
var(U1)
var(U2)

# Vamos a comparar las densidades de las simulaciones
plot(density(U1))
lines(density(U2), col = "red")
# Pruebita gráfica de si dos distribuciones son similares
# Otra pruebita se puede hacer con QQ-plots
plot(quantile(x = U1, probs = seq(from = 0, to = 1, length = 1e3)),
     quantile(x = U2, probs = seq(from = 0, to = 1, length = 1e3)),
     pch = '.')
abline(a = 0, b = 1, col = "red")
