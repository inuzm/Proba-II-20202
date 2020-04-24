# Ayudantía 24 abril
# INM
# 

# Cargar las librerías
library(tidyverse)
library(lubridate)
library(rcartocolor)
library(readxl)
library(curl)
library(patchwork)

# Descargar y leer los datos del coronavirus
# Crear archivos temporales
tempo <- tempfile()
tempo2 <- tempfile()
# Descargar datos del covid
download.file(
    url = "http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip",
    destfile = tempo
)
# Desscomprimir los datos y gaurdarlos en el segundo archivo temporar
unzip(zipfile = tempo, exdir = tempo2)
# Ver los archivos que se encuentras dentro del zip que descargamos
list.files(path = file.path(tempo2)) 
# Con base en lo anterior, modificar "200416COVID19MEXICO.csv"
datos_cov <- read.csv(file = file.path(tempo2, list.files(path = file.path(tempo2))[1]), 
                      stringsAsFactors = FALSE)
# Desenlazar y quitar los archivos temporales
unlink(tempo)
unlink(tempo2)
rm(tempo)
rm(tempo2)

# Para descargar las claves haremos un procedimiento similar
claves <- read_xlsx("datos_covid/diccionario_datos_covid19/Catalogos_0412.xlsx", sheet = 8)

# Poner las variables de fechas como fechas
datos_cov <- datos_cov %>%
    mutate(
        FECHA_ACTUALIZACION = lubridate::ymd(FECHA_ACTUALIZACION),
        FECHA_INGRESO = lubridate::ymd(FECHA_INGRESO),
        FECHA_SINTOMAS = lubridate::ymd(FECHA_SINTOMAS),
        FECHA_DEF = lubridate::ymd(FECHA_DEF)
    )

# Poner las entidades por sus abreviaturas y no de forma numérica
claves <- claves %>%
    mutate(CLAVE_ENTIDAD = as.integer(CLAVE_ENTIDAD))
datos_cov <- datos_cov %>%
    mutate(ENTIDAD_UM = claves$ABREVIATURA[ENTIDAD_UM])

# Gráfica de casos diarios a nivel nacional
datos_cov %>%
    filter(RESULTADO == 1) %>% 
    group_by(FECHA_INGRESO) %>%
    tally() %>%
    ggplot(aes(x = FECHA_INGRESO, y = n)) +
    geom_col() +
    theme_minimal() +
    labs(x = "Fecha de ingreso", y = "Casos diarios") +
    scale_x_date(labels = scales::date_format("%d-%m"))

# Gráfica de casos acumulados a nivel nacional
datos_cov %>%
    filter(RESULTADO == 1) %>% 
    group_by(FECHA_INGRESO) %>%
    tally() %>%
    ggplot(aes(x = FECHA_INGRESO)) +
    geom_col(aes(y = n), fill = "hotpink") +
    geom_line(aes(y = cumsum(n)), col = "darkorchid") +
    theme_minimal() +
    labs(x = "Fecha de ingreso", y = "Casos acumulados") +
    scale_x_date(labels = scales::date_format("%d-%m"))

# Modelo logístico
datos_conf <- datos_cov %>%
    filter(RESULTADO == 1) %>%
    group_by(FECHA_INGRESO) %>%
    tally() %>%
    mutate(acumulados = cumsum(n))

ggplot(datos_conf, aes(x = FECHA_INGRESO, y = acumulados)) +
    geom_point() +
    theme_minimal() +
    labs(x = "Fecha de ingreso", y = "Casos acumulados") +
    scale_x_date(labels = scales::date_format("%d-%m"))

# Vamos a hacer un modelo logístico
modelo_logistico <- nls(
    acumulados ~ SSlogis(FECHA_INGRESO, ASym, xmid, scal), 
    data = datos_conf
)
# No hace el modelo porque la variable FECHA_INGRESO es una variable en formato fecha.
# Por lo tanto tenemos que crear una variable que represente las fechas de forma
# numérica.
# Se pone el primer día con infectados como día cero.
datos_conf$fecha_numerica <- as.numeric(datos_conf$FECHA_INGRESO) - 
    min(as.numeric(datos_conf$FECHA_INGRESO))
# Ahora sí hacemos un modelo logístico.
modelo_logistico <- nls(
    acumulados ~ SSlogis(fecha_numerica, Asym, xmid, scal),
    data = datos_conf
)

# Resumen general del modelo
summary(object = modelo_logistico)
# Con esto sacamos los coeficientes
coef(object = modelo_logistico)
# Parámetros para bandas de confianza
confint(object = modelo_logistico, level = 0.99)
# Vamos a crear las curvas logísticas
# Para esto obtenemos los coeficientes del modelo logístico ajustado
params1 <- coef(object = modelo_logistico)
# También pondremos una banda de confianza al nivel 99 % (lo que eso signifique).
# Parámetros de curva inferior
params_inf <- confint(object = modelo_logistico, level = 0.99)[,1]
# Parámetros de curva superior
params_sup <- confint(object = modelo_logistico, level = 0.99)[,2]

# Una función logística es de la forma
# Asym /(1 + exp(-(x-xmid) / scal))
# Creamos una función que cree funciones logísticas con los parámetros
# obtenidos anteriormente, usando que
#   params[1] := Asym
#   params[2] := xmid
#   params[3] := scal
hacer_modelo_logistico <- function(params){
    f1 <- function(t1){
        return(
            params[1] / (1 + exp( (params[2] - t1) / params[3] ))
        )
    }
    return(f1)
}

# Generamos las funciones con la función que hicimos
logis1 <- hacer_modelo_logistico(params = params1)
logis_inf <- hacer_modelo_logistico(params = params_inf)
logis_sup <- hacer_modelo_logistico(params = params_sup)

# Para gráficar las curvas que acabamos de crear
t_aux <- seq(from = 0, to = 150, length.out = 1e4)
y1 <- logis1(t1 = t_aux)
y2 <- logis_sup(t1 = t_aux)
y3 <- logis_inf(t1 = t_aux)

# Gráficas con base R
plot(t_aux, y2, type = "l", lty = 3, 
     xlab = "Fecha de ingreso desde el primer paciente", ylab = "Casos acumulados",
     main = "Ajuste de curva logística")
lines(t_aux, y1, col = "red")
lines(t_aux, y3, lty = 3)
points(datos_conf$fecha_numerica, datos_conf$acumulados, pch = 20)

# Gráfica con ggplot
Logis <- data.frame(
    t = t_aux,
    y1 = y1,
    y2 = y2,
    y3 = y3
)
ggplot(Logis, aes(x = t)) +
    geom_line(aes(y = y2), linetype = 3) +
    geom_line(aes(y = y3), linetype = 3) +
    geom_line(aes(y = y1), col = "red") +
    geom_point(data = datos_conf, aes(x = fecha_numerica, y = acumulados)) +
    theme_minimal() +
    labs(
        x = "Fecha de ingreso desde el primer paciente",
        y = "Casos acumulados",
        title = "Ajuste de curva logística"
    )

# Queda como ejercicio repetir lo de arriba con la fecha de síntomas

# Vamos a hacer un poco de regresión lineal (brevísima introducción por tiempo)
# La idea general es que si tenemos datos
# (x_1, y_1),...,(x_n,y_n)
# y creemos que y_i = a + b * x_i + e_i
# Generamos x
x <- 0:40
# Generamos y por medio de y = 3 * x + e, con e ~ normal(0,sqrt(10))
y1 <- 3*x + rnorm(n = length(x), mean = 0, sd = 10)
# Graficamos los datos y debería de parecer que hay una tendencia lineal
plot(x, y1, pch = 20)
# Los juntamos en un data.frame para hacer la regresión lineal
X <- data.frame(
    x = x,
    y = y1
)
# Regresión lineal
lm1 <- lm(y ~ x, data = X)
# Resumen de la regresión
summary(lm1)
# Veamos qué tan bien ajustó
# Los datos
plot(x, y1, pch = 20)
# La línea de regresión
abline(coef(lm1), col = "red")
# Las bandas de confianza al 99 %
abline(confint(object = lm1, level = 0.99)[,1], lty = 3)
abline(confint(object = lm1, level = 0.99)[,2], lty = 3)
