# Ayudantía 16/04/2020
# Imanol NM
# 
# Descargar datos de:
# https://www.gob.mx/salud/documentos/datos-abiertos-152127
# Datos covid: http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip
# Diccionario de los datos: http://187.191.75.115/gobmx/salud/datos_abiertos/diccionario_datos_covid19.zip

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
datos_cov <- read.csv(file = file.path(tempo2, "200416COVID19MEXICO.csv"), 
                      stringsAsFactors = FALSE)
# Ver los datos
View(datos_cov)
# Desenlazar y quitar los archivos temporales
unlink(tempo)
unlink(tempo2)
rm(tempo)
rm(tempo2)

# Para descargar las claves haremos un procedimiento similar
tempo <- tempfile()
tempo2 <- tempfile()
download.file(
    url = "http://187.191.75.115/gobmx/salud/datos_abiertos/diccionario_datos_covid19.zip", 
    destfile = tempo
)
unzip(zipfile = tempo, exdir = tempo2)
list.files(path = file.path(tempo2))
claves <- read_xlsx(file.path(tempo2, "Catalogos_0412.xlsx"), sheet = 8)
View(claves)
unlink(tempo)
unlink(tempo2)
rm(tempo)
rm(tempo2)


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
    geom_col(fill = "hotpink") +
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

# Gráfica de casos diarios a nivel nacional, desagregando por estado
datos_cov %>%
    filter(RESULTADO == 1) %>%
    group_by(FECHA_INGRESO, ENTIDAD_UM) %>%
    tally() %>%
    ggplot(aes(x = FECHA_INGRESO, y = n, fill = ENTIDAD_UM)) +
    geom_col() +
    theme_minimal() +
    labs(x = "Fecha de ingreso", y = "Casos diarios") +
    scale_x_date(labels = scales::date_format("%d-%m")) +
    scale_fill_discrete(guide = guide_legend("Entidad"))

# Gráfica de casos diarios a nivel estatal
datos_cov %>%
    filter(RESULTADO == 1) %>%
    group_by(FECHA_INGRESO, ENTIDAD_UM) %>%
    tally() %>%
    ggplot(aes(x = FECHA_INGRESO, y = n, col = ENTIDAD_UM)) +
    geom_line() +
    theme_minimal() +
    labs(x = "Fecha de ingreso", y = "Casos diarios") +
    scale_x_date(labels = scales::date_format("%d-%m")) +
    scale_color_discrete(guide = guide_legend("Entidad"))

# Gráfica de los casos acumulados a nivel estatal
datos_cov %>%
    filter(RESULTADO == 1) %>%
    group_by(FECHA_INGRESO, ENTIDAD_UM) %>%
    tally() %>% 
    group_by(ENTIDAD_UM) %>%
    mutate(Casos_ac = cumsum(n)) %>%
    ggplot(aes(x = FECHA_INGRESO, y = Casos_ac, col = ENTIDAD_UM)) +
    geom_line() +
    theme_minimal() +
    labs(x = "Fecha de ingreso", y = "Casos acumulados") +
    scale_x_date(labels = scales::date_format("%d-%m")) +
    scale_color_discrete(guide = guide_legend("Entidad"))

# Creamos una nueva tabla en la que solamente tengamos los casos confirmados
# en la que habrá dos nuevas variables:
#  n: los casos diarios en una entidad
#  Casos_ac: los casos acumulados en una entidad
datos_conf <- datos_cov %>%
    filter(RESULTADO == 1) %>%
    group_by(FECHA_INGRESO, ENTIDAD_UM) %>%
    tally() %>% 
    group_by(ENTIDAD_UM) %>%
    mutate(Casos_ac = cumsum(n))
    
# Obtener las entidades que ya rebasaron 200 casos en total
rebasar200 <-  datos_conf %>%
    summarise(Casos_totales = max(Casos_ac)) %>%
    filter(Casos_totales > 200) %>%
    pull(ENTIDAD_UM)

# Tabla de las entidades que hayan rebasado los 200 casos en total
datos_conf_mas_200 <- datos_conf %>%
    filter(ENTIDAD_UM %in% rebasar200)

# Tabla de las entidades que no hayan rebasado los 200 casos en total
datos_conf_menos_200 <- datos_conf %>%
    filter( !(ENTIDAD_UM %in% rebasar200) )

p1 <- datos_conf_mas_200 %>%
    ggplot(aes(x = FECHA_INGRESO, y = Casos_ac, col = ENTIDAD_UM)) +
    geom_line() +
    theme_minimal() +
    scale_color_carto_d(palette = "Safe", guide = guide_legend("Entidad")) +
    labs(x = "Fecha de ingreso", y = "Casos acumulados") +
    scale_x_date(labels = scales::date_format("%d-%m"))

p2 <- datos_conf_menos_200 %>%
    ggplot(aes(x = FECHA_INGRESO, y = Casos_ac, col = ENTIDAD_UM)) +
    geom_line() +
    theme_minimal() +
    labs(x = "Fecha de ingreso", y = "Casos acumulados") +
    scale_x_date(labels = scales::date_format("%d-%m")) +
    scale_color_discrete(guide = guide_legend("Entidad"))

# Creamos un arreglo de gráficas
# Ojo, por la escala no son comparables
p1 / p2
