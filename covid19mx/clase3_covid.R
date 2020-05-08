# 8 de mayo
# INM

# Cargamos librerías
library(tidyverse)
library(lubridate)
library(scales)

# Vamos a leer todas las bases de datos y juntarlas en un solo data.frame
datos <- list.files("datos_historicos/") %>%
    map_df(
        ~read.csv(
            file = paste0("datos_historicos/", .),
            stringsAsFactors = FALSE
        )
    )

# Solamente nos interesan los datos que sean positivos
datos_conf <- datos %>%
    filter(RESULTADO == 1) %>%
    mutate(
        FECHA_ACTUALIZACION = ymd(FECHA_ACTUALIZACION),
        FECHA_INGRESO = ymd(FECHA_INGRESO),
        FECHA_SINTOMAS = ymd(FECHA_SINTOMAS),
        FECHA_DEF = ymd(FECHA_DEF)
    )

# Quitar todos los datos
rm(datos)

# Vamos a ver cuántos casos hay reportados al día de actualización
casos_acum <- datos_conf %>%
    group_by(FECHA_ACTUALIZACION) %>%
    tally()
# Creamos una variable que cuente los casos diarios
casos_acum$inc <- c(NA, diff(casos_acum$n))

# Tomamos los datos más recientes entre todos
datos_recientes <- datos_conf %>%
    filter(FECHA_ACTUALIZACION == max(FECHA_ACTUALIZACION))

# Hacemos una gráfica de los casos confirmados por día en que se atendió
datos_recientes %>% 
    group_by(FECHA_INGRESO) %>%
    tally() %>%
    ggplot(aes(x = FECHA_INGRESO, y = cumsum(n))) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    scale_x_date(labels = scales::date_format("%d-%m")) +
    scale_y_continuous(breaks= pretty_breaks())
    theme(
        text=element_text(family="Times New Roman"),
        legend.position = "bottom"
    ) +
    labs(x = "Fecha de ingreso", y = "Casos acumulados")

# Gráfica de casos acumualados como se reporta en las conferencias verpertinas
casos_acum %>%
    ggplot(aes(x = FECHA_ACTUALIZACION, y = n)) +
    geom_line() +
    theme_minimal() +
    scale_x_date(labels = scales::date_format("%d-%m")) +
    scale_y_continuous(breaks= pretty_breaks())
theme(
    text=element_text(family="Times New Roman"),
    legend.position = "bottom"
) +
    labs(x = "Fecha de actualización", y = "Casos acumulados")

# Para hacer poder comparar las curvas de los casos confirmados, una considerando fechas
# de primeros síntomas y otra considerando las fechas en que son reportadas en las
# conferencias vespertinas

# Respuesta fácil
# Meter una segunda gráfica dentro de otra
datos_recientes %>% 
    group_by(FECHA_SINTOMAS) %>%
    tally() %>%
    ggplot(aes(x = FECHA_SINTOMAS, y = cumsum(n))) +
    geom_line() +
    geom_point() +
    geom_line(data = casos_acum, aes(x = FECHA_ACTUALIZACION, y = n), col = "red") +
    theme_minimal() +
    cale_x_date(labels = scales::date_format("%d-%m")) +
    scale_y_continuous(breaks= pretty_breaks())
theme(
    text=element_text(family="Times New Roman"),
    legend.position = "bottom"
) +
    labs(x = "Fecha", y = "Casos acumulados")

# Respuesta no tan fácil pero quizá más elegante
# 

# Creamos una tabla con los casos diarios y acumulados por fecha de primeros síntomas
# usando los datos más recientes de la SSalud
casos_acum_dr <- datos_recientes %>% 
    group_by(FECHA_SINTOMAS) %>%
    tally() %>%
    mutate(acum = cumsum(n))

# Vamos a crear una tabla que tenga los datos el día de actualización y el día en
# que se presentaron los primeros síntomas
datos_grafica <- data.frame(
    fechas = c(casos_acum$FECHA_ACTUALIZACION, casos_acum_dr$FECHA_SINTOMAS),
    casos_acum = c(casos_acum$n, casos_acum_dr$acum),
    casos_diarios = c(casos_acum$inc, casos_acum_dr$n),
    tipo = rep(c("a", "s"), times = c( nrow(casos_acum), nrow(casos_acum_dr) ))
)

# Graficamos los casos acumulados
datos_grafica %>%
    ggplot(aes(x = fechas, y = casos_acum, col = tipo)) +
    geom_line(size = 0.8) +
    theme_minimal() +
    scale_x_date(labels = scales::date_format("%d-%m")) +
    scale_y_continuous(breaks= pretty_breaks()) +
    scale_colour_manual(values = c("firebrick", "darkslateblue"), name = "", breaks = c("a", "s"), labels = c("Día de actualización", "Día de primeros síntomas")) +
    theme(
        text=element_text(family="Times New Roman"),
        legend.position = "bottom"
    ) +
    labs(x = "Fecha", y = "Casos acumulados")

# Graficamos los casos diarios, considerando del 13 de abril en adelante para comparar
datos_grafica %>%
    filter(fechas > min(datos_grafica$fechas[datos_grafica$tipo == 'a'])) %>%
    ggplot(aes(x = fechas, y = casos_diarios, fill = tipo)) +
    geom_col(position = "dodge") +
    theme_minimal() +
    scale_x_date(labels = scales::date_format("%d-%m")) +
    scale_y_continuous(breaks= pretty_breaks()) +
    scale_fill_manual(values = c("firebrick", "darkslateblue"), name = "", breaks = c("a", "s"), labels = c("Día de actualización", "Día de primeros síntomas")) +
    theme(
        text=element_text(family="Times New Roman"),
        legend.position = "bottom"
    ) +
    labs(x = "Fecha", y = "Casos diarios")
    