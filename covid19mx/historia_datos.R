#! /usr/bin/env Rscript
suppressPackageStartupMessages({
require(tidyverse)
require(lubridate)
require(scales)
require(readxl)
require(patchwork)
})

datos <- list.files("datos_historicos/") %>%
    map_df(
        ~read.csv(
            file = paste0("datos_historicos/", .),
            stringsAsFactors = FALSE
        )
    )

datos_conf <- datos %>%
    filter(RESULTADO == 1) %>%
    mutate(
        FECHA_ACTUALIZACION = ymd(FECHA_ACTUALIZACION),
        FECHA_INGRESO = ymd(FECHA_INGRESO),
        FECHA_SINTOMAS = ymd(FECHA_SINTOMAS),
        FECHA_DEF = ymd(FECHA_DEF)
    )

claves <- read_xlsx("diccionario_datos/Catalogos_0412.xlsx", sheet = 8)
claves <- claves %>%
    mutate(CLAVE_ENTIDAD = as.integer(CLAVE_ENTIDAD))
datos_conf <- datos_conf %>%
    mutate(ENTIDAD_UM = claves$ABREVIATURA[ENTIDAD_UM])
datos_conf <- datos_conf %>%
    mutate(ENTIDAD_RES = claves$ABREVIATURA[ENTIDAD_RES])

rm(datos)

casos_acum <- datos_conf %>%
    group_by(FECHA_ACTUALIZACION, FECHA_INGRESO, ENTIDAD_RES) %>%
    tally()

casos_acum <-  casos_acum %>%
    group_by(FECHA_ACTUALIZACION, ENTIDAD_RES) %>%
    mutate(acum = cumsum(n))

fechas <- unique(casos_acum$FECHA_ACTUALIZACION)

for(j in 1:length(claves$ABREVIATURA)){
    datos_loop <- casos_acum %>%
        filter(ENTIDAD_RES == claves$ABREVIATURA[j])
    if(nrow(datos_loop) > 0){
        k <- 1
        cat(sprintf("\nEstado: %s\n", claves$ENTIDAD_FEDERATIVA[j]))
        pb <- txtProgressBar(max = length(fechas), style = 3)
        for(i in 1:length(fechas)){
            png(paste0("gifs/diario/", claves$ABREVIATURA[j], sprintf("/%06d.png", k)), width = 1200, height = 900, res = 150)
            p1 <- datos_loop %>%
                filter(FECHA_ACTUALIZACION == fechas[i]) %>%
                ggplot(aes(x = FECHA_INGRESO, y = n)) +
                geom_col(width = 1, fill = "darkorchid", col = NA) +
                theme_minimal() +
                scale_x_date(labels = scales::date_format("%d-%m"), limits = range(casos_acum$FECHA_INGRESO), breaks = breaks_pretty(n = 5))  +
                scale_y_continuous(breaks= pretty_breaks()) +
                theme(
                    text=element_text(family="Times New Roman"),
                    legend.position = "bottom"
                ) +
                labs(
                    x = "Fecha de ingreso del caso", y = "Casos diarios",
                    subtitle = paste("Fecha de corte:", fechas[i]),
                    title = claves$ENTIDAD_FEDERATIVA[j]
                )
            plot(p1)
            dev.off()
            png(paste0("gifs/acum/", claves$ABREVIATURA[j], sprintf("/%06d.png", k)), width = 1200, height = 900, res = 150)
            p2 <- datos_loop %>%
                filter(FECHA_ACTUALIZACION == fechas[i]) %>%
                ggplot(aes(x = FECHA_INGRESO, y = acum)) +
                geom_line(col = "darkorchid") +
                theme_minimal() +
                scale_x_date(labels = scales::date_format("%d-%m"), limits = range(casos_acum$FECHA_INGRESO), breaks = breaks_pretty(n = 5)) +
                scale_y_continuous(breaks= pretty_breaks()) +
                theme(
                    text=element_text(family="Times New Roman"),
                    legend.position = "bottom"
                ) +
                labs(x = "Fecha de ingreso del caso", y = "Casos acumulados",
                subtitle = paste("Fecha de corte:", fechas[i]), title = claves$ENTIDAD_FEDERATIVA[j])
            plot(p2)
            dev.off()
            k <- k + 1
            setTxtProgressBar(pb, i)
        }
        close(pb)
    }
}
