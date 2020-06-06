
library(tidyverse)
library(hrbrthemes)
library(plotly)
library(lubridate)
library(ggrepel) # para ggplot pero con ggplotly no sirve..

# Grafico variacion interanual

# Objetos ya en la app
expo_shiny <- readRDS("data/expo_shiny_post2010.RDS")

desc_ncm <- readRDS("data/desc_ncm.RDS")

categ <- unique(expo_shiny$cat_omc_1)

ano_1 <- expo_shiny %>% 
        filter(ano == max(ano)) %>% 
        left_join(desc_ncm) %>% 
        mutate(Mes = month(mes, label = TRUE, abbr = TRUE))

# deberia ser sobre expo_shiny
expo_shiny$cat_omc_1 <- as_factor(expo_shiny$cat_omc_1)
expo_shiny$cat_omc_2 <- factor(expo_shiny$cat_omc_2, 
                             levels = c(
                                     "Productos alimenticios",
                                     "Materias primas",
                                     "Menas y minerales",
                                     "Combustibles",
                                     "Metales no ferrosos",
                                     "Hierro y acero",
                                     "Productos químicos",
                                     "Otras semimanufacturas",
                                     "Maquinaria y equipo de transporte totales",
                                     "Textiles",
                                     "Prendas de vestir",
                                     "Otras manufacturas",
                                     "Otros"
                             ))



levels(ano_1$cat_omc_2)

unique(ano_1$cat_omc_1)


# Lo nuevo
mesec <- max(unique(ano_1$mes))

var_subcat <- expo_shiny %>% 
        filter(ano >= max(ano) - 1 & mes <= mesec) %>% 
        group_by(ano, cat_omc_1, cat_omc_2) %>% 
        summarise(fob_mm = sum(fob) / 1000000) %>% 
        group_by(cat_omc_2) %>% 
        mutate(var = (fob_mm / lag(fob_mm, n = 1) - 1) * 100) %>% 
        as_tibble()

levels(var_subcat$cat_omc_1)

color_cat_1 <- c("#00B1AC", "#00ADE6","#E9644C", "#7F7F7F")



graf_subcat <- var_subcat %>%
        ggplot(aes(as_factor(ano), fob_mm, group = cat_omc_2)) +
        geom_line(aes(color = cat_omc_1)) +
        geom_point(aes(color = cat_omc_1),
                   size = 3.5) +
        geom_text(
                data = subset(var_subcat, ano < max(ano)),
                aes(color = cat_omc_1),
                label = if_else(
                        str_length(unique(var_subcat$cat_omc_2)) > 22,
                        paste0(str_sub(
                                unique(var_subcat$cat_omc_2),
                                start = 1,
                                end = 15
                        ), "..."),
                        paste0(unique(var_subcat$cat_omc_2))
                ),
                nudge_x = -0.3,
                hjust = 0,
        ) +
        geom_text(
                aes(color = cat_omc_1),
                # alpha = if_else(is.na(var_subcat$var), 0, 1),
                label = if_else(is.na(var_subcat$var), paste0(""),
                                paste0(
                                        format(
                                                var_subcat$var,
                                                digits = 2,
                                                big.mark = ".",
                                                decimal.mark = ","
                                        ),
                                        " %"
                                )),
                nudge_x = 0.15,
                hjust = 0
        ) +
        scale_color_manual(values = color_cat_1) +
        scale_y_log10() +
        theme_minimal() +
        theme(
                legend.position = "none",
                legend.title = element_blank(),
                panel.grid = element_blank(),
                axis.text.y = element_blank()
        ) +
        labs(x = "",
             y = "")



ggplotly(graf_subcat)

       
# Convertir cat_omc a factors con niveles ordenados LISTO
# Quitar legend de size LISTO
# Modificar texto de Combustible, acortarlo LISTO

# sumar texto de cada subcat, LISTO
# y var interanual. LISTO
# fob??

# Cambiar paleta de colores: gris para Otros y verde, azul y amarillo/naranja LISTO



# El alpha o saturacion del punto deberia ser una variable que muestre el periodo mayor o 
#   el signo de la variacion interanual SE ME COMPLICA NO PONER SU LEYENDA


# convertir a ggplotly LISTO. 
# con algunos detalles para mejorar
