
library(tidyverse)
library(hrbrthemes)
library(plotly)
library(lubridate)

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
        summarise(fob_mm = sum(fob) / 1000000)

var_subcat %>% 
        ggplot(aes(as_factor(ano), fob_mm, group = cat_omc_2)) +
        geom_line(aes(color = cat_omc_1)) +
        geom_point(aes(color = cat_omc_1), 
                       size = 3.5
                   ) +
        scale_color_discrete(labels = c("Productos agrícolas",
                             "Manufacturas",
                             "Combustibles y prod. ind. extrac.",
                             "Otros")
                             ) +
        scale_y_log10() +
        theme_minimal() +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              panel.grid = element_blank(),
              axis.text.y = element_blank()) +
        labs(x = "",
             y = "")
        
# Convertir cat_omc a factors con niveles ordenados LISTO
# Quitar legend de size LISTO
# Modificar texto de Combustible, acortarlo LISTO
# sumar texto de cada subcat, fob en mm y var interanual. Todavia lo ten que hacer
# Cambiar paleta de colores: gris para Otros y verde, azul y amarillo/naranja
# El color del punto deberia ser una variable que muestre el periodo mayor o 
#   el signo de la variacion interanual
# convertir a ggplotly
