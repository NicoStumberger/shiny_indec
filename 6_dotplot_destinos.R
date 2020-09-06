
library(tidyverse)
library(readxl)
library(plotly) # para los graficos interactivos
library(lubridate) # para manejar fechas

# Carga de Datasets ----
expo_shiny <- readRDS("data/expo_shiny_post2010.RDS") %>% 
        filter(!is.na(fob))

skimr::skim(expo_shiny)
desc_ncm <- readRDS("data/desc_ncm.RDS")

# Formula para que los meses en ingles, aparezcan en espanol en el server
english_months <- c("jan", "feb", "mar", "apr", 
                    "may", "jun", "jul", "aug", 
                    "sep", "oct", "nov", "dec")
spanish_months <- c("Ene", "Feb", "Mar", "Abr", 
                    "May", "Jun", "Jul", "Ago", 
                    "Sep", "Oct", "Nov", "Dic")

# Tabla diccionariio de meses para facilitar la escritura en espanol en los titulos

dic_meses <- tribble(
        ~mes, ~Mes, ~Mes_completo,
        1, "Ene", "Enero",
        2, "Feb", "Febrero",
        3, "Mar", "Marzo",
        4, "Abr", "Abril",
        5, "May", "Mayo",
        6, "Jun", "Junio",
        7, "Jul", "Julio",
        8, "Ago", "Agosto",
        9, "Sep", "Septiembre",
        10, "Oct", "Octubre",
        11, "Nov", "Noviembre",
        12, "Dic", "Diciembre"
)


# quality_var_tibble$qual_mostrar[quality_var_tibble$quality == input$quality]


to_spanish_dict <- spanish_months
names(to_spanish_dict) <- english_months

translate_date <- function(date, output_lang = "es"){
        if(output_lang == "es"){
                str_replace_all(tolower(date), to_spanish_dict)
        }
}



# Armado de tablas

categ <- unique(expo_shiny$cat_omc_1)

subcat <- unique(expo_shiny$cat_omc_2)

ano_1 <- expo_shiny %>%
        filter(ano == max(ano)) %>%
        left_join(desc_ncm) %>%
        mutate(Mes = translate_date(month(mes, label = TRUE, abbr = TRUE)))



ano_0 <- expo_shiny %>%
        filter(ano == max(ano) - 1 & mes <= max(ano_1$mes)) %>%
        left_join(desc_ncm) %>%
        mutate(Mes = translate_date(month(mes, label = TRUE)))

mesec <- max(unique(ano_1$mes))

color_cat_1 <- c("#00B1AC", "#00ADE6","#E9644C", "#7F7F7F")




# Dotplot destinos ----

# Aunq para tener los datos reales a estos destinos deberia usar el otro
# archivo de INDEC y no expo_shiny, uso este ultimo porque 
# en definitiva lo que quiero hacer es: en base a los productos/sectores
# seleccionados, conocer los destinos. Esto debe ser aclarado en un disclaimer:
# si se selecciona la totalidad de subcategorias no van a aparecer los montos
# reales totales exportados por argentina a esos destinos, porque no se suman
# los confidenciales.

# Pretunta: escondo el Confidencial y lo aclaro o lo dejo a los efectos de hacer
# visible el problema del secreto estadistico para hacer analisis?

dotplot_destinos <- expo_shiny %>% 
        filter(ano >= max(ano) - 1 & mes <= mesec) %>% 
        group_by(ano, nom_indec) %>% 
        summarise(y = round(sum(pnet_kg, na.rm = TRUE) / 1000000000, digits = 2)) %>%
        pivot_wider(names_from = ano, values_from = y) %>% 
        rename(ano_0 = 2,
               ano_1 = 3) %>% 
        mutate(var = (ano_1 / ano_0  - 1) * 100) %>% 
        filter(nom_indec != "Confidencial") %>% 
        slice_max(order_by = ano_1, n = 10)


# Y el resto del codigo que es similar/identico al dotplot de subcategorias.


# Dotplot productos ----

# Mismo dilema: mantengo o quito Confidencial?

dotplot_productos <- expo_shiny %>% 
        filter(ano >= max(ano) - 1 & mes <= mesec) %>% 
        group_by(ano, ncm) %>% 
        summarise(y = sum(fob, na.rm = TRUE) / 1000000) %>% 
        pivot_wider(names_from = ano, values_from = y) %>% 
        rename(ano_0 = 2,
               ano_1 = 3) %>% 
        mutate(var = (ano_1 / ano_0  - 1) * 100) %>% 
        filter(ncm != "99999999") %>% 
        slice_max(order_by = ano_1, n = 10)

# Hacer left_join de cat_omc_1 y de descripcion de ncm