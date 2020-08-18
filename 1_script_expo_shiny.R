library(tidyverse)
library(readr)
library(readxl)

## Este script es para generar y procesar un solo dataset para utilizar en la web app 
## de exportaciones que pienso armar.

## Expo mensual historico hasta 2018.

files <- list.files(path = "data/historico/", pattern = ".csv", full.names = TRUE)

hist <- sapply(X = files, 
               FUN = read_delim,
               ";",
               skip = 1,
               col_names = FALSE,
               escape_double = FALSE,
               locale = locale(decimal_mark = ",", grouping_mark = "."),
               trim_ws = TRUE,
               simplify = FALSE) %>% 
        bind_rows(.id = NULL) %>% 
        rename(ano = X1,
               mes = X2,
               ncm = X3,
               pdes = X4,
               pnet_kg = X5,
               fob = X6) %>% 
        as_tibble() %>%
        mutate(hs6 = str_sub(string = ncm, start = 1, end = 6),
               ano = as.integer(ano),
               mes = as.integer(mes),
               pdes = as.character(pdes))

skimr::skim(hist)

# indec mensual 2020 y 2019. Los debo leer por separado porque los ultimos
# descargas de INDEC vienen con "s" como NA. 
# "s" indica que registra movimiento, pero el número de operadores es 
# insuficiente para mostrar el valor de la operación. 
# En este caso, esa transacción es parte del código “99999999” y su 
# descripción es: “Confidencial”
# 

files_2 <- list.files(path = "data/2020_2019/", pattern = ".csv", full.names = TRUE)

indec_2020_2019 <- sapply(X = files_2, 
               FUN = read_delim,
               ";",
               na = "s",
               skip = 1,
               col_names = FALSE,
               escape_double = FALSE,
               locale = locale(decimal_mark = ",", grouping_mark = "."),
               trim_ws = TRUE,
               simplify = FALSE) %>% 
        bind_rows(.id = NULL) %>% 
        rename(ano = X1,
               mes = X2,
               ncm = X3,
               pdes = X4,
               pnet_kg = X5,
               fob = X6) %>% 
        as_tibble() %>%
        mutate(hs6 = str_sub(string = ncm, start = 1, end = 6),
               ano = as.integer(ano),
               mes = as.integer(mes),
               pdes = as.character(pdes))

skimr::skim(indec_2020_2019)


# Bind rows

hist <- bind_rows(hist, indec_2020_2019)

## Dic de paises que arme pdes_unicos

dic_pais <- readRDS("data/dic_pais.RDS")

## Dic de OMC para las categorias (deberia actualizarlo con las sub sub categorias)

dic_omc <- readRDS("data/hs_omc.RDS") %>% 
        select(hs6, cat_omc_1, cat_omc_2)

## Jointear todos

expo_shiny <- hist %>% 
        left_join(dic_omc) %>% 
        left_join(dic_pais)

# Necesito las categorias convertidas en factors
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

skimr::skim(expo_shiny)     

## Desde 2002
saveRDS(expo_shiny, "output/expo_shiny.RDS")

## Desde 2010
expo_shiny %>% 
        filter(ano >= 2010) %>% 
        saveRDS("output/expo_shiny_post2010.RDS")

expo_shiny %>% 
        filter(ano >= 2010) %>% 
        saveRDS("data/expo_shiny_post2010.RDS")

