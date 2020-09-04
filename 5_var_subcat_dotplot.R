



# Librarias utilizadas
library(tidyverse)
# library(hrbrthemes) # no lo estoy usando
library(plotly)
library(lubridate)
# library(ggrepel) # para ggplot pero con ggplotly no sirve..

# Objetos ya en la app

expo_shiny <- readRDS("data/expo_shiny_post2010.RDS") %>% 
        filter(!is.na(fob))

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

# Armado de tablas ----

categ <- unique(expo_shiny$cat_omc_1)

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


# Reactive expressions ----


subset_dotplot <- expo_shiny %>% 
        # mutate(cat_omc_2 = droplevels(cat_omc_2),
        #        cat_omc_1 = droplevels(cat_omc_1)) %>%
        # mutate(cat_omc_2 = as.character(cat_omc_2),
        #        cat_omc_1 = as.character(cat_omc_1)) %>%
        filter(ano >= max(ano) - 1 & mes <= mesec) %>% 
        group_by(ano, cat_omc_1, cat_omc_2) %>% 
        summarise(y = sum(fob, na.rm = TRUE) / 1000000) %>% 
        pivot_wider(names_from = ano, values_from = y) %>%
        rename(ano_0 = 3,
               ano_1 = 4) %>% 
        mutate(var = (ano_1 / ano_0  - 1) * 100) %>% 
        arrange(ano_1)


# RenderPlotly ----
dotplot <- subset_dotplot %>%
        ggplot(aes(ano_1, reorder(cat_omc_2, ano_1))) +
        geom_segment(aes(
                x = ano_1,
                y = reorder(cat_omc_2, ano_1),
                xend = ano_0,
                yend = reorder(cat_omc_2, ano_1),
                color = cat_omc_1
        )) +
        geom_point(aes(
                color = cat_omc_1,
                text = paste(
                        cat_omc_2,
                        "<br>",
                        "2020",
                        " ",
                        round(ano_1, 2),
                        "<br>",
                        round(var, 2)
                )
        ),
        size = 2) +
        geom_point(
                aes(
                        ano_0,
                        reorder(cat_omc_2, ano_1),
                        color = cat_omc_1,
                        text = paste(cat_omc_2,
                                     "<br>",
                                     "2019",
                                     " ",
                                     round(ano_0, 2),
                                     "<br>")
                ),
                alpha = 0.3,
                size = 2
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        scale_x_log10() +
        labs(x = "USD",
             y = "")

dotplot

ggplotly(dotplot, tooltip = "text")
