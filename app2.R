
library(shiny)
library(tidyverse)
library(readxl)
library(here)
library(shinydashboard)
library(leaflet) # para mapa
library(spData) # tiene dataset world con los multipolygons de todos los paises

dic_omc <- readRDS(here("data/hs_omc.RDS"))

dic_paises <- read_excel("data/paises.xls", skip = 3) %>% 
    janitor::clean_names() %>% 
    filter(!is.na(codigo))

## 2019
ano0 <- read_delim(
    "data/2019/expon19.csv",
    ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    trim_ws = TRUE)  %>%
    janitor::clean_names() %>% 
    rename(ano = a_f1_o,
           fob = fob_u_s) %>% 
    as_tibble() %>% 
    mutate(hs6 = str_sub(string = ncm, start = 1, end = 6))

ano0$ano <- as.integer(ano0$ano)
ano0$mes <- as.integer(ano0$mes)
ano0$pdes <- as.character(ano0$pdes)

ano0 <- ano0 %>% 
    left_join(dic_omc, by = c("hs6" = "from_hs_2017"))

ano0 <- ano0 %>% 
    left_join(dic_paises, by = c("pdes" = "codigo"))

## 2020
ano1 <- read_delim(
    "data/2020/expon20.csv",
    ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    trim_ws = TRUE)  %>%
    janitor::clean_names() %>% 
    rename(ano = a_f1_o,
           fob = fob_u_s) %>% 
    as_tibble() %>% 
    mutate(hs6 = str_sub(string = ncm, start = 1, end = 6))

ano1$ano <- as.integer(ano1$ano)
ano1$mes <- as.integer(ano1$mes)
ano1$pdes <- as.character(ano1$pdes)

ano1 <- ano1 %>% 
    left_join(dic_omc, by = c("hs6" = "from_hs_2017"))

ano1 <- ano1 %>% 
    left_join(dic_paises, by = c("pdes" = "codigo"))


top_dest <- ano0 %>% 
    group_by(cat_omc2, pdes) %>% 
    summarise(fob = sum(fob),
              n = n()) %>% 
    filter(pdes != 999 & !is.na(cat_omc2)) %>% 
    mutate(rank = rank(desc(fob))) %>% 
    arrange(cat_omc2, rank) %>% 
    filter(rank <= 10) %>% 
    left_join(dic_paises, by = c("pdes" = "codigo"))


tot_ano0 <- subset(ano0, ano0$mes <= max(ano1$mes)) %>% 
    group_by(ano) %>% 
    summarise(fob = sum(fob),
              kg = sum(pnet_kg))


## Para boxes de totales
var_usd <- (sum(ano1$fob) / sum(tot_ano0$fob) - 1) * 100
var_kg <- (sum(ano1$pnet_kg) / sum(tot_ano0$kg) - 1) * 100


## Variaciones

ano0_p <- subset(ano0, ano0$mes <= max(ano1$mes))

ano0_ano1 <- rbind(ano0_p, ano1)

ano0_ano1 %>% 
    group_by(ano, cat_omc2) %>% 
    summarise(fob = sum(fob),
              kg = sum(pnet_kg))




## Mapa

summary(world$pop)

class(world)

bins <- c(0, 5.630e+04, 3.755e+06, 1.040e+07, 3.075e+07, Inf)
pal <- colorBin(palette = "Blues", domain = world$pop, bins = 6, pretty = FALSE)
qpal <- colorQuantile("Blues", domain = world$pop, n = 7)


m <- leaflet(data = world, 
             options = leafletOptions(minZoom = 1.5, maxZoom = 18)) %>%
    addPolygons(color = "#444444", 
                weight = 1, 
                smoothFactor = 0.5, 
                opacity = 1.0, 
                fillOpacity = 0.5,
                fillColor = ~pal(pop), # aca debe ir una fc para darle peso
                highlightOptions = highlightOptions(color = "white", 
                                                    weight = 2,
                                                    bringToFront = TRUE))


labels <- sprintf(
    "<strong>%s</strong><br/>%g pobl total",
    world$name_long, world$pop / 1000000) %>% 
    lapply(htmltools::HTML)

m <- m %>% 
    addPolygons(
        color = "#444444", 
        weight = 1, 
        smoothFactor = 0.5, 
        opacity = 1.0, 
        fillOpacity = 0.5,
        fillColor = ~qpal(pop), # aca debe ir una fc para darle peso
        highlightOptions = highlightOptions(color = "white", 
                                            weight = 2,
                                            bringToFront = TRUE),
        label = labels, labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px", direction = "auto")
    )

m <- m %>% 
    addLegend(pal = pal, values = ~pop, opacity = 0.7, title = NULL,
                     position = "bottomright")



## Historico

## Primero leer los datasets historicos y rbindearlos en uno solo 
## (incluyendo el ultimo ano)

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
    mutate(hs6 = str_sub(string = ncm, start = 1, end = 6))

hist$ano <- as.integer(hist$ano)
hist$mes <- as.integer(hist$mes)
hist$pdes <- as.character(hist$pdes)

hist <- hist %>% 
    left_join(dic_omc, by = c("hs6" = "from_hs_2017"))

hist <- hist %>% 
    left_join(dic_paises, by = c("pdes" = "codigo"))

skimr::skim(hist)

## como armamos el grafico


g <- hist %>% 
    filter(ano < max(ano)) %>% 
    group_by(ano, mes) %>% 
    summarise(fob = sum(fob))

ult <- hist %>% 
    filter(ano == max(ano)) %>% 
    group_by(ano, mes) %>% 
    summarise(fob = sum(fob))

ult_ant <- hist %>% 
    filter(ano == max(ano)-1) %>% 
    group_by(ano, mes) %>% 
    summarise(fob = sum(fob))

g_hist <- g %>% 
    ggplot(aes(as.factor(mes), fob)) +
    geom_line(aes(group = ano), color = "grey40", alpha = 0.5) +
    geom_line(data = ult, aes(group = ano), color = "blue", size = 1.5) +
    geom_line(data = ult_ant, aes(group = ano), color = "grey30", size = 1)


## Input
omc2 <- unique(top_dest$cat_omc2)




ui <- dashboardPage(
    dashboardHeader(title = "Exportaciones argentinas"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("General", tabName = "general", 
                     icon = icon("dashboard")
            ),
            menuItem("Por destino", tabName = "destino", 
                     icon = icon("globe-americas")
            ),
            menuItem("Por producto", tabName = "producto", 
                     icon = icon("wine-glass-alt")
            )
        )
        
    ),
    dashboardBody(
        tabItems(
            ## Contenido de primer tab
            tabItem(tabName = "general",
                    ## Boxes deben ir en rows o cols
                    fluidRow(
                        valueBoxOutput(outputId = "tot"),
                        valueBoxOutput(outputId = "var_usd"),
                        valueBoxOutput(outputId = "var_kg")
                    ),
                    fluidRow(
                        leafletOutput("mapa")
                    ),
                    fluidRow(
                        box(
                            plotOutput(outputId = "hist")
                        )
                    ),
                    fluidRow(
                        box(
                            title = "Categoría",
                            radioButtons(inputId = "omc2", 
                                         label = "Elegi una categoria",
                                         choices = unique(top_dest$cat_omc2),
                                         selected = NULL)
                        ),
                        box(
                            plotOutput(outputId = "col", height = 250)
                        )
                    )
            ),
            
            ## Contenido del sgdo tab
            tabItem(tabName = "destino",
                    h2("Exportaciones por destino")
                    ),
            
            ## Tercer tab
            tabItem(tabName = "producto",
                    h2("Exportaciones por producto")
                    )
        )
    )
    
)


server <- function(input, output) {
    output$tot <- renderValueBox({
        valueBox(
            paste0(format(round(sum(ano1$fob) / 1000000, digits = 0), 
                          big.mark = ".", decimal.mark = ",")),
            "Valores FOB (en USD)", icon = icon("dollar-sign")
        )
    })
    output$var_usd <- renderValueBox({
        valueBox(
            paste0(format(round(var_usd, digits = 1), 
                                 big.mark = ".", decimal.mark = ",")),
            "Variación FOB (interanual)", icon = icon("percent")
        )
    })
    output$var_kg <- renderValueBox({
        valueBox(
            format(round(var_kg, digits = 1), 
                          big.mark = ".", 
                          decimal.mark = ","),
            "Variación Kg (interanual)", icon = icon("percent")
        )
    })
    output$mapa <- renderLeaflet({
        m
    })
    output$hist <- renderPlot({
        title <- "Evolucion de las exportaciones mensuales"
        g_hist
    })
    output$col <- renderPlot({
        title <- "Top destinos" 
        top_dest %>% 
            filter(cat_omc2 == input$omc2) %>% 
            ggplot(aes(fob, reorder(denominacion, fob))) +
            geom_col(aes(fill = fob)) +
            scale_x_continuous(labels = scales::comma) +
            labs(title = title,
                 x = "USD",
                 y = "") +
            guides(fill = FALSE)
    })
    
}

shinyApp(ui = ui, server = server)