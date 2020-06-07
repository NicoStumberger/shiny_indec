
library(shiny)
library(tidyverse)
library(shinydashboard)
library(leaflet) # para mapa
library(rgdal) # para leer shapefiles
library(sf) # para manipular shapefiles y convertirlos a simple feature
library(htmltools)
library(readxl)
library(plotly) # para los graficos interactivos
library(lubridate) # para manejar fechas

# Carga de Datasets ----
expo_shiny <- readRDS("data/expo_shiny_post2010.RDS")

desc_ncm <- readRDS("data/desc_ncm.RDS")

categ <- unique(expo_shiny$cat_omc_1)

ano_1 <- expo_shiny %>% 
    filter(ano == max(ano)) %>% 
    left_join(desc_ncm) %>% 
    mutate(Mes = month(mes, label = TRUE, abbr = TRUE))

ano_0 <- expo_shiny %>% 
    filter(ano == max(ano) - 1 & mes <= max(ano_1$mes)) %>% 
    left_join(desc_ncm) %>% 
    mutate(Mes = month(mes, label = TRUE, abbr = TRUE))

# Mapa ----

mapa_crudo <- readOGR(
    dsn = "data/nuevo_mundo_simpli_es" ,
    layer = "nuevo_mundo_simpli_es",
    verbose = FALSE,
    encoding = "UTF-8" # para nombres en espanol con enies
) %>%
    st_as_sf(mapa_crudo)


# Grafico evolucion mensual ----

# Grafico de pendientes ----

# mesec queda fuera del server
mesec <- max(unique(ano_1$mes))

# paleta de colores queda fuera del server
color_cat_1 <- c("#00B1AC", "#00ADE6","#E9644C", "#7F7F7F")

# var_subcat puede quedar fuera del server
var_subcat <- expo_shiny %>% 
    filter(ano >= max(ano) - 1 & mes <= mesec) %>% 
    group_by(ano, cat_omc_1, cat_omc_2) %>% 
    summarise(fob_mm = sum(fob) / 1000000) %>% 
    group_by(cat_omc_2) %>% 
    mutate(var = (fob_mm / lag(fob_mm, n = 1) - 1) * 100) %>% 
    as_tibble()


# UI ----

# Sidebar

sidebar <- dashboardSidebar(collapsed = TRUE,
                            sidebarMenu(
    menuItem(
        "General",
        tabName = "general",
        icon = icon("chart-line"),
        badgeLabel = "Actualizado",
        badgeColor = "green"
    ),
    menuItem("Por destino", tabName = "destino",
             icon = icon("th"), 
             badgeLabel = "Prox.",
             badgeColor = "yellow"
             ),
    menuItem(
        "Por producto",
        tabName = "producto",
        icon = icon("angellist"),
        badgeLabel = "Prox.",
        badgeColor = "yellow"
    )
))

# Body
    
body <- dashboardBody(tabItems(
    ## Contenido de primer tab
    tabItem(
        tabName = "general",
        # h1("Exportaciones argentinas"),
        h3(paste0(
            "Periodo: ",
            min(ano_1$Mes),
            " - ",
            max(ano_1$Mes),
            " ",
            "(",
            max(ano_1$ano),
            ")"
        )),
        ## Boxes deben ir en rows o cols
        fluidRow(
            box(
                title = "Categorias",
                checkboxGroupInput(
                    "categoria",
                    "Elige una o varias",
                    choices = unique(expo_shiny$cat_omc_1),
                    selected = unique(expo_shiny$cat_omc_1)
                ),
                width = 4
            ),
            valueBoxOutput(outputId = "tot_1"),
            valueBoxOutput(outputId = "var_usd"),
            valueBoxOutput(outputId = "tot_0"),
            valueBoxOutput(outputId = "var_kg")
        ),
        fluidRow(leafletOutput("mapa")),
        fluidRow(
            box(
                title = "Evolución mensual de las exportaciones",
                solidHeader = TRUE,
                plotlyOutput(outputId = "evol")
            ),
            box(
                title = "Variación interanual por subcategorías",
                solidHeader = TRUE,
                plotlyOutput(outputId = "pendiente")
                )
        )
    ),
    ## Contenido del sgdo tab
    tabItem(
        tabName = "destino",
        h2("Exportaciones por destino"),
        fluidRow(
            box(width = 4,
                br(),
                imageOutput("gif1", height = "auto")),
            box(width = 4,
                br(),
                imageOutput("gif2", height = "auto")),
            box(width = 4,
                br(),
                imageOutput("gif3", height = "auto"))
        )
    ),
    ## Tercer tab
    tabItem(
        tabName = "producto",
        h2("Exportaciones por producto"),
        fluidRow(
            box(width = 4,
                br(),
                imageOutput("gif4", height = "auto")),
            box(width = 4,
                br(),
                imageOutput("gif5", height = "auto")),
            box(width = 4,
                br(),
                imageOutput("gif6", height = "auto"))
        )
    )
))

# Union en una pagina ----
ui <- dashboardPage(
    dashboardHeader(title = "Exportaciones"),
    sidebar,
    body
)

# Server ----

server <- function(input, output) {
    subset_cat_1 <- reactive(
        ano_1 %>% 
            filter(cat_omc_1 %in% input$categoria)
    )
    subset_cat_0 <- reactive(
        ano_0 %>% 
            filter(cat_omc_1 %in% input$categoria)
    )
    subset_cat <- reactive(
        expo_shiny %>% 
            filter(cat_omc_1 %in% input$categoria)
    )
    subset_pendiente <- reactive(
        var_subcat %>% 
            filter(cat_omc_1 %in% input$categoria)
    )
    
    # Total del periodo
    output$tot_1 <- renderValueBox({
        valueBox(
                format(round(sum(subset_cat_1()$fob) / 1000000, digits = 0),
                       big.mark = ".", 
                       decimal.mark = ","),
            paste0("Periodo actual ", max(ano_1$ano), " (en mill. USD)"), 
            icon = icon("dollar-sign"),
            color = "light-blue"
            )
    })
    
    # Total del periodo anterior
    output$tot_0 <- renderValueBox({
        valueBox(
           format(round(sum(subset_cat_0()$fob) / 1000000, digits = 0),
                         big.mark = ".", decimal.mark = ","),
           paste0("Mismo periodo ", max(ano_0$ano), " (en mill. USD)"), 
           icon = icon("dollar-sign"),
           color = "light-blue"
        )
    })
    
    # Variacion interanual en usd
    output$var_usd <- renderValueBox({
        valueBox(
            paste0(format(round((sum(subset_cat_1()$fob) / sum(subset_cat_0()$fob) - 1) * 100, 
                                digits = 1), 
                          big.mark = ".", decimal.mark = ","), " %"),
            "Var interanual en USD", icon = icon("money-bill"),
            color = "light-blue"
        )
    })
    
    # Variacion interanual en kg
    output$var_kg <- renderValueBox({
        valueBox(
            paste0(
                format(
                round(
                (sum(
                    subset_cat_1()$pnet_kg) / sum(subset_cat_0()$pnet_kg) - 1) * 100, 
                                digits = 1), 
                          big.mark = ".", decimal.mark = ","), " %"),
            "Var interanual en Kg", icon = icon("dolly"),
            color = "light-blue"
        )
    })
    
    # Mapa
    output$mapa <- renderLeaflet({
        # Preparacion de dataset
        ano_0_fob <- subset_cat_0() %>% 
            group_by(iso3) %>% 
            summarise(fob_mm_tot_0 = sum(fob) / 1000000)
        
        top_p <- subset_cat_1() %>% 
            group_by(iso3, desc_ncm) %>% 
            summarise(fob_mm = sum(fob) / 1000000) %>% 
            mutate(fob_mm_tot = sum(fob_mm),
                   part = fob_mm / fob_mm_tot * 100) %>% 
            arrange(iso3, desc(part)) %>% 
            top_n(n = 3, wt = part) %>% 
            mutate(top = paste0("top_", rank(desc(part)))) %>% 
            pivot_wider(names_from = top, values_from = c(desc_ncm, fob_mm, part)) %>% 
            left_join(ano_0_fob) %>% 
            mutate(ano_0_var = (fob_mm_tot / fob_mm_tot_0 - 1) * 100)
        
        #Union con geometrias
        mapa <- left_join(mapa_crudo, top_p, by = c("ISO3_CO" = "iso3"))
        
        # Paleta de colores
        pal <- colorNumeric(palette = c("#79dcff", "#01548A"),  # mejorar la paleta
                            domain = mapa$fob_mm_tot, # podria cambiarlo para dejarlo fuera?
                            na.color = "#BFBFBF")
        
        # Popup
        popup <- if_else(
            is.na(mapa$fob_mm_tot),
            paste0("<b>", mapa$nmbr_tr, "</b>",
                   "<br>",
                   "Sin datos en el periodo"),
            paste0(
                "<b>",
                mapa$nmbr_tr,
                " | ",
                "USD ",
                format(
                    round(mapa$fob_mm_tot, 1),
                    big.mark = ".",
                    decimal.mark = ","
                ),
                " mill.", "</b>",
                "<br>",
                "Var. interanual: ",
                format(
                    round(mapa$ano_0_var, 1),
                    decimal.mark = ","
                ), "%",
                "<br>",
                "Principales productos al destino:", "<br>",
                format(
                    round(mapa$part_top_1, 1), 
                    decimal.mark = ","),"%", " | ", mapa$desc_ncm_top_1, "<br>", 
                format(
                    round(mapa$part_top_2, 1), 
                    decimal.mark = ","),"%", " | ", mapa$desc_ncm_top_2, "<br>",
                format(
                    round(mapa$part_top_3, 1), 
                    decimal.mark = ","),"%", " | ", mapa$desc_ncm_top_3
            )
        )
        
        # Renderizacion del mapa
        leaflet(data = mapa, options = leafletOptions(minZoom = 2.2, maxZoom = 6)) %>%
            setView(lng = 0, lat = 0, zoom = 2.3) %>% 
            fitBounds(lng1 = -169.276515, lat1 = 65.715532, 
                      lng2 = 179.738538, lat2 = -67.196362) %>% 
            clearBounds() %>% 
            # addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
            addPolygons(
                fillColor = ~ pal(fob_mm_tot),
                weight = 1,
                opacity = 1,
                color = "white",
                dashArray = "",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                    weight = 5,
                    color = "#7F7F7F",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE
                ),
                label = mapa$nmbr_tr,
                labelOptions = labelOptions(
                    style = list("front-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                ),
                popup = popup,
                popupOptions = popupOptions(
                    style = list("front-weight" = "normal", padding = "3px 8px"),
                    textsize = "20px",
                    direction = "auto"
                )
            ) %>% 
            addEasyButton(easyButton(
                icon="fa-globe", title="Zoom to Level 1",
                onClick=JS("function(btn, map){ map.setZoom(2.3); }")))

    })
    
    # Evolucion mensual
    output$evol <- renderPlotly({
        evol <-  subset_cat() %>% 
            group_by(ano, mes) %>% 
            summarise(fob_tot_mm = sum(fob) / 1000000) %>% 
            as_tibble() %>% 
            mutate(Mes = month(mes, label = TRUE))
        
        evol_ant <- evol %>% 
            filter(ano < max(ano) - 1)
        
        evol_0 <- evol %>% 
            filter(ano == max(ano) - 1)
        
        evol_1 <- evol %>% 
            filter(ano == max(ano))
        
        evol_punto_1 <- evol %>% 
            filter(ano == max(ano)) %>% 
            filter(mes == max(mes))
        
        ev <- evol_ant %>%
            ggplot(aes(Mes, fob_tot_mm)) +
            geom_line(aes(group = ano, color = paste0("2010-", max(ano))) 
            ) +
            geom_line(
                data = evol_0,
                aes(group = ano, color = paste0(max(ano)))
            ) +
            geom_line(
                data = evol_1,
                aes(group = ano, color = paste0(max(ano))),
                size = 1.2,
            ) +
            geom_point(data = evol_punto_1,
                       aes(Mes, 
                           fob_tot_mm, 
                           color = paste0(max(month(evol_1$mes, 
                                                    label = TRUE, 
                                                    abbr = FALSE))
                           )
                       ),
                       size = 3) +
            theme_minimal() +
            theme(panel.grid.major = element_blank()) +
            labs(
                # podria sacar eltexto del server
                x = "",
                y = "USD FOB en mill.",
                color = "Años"
            ) +
            # podría sacarlo del server
            scale_y_continuous(labels = scales::comma_format(big.mark = ".",
                                                             decimal.mark = ",")) +
            scale_color_manual("",
                               # Podria sacarlo del server
                               breaks = c(paste0("2010-", max(evol_ant$ano)),
                                          paste0(max(evol_0$ano)),
                                          paste0(max(evol_1$ano)), 
                                          paste0(max(month(evol_1$mes, 
                                                           label = TRUE, 
                                                           abbr = FALSE)))
                               ),
                               # podría sacarlo del server
                               values = c("#BFBFBF", 
                                          "#00ADE6", 
                                          "#3175AC", 
                                          "#3175AC"))
        
        # Render grafico
        ggplotly(ev, tooltip = c("x", "y"))
    })
    
    # Grafico de pendientes
    output$pendiente <- renderPlotly({
        
        g_pendiente <- subset_pendiente() %>%
            ggplot(aes(as_factor(ano), fob_mm, group = cat_omc_2)) +
            geom_line(aes(color = cat_omc_1)) +
            geom_point(aes(color = cat_omc_1),
                       size = 3.5) +
            geom_text(
                data = subset(subset_pendiente(), ano < max(ano)),
                aes(color = cat_omc_1),
                label = if_else(
                    str_length(unique(subset_pendiente()$cat_omc_2)) > 22,
                    paste0(str_sub(
                        unique(subset_pendiente()$cat_omc_2),
                        start = 1,
                        end = 15
                    ), "..."),
                    paste0(unique(subset_pendiente()$cat_omc_2))
                ),
                size = 3,
                nudge_x = -0.3,
                hjust = 0,
            ) +
            geom_text(
                aes(color = cat_omc_1),
                label = if_else(is.na(subset_pendiente()$var), paste0(""),
                                paste0(
                                    format(
                                        subset_pendiente()$var,
                                        digits = 2,
                                        big.mark = ".",
                                        decimal.mark = ","
                                    ),
                                    " %"
                                )), 
                size = 3,
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
        
        ggplotly(g_pendiente)
    })
    
    # Por destino
    # output$gif1 <- renderImage({
    #     return(list(src = "data/working.gif", contentType = "image/gif"))
    # }, deleteFile = FALSE)
    
    output$gif2 <- renderImage({
        return(list(src = "data/working.gif", contentType = "image/gif"))
    }, deleteFile = FALSE)
    
    # output$gif3 <- renderImage({
    #     return(list(src = "data/working.gif", contentType = "image/gif"))
    # }, deleteFile = FALSE)
    
    # Por producto
    # output$gif4 <- renderImage({
    #     return(list(src = "data/working.gif", contentType = "image/gif"))
    # }, deleteFile = FALSE)
    
    output$gif5 <- renderImage({
        return(list(src = "data/working.gif", contentType = "image/gif"))
    }, deleteFile = FALSE)
    
    # output$gif6 <- renderImage({
    #     return(list(src = "data/working.gif", contentType = "image/gif"))
    # }, deleteFile = FALSE)
}


# Run the application 
shinyApp(ui = ui, server = server)
