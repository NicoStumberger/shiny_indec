
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
library(shinydashboardPlus) # cuenta con algunas funcionalidades que mejoran el dash
library(shinyWidgets)
# library(semantic.dashboard)


# Setea el el Sys locale en ingles
Sys.setlocale("LC_TIME", "English")

# Carga de Datasets ----
expo_shiny <- readRDS("data/expo_shiny_post2010.RDS") %>%
  filter(!is.na(fob)) %>%
  mutate(
    nom_indec = case_when(
      iso3 == "HKG" ~ "Hong Kong (China)",
      TRUE ~ nom_indec
    )
  )

skimr::skim(expo_shiny)
desc_ncm <- readRDS("data/desc_ncm.RDS")

# Preguntas frecuentes
faq <- read_excel("data/faq.xlsx") %>% 
  mutate(pregunta = str_c(id, ".", " ", pregunta))

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

cat_subcat_ncm <- expo_shiny %>% 
  select(cat_omc_1, cat_omc_2, ncm) %>% 
  distinct() %>% 
  left_join(desc_ncm)

ano_1 <- expo_shiny %>%
  filter(ano == max(ano)) %>%
  left_join(desc_ncm) %>%
  mutate(Mes = translate_date(month(mes, label = TRUE, abbr = TRUE)))



ano_0 <- expo_shiny %>%
  filter(ano == max(ano) - 1 & mes <= max(ano_1$mes)) %>%
  left_join(desc_ncm) %>%
  mutate(Mes = translate_date(month(mes, label = TRUE)))


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

mesec <- max(unique(ano_1$mes))

color_cat_1 <- c("#00B1AC", "#00ADE6","#E9644C", "#7F7F7F")


# UI ----

# Sidebar ----

header <- dashboardHeaderPlus()

sidebar <- dashboardSidebar(sidebarMenu(
                                menuItem(
                                    "Coyuntura",
                                    tabName = "general",
                                    icon = icon("chart-line"),
                                    badgeLabel = "Actualizado",
                                    badgeColor = "green"
                                ),
                                menuItem(
                                    "Ayuda",
                                    tabName = "ayuda",
                                    icon = icon("info")
                                )
                            ),
                            disable = TRUE,
                            collapsed = TRUE)

# Body ----
    
body <- dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Calibri";
        font-weight: bold;
        font-size: 24px;
      }
      '))),
    tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }
      '))),
    tabItems(
    ## Contenido de primer tab
    tabItem(
        tabName = "general",
        fluidRow(
          widgetUserBox(
            title = tags$h1("Mapa de las exportaciones"),
            subtitle = tags$h4(paste("Periodo Enero - ", 
                             dic_meses$Mes_completo[dic_meses$mes == max(ano_1$mes)], 
                             max(ano_1$ano))
                             ),
            width = 12,
            type = 2,
            # src = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg",
            color = "primary",
            "Toda la información publicada en este tablero trata sobre 
            las exportaciones argentinas durante el periodo arriba 
            mencionado y su correspondiente interanual, a menos que se 
            explicite lo contrario.",
            footer = "La fuente de información es AAICI en base a datos de 
            INDEC y a la categorización por sectores y subsectores de la OMC."
          ),
        ),
        
        h3("Análisis por categoría"),
        tags$hr(style="border-color: #00ADE6;"),
        # Primer fila ----
        fluidRow(
            boxPlus(
                # title = "Categorias",
                prettyCheckboxGroup(
                    inputId = "categoria",
                    label = "Selecciona una o varias Categorías",
                    thick = TRUE,
                    # choices = levels(expo_shiny$cat_omc_1),
                    selected = levels(expo_shiny$cat_omc_1),
                    animation = "pulse",
                    status = "info",
                    choiceNames = c(
                        "Productos agrícolas",
                        "Manufacturas",
                        "Combustibles y p.i.e.",
                        "Otros"
                    ),
                    choiceValues = levels(expo_shiny$cat_omc_1)
                ),
                actionBttn(inputId = "ver", 
                           label = "Actualizar",
                           style = "jelly", 
                           color = "success", 
                           size = "sm", 
                           block = TRUE,
                           icon = icon("redo")),
                closable = FALSE,
                width = 4
            ),
            boxPlus(
                solidHeader = FALSE,
                title = "¿Cuánto se exportó en el periodo y cuál fue la variación interanual?",
                p("Totales en valores y cantidades para las categorías seleccionadas."),
                width = 8,
                # status = "info",
                closable = FALSE,
                footer = fluidRow(# descripcion general del periodo
                    column(width = 6,
                           uiOutput("desc_ano_1")),
                    column(width = 6,
                           uiOutput("desc_ano_1_ton")))
            ),
        ),
        # Segunda fila MAPA ----
        fluidRow(
            box(title = "¿Cuáles son los principales destinos?",
                p("Click en cada país para ver valores, variación y principales
                  productos exportados en el periodo"),
                width = 12,
                leafletOutput("mapa") %>% 
                  shinycssloaders::withSpinner(type = 2, color = "#00ADE6", 
                                               color.background = "white")
            )
        ),
        # Tercer fila ----
        fluidRow(
            box(
                title = "¿Cómo evolucionan las exportaciones mensuales de la categoría
                seleccionada comparando con años anteriores?",
                tags$h6(em("Pasa el mouse por encima del gráfico para ver detalles,
                  clickea las leyendas para quitar información del gráfico.
                  También podés ver la evolución en cantidades haciendo click al pie del gráfico.")),
                solidHeader = TRUE,
                plotlyOutput(outputId = "evol") %>% 
                  shinycssloaders::withSpinner(type = 2, color = "#00ADE6", 
                                               color.background = "white"),
                width = 6,
                prettyToggle(
                    inputId = "toggle1",
                    label_on = "Mostrar valores FOB",
                    label_off = "Mostrar cantidades",
                    icon_on = icon("angle-left"),
                    icon_off = icon("angle-right"), 
                    animation = "pulse"
                )
            ),
            box(
                title = "¿Cuáles son las principales subcategorías y cuál fue la variación?",
                tags$h6(em("Pasa el mouse por encima del gráfico para ver detalles.
                  También podés ver la evolución en cantidades haciendo click al pie del gráfico.")),
                solidHeader = TRUE,
                plotlyOutput(outputId = "pendiente") %>% 
                  shinycssloaders::withSpinner(type = 2, color = "#00ADE6", 
                                               color.background = "white"),
                prettyToggle(
                    inputId = "toggle2",
                    label_on = "Mostrar valores FOB",
                    label_off = "Mostrar cantidades",
                    icon_on = icon("angle-left"),
                    icon_off = icon("angle-right"), 
                    animation = "pulse"
                )
            )
        ),
        
        
        h3("Análisis por subcategoría"),
        tags$hr(style="border-color: #00ADE6;"),
        # Cuarta fila ----
        fluidRow(
          boxPlus(
              selectizeInput(
                inputId = "subcat", 
                label = "Selecciona una o varias subcategorías",
                choices = subcat,
                multiple = TRUE,
                options = list(placeholder = "Selecciona una o varias subcategorías")
            ),
            actionBttn(inputId = "ver_subcat", 
                       label = "Actualizar",
                       style = "jelly", 
                       color = "success", 
                       size = "sm", 
                       block = TRUE,
                       icon = icon("redo")),
            width = 4,
            tags$hr(),
            p("Totales en valores y cantidades para las subcategorías seleccionadas."),
            footer = fluidRow(# descripcion general del periodo
              column(width = 6,
                     uiOutput("desc_ano_1_subcat")),
              column(width = 6,
                     uiOutput("desc_ano_1_ton_subcat")))
            
          ),
          box(
            title = "¿Cómo evolucionan las exportaciones mensuales de 
            las subcategorías seleccionadas, comparando con años anteriores?",
            tags$h6(em("Pasa el mouse por encima del gráfico para ver detalles.
                  También podés ver la evolución en cantidades haciendo click al pie del gráfico.")),
            solidHeader = TRUE,
            plotlyOutput(outputId = "evol_sub_cat") %>% 
              shinycssloaders::withSpinner(type = 2, color = "#00ADE6", 
                                           color.background = "white"),
            width = 8,
            prettyToggle(
              inputId = "toggle3",
              label_on = "Mostrar valores FOB",
              label_off = "Mostrar cantidades",
              icon_on = icon("angle-left"),
              icon_off = icon("angle-right"), 
              animation = "pulse"
            )
          )
        ),
        
        # Quinta fila ----
        fluidRow(
          box(
            title = "¿Cuáles son los principales destinos de las 
            subcategorías seleccionadas y cuánto variaron?",
            tags$h6(em("Pasa el mouse por encima del gráfico para ver detalles,
                  También podés ver en cantidades haciendo click al pie del gráfico.")),
            solidHeader = TRUE,
            plotlyOutput(outputId = "destinos") %>% 
              shinycssloaders::withSpinner(type = 2, color = "#00ADE6", 
                                           color.background = "white"),
            width = 6,
            prettyToggle(
              inputId = "toggle4",
              label_on = "Mostrar valores FOB",
              label_off = "Mostrar cantidades",
              icon_on = icon("angle-left"),
              icon_off = icon("angle-right"), 
              animation = "pulse"
            )
          ),
          box(
            title = "¿Cuáles son los principales productos de las 
            subcategorías seleccionadas y cuánto variaron?",
            tags$h6(em("Pasa el mouse por encima del gráfico para ver detalles.
                  También podés ver en cantidades haciendo click al pie del gráfico.")),
            solidHeader = TRUE,
            plotlyOutput(outputId = "productos") %>% 
              shinycssloaders::withSpinner(type = 2, color = "#00ADE6", 
                                           color.background = "white"),
            width = 6,
            prettyToggle(
              inputId = "toggle5",
              label_on = "Mostrar valores FOB",
              label_off = "Mostrar cantidades",
              icon_on = icon("angle-left"),
              icon_off = icon("angle-right"), 
              animation = "pulse"
            )
          )
        ),
        tags$hr(style="border-color: #00ADE6;")
    ),
    
    ## Info Ayuda FAQ ----
    tabItem(tabName = "ayuda",
            h2("Preguntas frecuentes"),
            fluidRow(
              widgetUserBox(
                title = tags$h4(faq$pregunta[faq$id==1]),
                boxToolSize = "md",
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                type = 2,
                color = "primary",
                faq$respuesta[faq$id==1]
              ),
              widgetUserBox(
                title = tags$h4(faq$pregunta[faq$id==2]),
                boxToolSize = "md",
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                type = 2,
                color = "primary",
                faq$respuesta[faq$id==2]
              ),
              widgetUserBox(
                title = tags$h4(faq$pregunta[faq$id==3]),
                boxToolSize = "md",
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                type = 2,
                color = "primary",
                faq$respuesta[faq$id==3]
              ),
              widgetUserBox(
                title = tags$h4(faq$pregunta[faq$id==4]),
                boxToolSize = "md",
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                type = 2,
                color = "primary",
                faq$respuesta[faq$id==4]
              ),
              widgetUserBox(
                title = tags$h4(faq$pregunta[faq$id==5]),
                boxToolSize = "md",
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                type = 2,
                color = "primary",
                faq$respuesta[faq$id==5]
              ),
              widgetUserBox(
                title = tags$h4(faq$pregunta[faq$id==6]),
                boxToolSize = "md",
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                type = 2,
                color = "primary",
                faq$respuesta[faq$id==6]
              ),
              widgetUserBox(
                title = tags$h4(faq$pregunta[faq$id==7]),
                boxToolSize = "md",
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                type = 2,
                color = "primary",
                faq$respuesta[faq$id==7]
              ),
              widgetUserBox(
                title = tags$h4(faq$pregunta[faq$id==8]),
                boxToolSize = "md",
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                type = 2,
                color = "primary",
                faq$respuesta[faq$id==8]
              ),
              widgetUserBox(
                title = tags$h4(faq$pregunta[faq$id==9]),
                boxToolSize = "md",
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                type = 2,
                color = "primary",
                faq$respuesta[faq$id==9]
              ),
              widgetUserBox(
                title = tags$h4(faq$pregunta[faq$id==10]),
                boxToolSize = "md",
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                type = 2,
                color = "primary",
                faq$respuesta[faq$id==10]
              ),
              widgetUserBox(
                title = tags$h4(faq$pregunta[faq$id==11]),
                boxToolSize = "md",
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                type = 2,
                color = "primary",
                faq$respuesta[faq$id==11]
              ),
              widgetUserBox(
                title = tags$h4(faq$pregunta[faq$id==12]),
                boxToolSize = "md",
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                type = 2,
                color = "primary",
                faq$respuesta[faq$id==12]
              ),
              widgetUserBox(
                title = tags$h4(faq$pregunta[faq$id==13]),
                boxToolSize = "md",
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                type = 2,
                color = "primary",
                faq$respuesta[faq$id==13]
              ),
              widgetUserBox(
                title = tags$h4(faq$pregunta[faq$id==14]),
                boxToolSize = "md",
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                type = 2,
                color = "primary",
                faq$respuesta[faq$id==14]
              ),
              widgetUserBox(
                title = tags$h4(faq$pregunta[faq$id==15]),
                boxToolSize = "md",
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                type = 2,
                color = "primary",
                faq$respuesta[faq$id==15]
              )
            ))
))




# Union en una pagina ----
ui <- dashboardPagePlus(
    header = header,
    sidebar,
    body
)

# SERVER ----

server <- function(input, output) {
    
    subset_cat_1 <- eventReactive(input$ver, {
        ano_1 %>% filter(cat_omc_1 %in% input$categoria)
    }, ignoreNULL = FALSE
    )
    subset_cat_0 <- eventReactive(input$ver, {
        ano_0 %>% filter(cat_omc_1 %in% input$categoria)
    }, ignoreNULL = FALSE
    )
    subset_cat <- eventReactive(input$ver, {
        expo_shiny %>% filter(cat_omc_1 %in% input$categoria)  
    }, ignoreNULL = FALSE
    )
    
    
    # Descripcion del periodo ----
    output$desc_ano_1 <- renderUI({
        
        number_0 <- format(round((
            sum(subset_cat_1()$fob, na.rm = TRUE) / sum(subset_cat_0()$fob, na.rm = TRUE) - 1
        ) * 100,
        digits = 1),
        big.mark = ".",
        decimal.mark = ",")
        
        header_0 <- format(round(sum(subset_cat_1()$fob, na.rm = TRUE) / 1000000, digits = 0),
                           big.mark = ".",
                           decimal.mark = ",")
        
        descriptionBlock(
            number = paste0(number_0," %"),
            numberColor = if_else(number_0 >= 0, "green", "red"),
            numberIcon = if_else(number_0 >= 0,
                                  "caret-up",
                                  "caret-down"),
            header = paste(header_0),
            text = "millones de USD",
            rightBorder = TRUE
        )
    })
    
    output$desc_ano_1_ton <- renderUI({
        
        number_0 <- format(round((
            sum(subset_cat_1()$pnet_kg, na.rm = TRUE) / sum(subset_cat_0()$pnet_kg, na.rm = TRUE) - 1
        ) * 100,
        digits = 1),
        big.mark = ".",
        decimal.mark = ",")
        
        
        header_0 <-
            format(
                round(sum(subset_cat_1()$pnet_kg, na.rm = TRUE) / 1000000000,
                      digits = 1),
                big.mark = ".",
                decimal.mark = ","
            )
        
        descriptionBlock(
            number = paste0(number_0," %"),
            numberColor = if_else(number_0 >= 0, "green", "red"),
            numberIcon = if_else(number_0 >= 0, 
                                  "caret-up", 
                                  "caret-down"),
            header = paste0(header_0),
            text = "millones de TONELADAS",
        )
    })
    
    # Mapa ----
    
    # Reactive
    
    ano_0_fob <- reactive({
      subset_cat_0() %>%
        group_by(iso3) %>%
        summarise(fob_mm_tot_0 = sum(fob, na.rm = TRUE) / 1000000)
    })
    
    top_p <- reactive({
      subset_cat_1() %>%
      group_by(iso3, desc_ncm) %>%
      summarise(fob_mm = sum(fob, na.rm = TRUE) / 1000000) %>%
      mutate(fob_mm_tot = sum(fob_mm, na.rm = TRUE),
             part = fob_mm / fob_mm_tot * 100) %>%
      arrange(iso3, desc(part)) %>%
      top_n(n = 3, wt = part) %>%
      mutate(top = paste0("top_", rank(desc(part)))) %>%
      pivot_wider(names_from = top,
                  values_from = c(desc_ncm, fob_mm, part)) %>%
      left_join(ano_0_fob()) %>%
      mutate(ano_0_var = (fob_mm_tot / fob_mm_tot_0 - 1) * 100)
    })
    
    #Union con geometrias
    mapa <- reactive({
      left_join(mapa_crudo, top_p(), by = c("ISO3_CO" = "iso3"))
    })
    
    # Paleta de colores
    pal <- reactive({
      colorNumeric(
        palette = c("#79dcff", "#01548A"),
        # mejorar la paleta
        domain = mapa()$fob_mm_tot,
        # podria cambiarlo para dejarlo fuera?
        na.color = "#BFBFBF"
      )
    })
    
    # Popup
    popup <- reactive({
      if_else(
      is.na(mapa()$fob_mm_tot),
      paste0(
        "<style='font-family:calibri;'>", "<b>",
        mapa()$nmbr_tr,
        "</b>",
        "<br>",
        "<style='font-family:calibri;'> Sin datos en el periodo"
      ),
      paste0(
        "<style='font-family:calibri;'>", "<b>",
        mapa()$nmbr_tr,
        " | ",
        "USD ",
        format(
          round(mapa()$fob_mm_tot, 1),
          big.mark = ".",
          decimal.mark = ","
        ),
        " mill.", "</b>",
        "<br>",
        "Var. interanual: ",
        format(round(mapa()$ano_0_var, 1),
               decimal.mark = ","),
        "%",
        "<br>",
        "Part. de los principales productos al destino:",
        "<br>",
        format(round(mapa()$part_top_1, 1),
               decimal.mark = ","),
        "%",
        " | ",
        mapa()$desc_ncm_top_1,
        "<br>",
        format(round(mapa()$part_top_2, 1),
               decimal.mark = ","),
        "%",
        " | ",
        mapa()$desc_ncm_top_2,
        "<br>",
        format(round(mapa()$part_top_3, 1),
               decimal.mark = ","),
        "%",
        " | ",
        mapa()$desc_ncm_top_3, "</p>"
      )
    )
    })
    
    output$mapa <- renderLeaflet({
        
        # Renderizacion del mapa
        leaflet(data = mapa(),
                options = leafletOptions(minZoom = 1.9, maxZoom = 6)) %>%
            setView(lng = 0,
                    lat = 0,
                    zoom = 1.9) %>%
            fitBounds(
                lng1 = -169.276515,
                lat1 = 65.715532,
                lng2 = 179.738538,
                lat2 = -67.196362
            ) %>%
            clearBounds() %>%
            # addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
            addPolygons(
                fillColor = ~ pal()(fob_mm_tot),
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
                label = mapa()$nmbr_tr,
                labelOptions = labelOptions(
                    style = list("font-family" = "Calibri", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                ),
                popup = popup(),
                popupOptions = popupOptions(
                    style = list("font-family" = "Calibri", padding = "3px 8px"),
                    # textsize = "20px",
                    direction = "auto"
                )
            ) %>%
            addEasyButton(easyButton(
                icon = "fa-globe",
                title = "Zoom to Level 1",
                onClick = JS("function(btn, map){ map.setZoom(2.3); }")
            ))
        
    })
    
    # Evolucion mensual ----
    
    # Reactive
    evol <- reactive({
      if(input$toggle1 == TRUE) {
        evol <-  subset_cat() %>%
          group_by(ano, mes) %>%
          # En millones de toneladas
          summarise(y = sum(pnet_kg, na.rm = TRUE) / 1000000000) %>%
          as_tibble() %>%
          mutate(Mes = factor(translate_date(month(mes, label = TRUE)),
                              levels = spanish_months))
      } else {
        evol <-  subset_cat() %>%
          group_by(ano, mes) %>%
          summarise(y =  sum(fob, na.rm = TRUE) / 1000000) %>%
          as_tibble() %>%
          mutate(Mes = factor(translate_date(month(mes, label = TRUE)),
                              levels = spanish_months))
      }
      
    })
    
    evol_ant <- reactive({
      evol() %>%
      filter(ano < max(ano) - 1)
    })
    
    evol_0 <- reactive({ 
      evol() %>%
      filter(ano == max(ano) - 1)
    })
    
    evol_1 <- reactive({
      evol() %>%
      filter(ano == max(ano))
    })
    
    evol_punto_1 <- reactive({
      evol() %>%
      filter(ano == max(ano)) %>%
      filter(mes == max(mes))
    })
    
    output$evol <- renderPlotly({
        
        ev <- evol_ant() %>%
            ggplot(aes(Mes, y)) +
            geom_line(aes(group = ano, 
                          text = paste0(Mes, " ", ano,
                                        "<br>", format(y, digits = 2, 
                                                       big.mark = ".",
                                                       decimal.mark = ",")
                                        ),
                          color = paste0("2010-", max(ano)))) +
            geom_line(data = evol_0(),
                      aes(group = ano,
                          text = paste0(Mes, " ", ano,
                                        "<br>", format(y, digits = 2, 
                                                       big.mark = ".",
                                                       decimal.mark = ",")
                          ),
                          color = paste0(max(ano)))) +
            geom_line(data = evol_1(),
                      aes(group = ano, 
                          text = paste0(Mes, " ", ano,
                                        "<br>", format(y, digits = 2, 
                                                       big.mark = ".",
                                                       decimal.mark = ",")
                          ),
                          color = paste0(max(ano))),
                      size = 1.2,
            ) +
            geom_point(data = evol_punto_1(),
                       aes(Mes,
                           y,
                           text = paste0(Mes, " ", ano,
                                         "<br>", format(y, digits = 2, 
                                                        big.mark = ".",
                                                        decimal.mark = ",")
                           ),
                           color = paste0(max(
                               month(evol_1()$mes,
                                     label = TRUE,
                                     abbr = FALSE)))
                           ),
                       size = 3) +
            theme_minimal() +
            theme(text = element_text(family = "Calibri"),
                  panel.grid.major = element_blank(), 
                  legend.position = "bottom") +
            labs(# podria sacar el texto del server
                x = "",
                y =  if_else(input$toggle1 == FALSE, 
                             "En millones de USD",
                             "En millones de toneladas"),
                color = "Años") +
            # podría sacarlo del server
            scale_y_continuous(labels = scales::comma_format(big.mark = ".",
                                                             decimal.mark = ",")) +
            scale_color_manual(
                "",
                # Podria sacarlo del server
                breaks = c(
                    paste0("2010-", max(evol_ant()$ano)),
                    paste0(max(evol_0()$ano)),
                    paste0(max(evol_1()$ano)),
                    paste0(max(
                        month(evol_1()$mes,
                              label = TRUE,
                              abbr = FALSE)
                    ))
                ),
                # podría sacarlo del server
                values = c("#F2F2F2",
                           "#00ADE6",
                           "#3175AC",
                           "#3175AC")
            )
        
        # Render grafico
        ggplotly(ev, tooltip = "text") %>% 
            layout(legend = list(orientation = "h", x = 0.05, y = -0.14))
    })
    
    # Dotplot subcat ----
    
    subset_dotplot <- reactive({
      
      if(input$toggle2 == FALSE){
        
        # Variacion FOB
        subset_dotplot <- subset_cat() %>% 
          filter(ano >= max(ano) - 1 & mes <= mesec) %>% 
          group_by(ano, cat_omc_1, cat_omc_2) %>% 
          summarise(y = sum(fob, na.rm = TRUE) / 1000000) %>% 
          pivot_wider(names_from = ano, values_from = y) %>% 
          rename(ano_0 = 3,
                 ano_1 = 4) %>% 
          mutate(var = (ano_1 / ano_0  - 1) * 100) %>% 
          arrange(ano_1)
        
      }else{
        
        # Variacion cantidades
        subset_dotplot <- subset_cat() %>% 
          filter(ano >= max(ano) - 1 & mes <= mesec) %>% 
          group_by(ano, cat_omc_1, cat_omc_2) %>% 
          # En millones de toneladas
          summarise(y = round(sum(pnet_kg, na.rm = TRUE) / 1000000000, digits = 2)) %>% 
          pivot_wider(names_from = ano, values_from = y) %>% 
          rename(ano_0 = 3,
                 ano_1 = 4) %>% 
          mutate(var = (ano_1 / ano_0  - 1) * 100) %>% 
          arrange(ano_1)
        
      }
    })
    
    
    output$pendiente <- renderPlotly({
      
      dotplot <- subset_dotplot() %>%
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
          text = paste0(
            "<b>",
            cat_omc_1, " ", max(evol_1()$ano),
            "</b>",
            "<br>",
            cat_omc_2,
            "<br>",
            format(
              ano_1,
              digits = 2,
              big.mark = ".",
              decimal.mark = ","
            ),
            "<br>",
            format(
              var,
              digits = 2,
              big.mark = ".",
              decimal.mark = ","
            ), " %"
          )
        ),
        size = 2) +
        geom_point(
          aes(
            ano_0,
            reorder(cat_omc_2, ano_1),
            color = cat_omc_1,
            text = paste0(
              "<b>",
              cat_omc_1, " ", max(evol_0()$ano),
              "</b>",
              "<br>",
              cat_omc_2,
              "<br>",
              format(
                ano_0,
                digits = 2,
                big.mark = ".",
                decimal.mark = ","
              )
            )
          ),
          alpha = 0.3,
          size = 2
        ) +
        scale_color_manual(breaks = unique(expo_shiny$cat_omc_1),
                           values = color_cat_1) +
        theme_minimal() +
        scale_x_log10(labels = scales::comma_format(big.mark = ".",
                                                    decimal.mark = ",")) +
        theme(
          text = element_text(family = "Calibri"),
          plot.margin = unit(c(1, 1, 1, -1), "cm"),
          legend.position = "none",
          legend.title = element_blank(),
          panel.grid.minor = element_blank()
        ) +
        labs(
          y = "",
          x = if_else(
            input$toggle2 == FALSE,
            "En millones de USD",
            "En millones de toneladas"
          )
        )
      
      
      ggplotly(dotplot, tooltip = "text")
      
    })
    
    # Reactive subcat ----
    
    subset_subcat_1 <- eventReactive(input$ver_subcat, {
      ano_1 %>% filter(cat_omc_2 %in% input$subcat)
    }, ignoreNULL = TRUE
    )
    
    subset_subcat_0 <- eventReactive(input$ver_subcat, {
      ano_0 %>% filter(cat_omc_2 %in% input$subcat)
    }, ignoreNULL = TRUE
    )
    
    subset_sub_cat <- eventReactive(
            input$ver_subcat, {
      expo_shiny %>% filter(cat_omc_2 %in% input$subcat)  
    }, ignoreNULL = TRUE
    )
    
    # Descripcion subcategoria ----
    
    output$desc_ano_1_subcat <- renderUI({
      
      number_0 <- format(round((
        sum(subset_subcat_1()$fob, na.rm = TRUE) / sum(subset_subcat_0()$fob, na.rm = TRUE) - 1
      ) * 100,
      digits = 1),
      big.mark = ".",
      decimal.mark = ",")
      
      header_0 <- format(round(sum(subset_subcat_1()$fob, na.rm = TRUE) / 1000000, digits = 0),
                         big.mark = ".",
                         decimal.mark = ",")
      
      descriptionBlock(
        number = paste0(number_0," %"),
        numberColor = if_else(number_0 >= 0, "green", "red"),
        numberIcon = if_else(number_0 >= 0,
                             "caret-up",
                             "caret-down"),
        header = paste(header_0),
        text = "millones de USD",
        rightBorder = TRUE
      )
    })
    
    output$desc_ano_1_ton_subcat <- renderUI({
      
      number_0 <- format(round((
        sum(subset_subcat_1()$pnet_kg, na.rm = TRUE) / sum(subset_subcat_0()$pnet_kg, na.rm = TRUE) - 1
      ) * 100,
      digits = 1),
      big.mark = ".",
      decimal.mark = ",")
      
      header_0 <-
        format(
          round(sum(subset_subcat_1()$pnet_kg, na.rm = TRUE) / 1000000000,
                digits = 1),
          big.mark = ".",
          decimal.mark = ","
        )
      
      descriptionBlock(
        number = paste0(number_0," %"),
        numberColor = if_else(number_0 >= 0, "green", "red"),
        numberIcon = if_else(number_0 >= 0, 
                             "caret-up", 
                             "caret-down"),
        header = paste0(header_0),
        text = "millones de TONELADAS",
      )
    })
    
    # Evolucion subcategorias ----
    
    evol_subcat <- reactive({

      if(input$toggle3 == TRUE) {
        evol <-  subset_sub_cat() %>%
          group_by(ano, mes) %>%
          # En millones de toneladas
          summarise(y = sum(pnet_kg, na.rm = TRUE) / 1000000000) %>%
          as_tibble() %>%
          mutate(Mes = factor(translate_date(month(mes, label = TRUE)),
                              levels = spanish_months))
      } else {
        evol <-  subset_sub_cat() %>%
          group_by(ano, mes) %>%
          summarise(y =  sum(fob, na.rm = TRUE) / 1000000) %>%
          as_tibble() %>%
          mutate(Mes = factor(translate_date(month(mes, label = TRUE)),
                              levels = spanish_months))
      }
      
    })
    
    evol_ant_subcat <- reactive({
      evol_subcat() %>%
        filter(ano < max(ano) - 1)
    })
    
    evol_0_subcat <- reactive({ 
      evol_subcat() %>%
        filter(ano == max(ano) - 1)
    })
    
    evol_1_subcat <- reactive({
      evol_subcat() %>%
        filter(ano == max(ano))
    })
    
    evol_punto_1_subcat <- reactive({
      evol_subcat() %>%
        filter(ano == max(ano)) %>%
        filter(mes == max(mes))
    })
    
    output$evol_sub_cat <- renderPlotly({
      
      
      
      ev <- evol_ant_subcat() %>%
        ggplot(aes(Mes, y)) +
        geom_line(aes(group = ano, 
                      text = paste0(Mes, " ", ano,
                                    "<br>", format(y, digits = 2, 
                                                   big.mark = ".",
                                                   decimal.mark = ",")
                      ),
                      color = paste0("2010-", max(ano)))) +
        geom_line(data = evol_0_subcat(),
                  aes(group = ano,
                      text = paste0(Mes, " ", ano,
                                    "<br>", format(y, digits = 2, 
                                                   big.mark = ".",
                                                   decimal.mark = ",")
                      ),
                      color = paste0(max(ano)))) +
        geom_line(data = evol_1_subcat(),
                  aes(group = ano, 
                      text = paste0(Mes, " ", ano,
                                    "<br>", format(y, digits = 2, 
                                                   big.mark = ".",
                                                   decimal.mark = ",")
                      ),
                      color = paste0(max(ano))),
                  size = 1.2,
        ) +
        geom_point(data = evol_punto_1_subcat(),
                   aes(Mes,
                       y,
                       text = paste0(Mes, " ", ano,
                                     "<br>", format(y, digits = 2, 
                                                    big.mark = ".",
                                                    decimal.mark = ",")
                       ),
                       color = paste0(max(
                         month(evol_1()$mes,
                               label = TRUE,
                               abbr = FALSE)))
                   ),
                   size = 3) +
        theme_minimal() +
        theme(text = element_text(family = "Calibri"),
              panel.grid.major = element_blank(), 
              legend.position = "bottom") +
        labs(# podria sacar el texto del server
          x = "",
          y =  if_else(input$toggle3 == FALSE, 
                       "En millones de USD",
                       "En millones de toneladas"),
          color = "Años") +
        # podría sacarlo del server
        scale_y_continuous(labels = scales::comma_format(big.mark = ".",
                                                         decimal.mark = ",")) +
        scale_color_manual(
          "",
          # Podria sacarlo del server
          breaks = c(
            paste0("2010-", max(evol_ant_subcat()$ano)),
            paste0(max(evol_0_subcat()$ano)),
            paste0(max(evol_1_subcat()$ano)),
            paste0(max(
              month(evol_1_subcat()$mes,
                    label = TRUE,
                    abbr = FALSE)
            ))
          ),
          # podría sacarlo del server
          values = c("#F2F2F2",
                     "#00ADE6",
                     "#3175AC",
                     "#3175AC")
        )
      
      # Render grafico
      ggplotly(ev, tooltip = "text") %>% 
        layout(legend = list(orientation = "h", x = 0.05, y = -0.14))
    })
    
    # Dotplot destinos ----
    
    dotplot_destinos <- reactive({
      
      if(input$toggle4 == FALSE){
        
        # Variacion FOB
        dotplot_destinos <- subset_sub_cat() %>% 
          filter(ano >= max(ano) - 1 & mes <= mesec) %>% 
          group_by(ano, nom_indec) %>% 
          summarise(y = sum(fob, na.rm = TRUE) / 1000000) %>% 
          pivot_wider(names_from = ano, values_from = y) %>% 
          rename(ano_0 = 2,
                 ano_1 = 3) %>% 
          mutate(var = (ano_1 / ano_0  - 1) * 100) %>% 
          filter(nom_indec != "Confidencial",
                 !is.na(ano_1)) %>% 
          slice_max(order_by = ano_1, n = 10, with_ties = FALSE)
        
      }else{
        
        # Variacion cantidades
        dotplot_destinos <- subset_sub_cat() %>% 
          filter(ano >= max(ano) - 1 & mes <= mesec) %>% 
          group_by(ano, nom_indec) %>% 
          # En millones de toneladas
          summarise(y = round(sum(pnet_kg, na.rm = TRUE) / 1000000000, digits = 2)) %>% 
          pivot_wider(names_from = ano, values_from = y) %>% 
          rename(ano_0 = 2,
                 ano_1 = 3) %>% 
          mutate(var = (ano_1 / ano_0  - 1) * 100) %>% 
          filter(nom_indec != "Confidencial",
                 !is.na(ano_1)) %>% 
          slice_max(order_by = ano_1, n = 10, with_ties = FALSE)
        
      }
    })
    
    
    output$destinos <- renderPlotly({
      
      destinos <- dotplot_destinos() %>%
        ggplot(aes(ano_1, reorder(nom_indec, ano_1))) +
        geom_segment(aes(
          x = ano_1,
          y = reorder(nom_indec, ano_1),
          xend = ano_0,
          yend = reorder(nom_indec, ano_1)
        ),
        color = "#00ADE6") +
        geom_point(aes(
          text = paste0(
            "<b>",
            nom_indec, " ", max(evol_1()$ano),
            "</b>",
            "<br>",
            format(
              ano_1,
              digits = 2,
              big.mark = ".",
              decimal.mark = ","
            ),
            "<br>",
            format(
              var,
              digits = 2,
              big.mark = ".",
              decimal.mark = ","
            ), " %"
          )
        ),
        color = "#00ADE6",
        size = 2) +
        geom_point(
          aes(
            ano_0,
            reorder(nom_indec, ano_1),
            # color = cat_omc_1,
            text = paste0(
              "<b>",
              nom_indec, " ", max(evol_0()$ano),
              "</b>",
              "<br>",
              format(
                ano_0,
                digits = 2,
                big.mark = ".",
                decimal.mark = ","
              )
            )
          ),
          color = "#00ADE6",
          alpha = 0.3,
          size = 2
        ) +
        # scale_color_manual(breaks = unique(expo_shiny$cat_omc_1),
        #                    values = color_cat_1) +
        theme_minimal() +
        scale_x_log10(labels = scales::comma_format(big.mark = ".",
                                                    decimal.mark = ",")) +
        theme(
          text = element_text(family = "Calibri"),
          plot.margin = unit(c(1, 1, 1, -1), "cm"),
          legend.position = "none",
          legend.title = element_blank(),
          panel.grid.minor = element_blank()
        ) +
        labs(
          y = "",
          x = if_else(
            input$toggle4 == FALSE,
            "En millones de USD",
            "En millones de toneladas"
          )
        )
      
      
      ggplotly(destinos, tooltip = "text")
      
    })
    
    # Dotplot productos ----
    
    dotplot_productos <- reactive({
      
      if(input$toggle5 == FALSE){
        
        # Variacion FOB
        dotplot_productos <- subset_sub_cat() %>% 
          filter(ano >= max(ano) - 1 & mes <= mesec) %>% 
          group_by(ano, ncm) %>% 
          summarise(y = sum(fob, na.rm = TRUE) / 1000000) %>% 
          pivot_wider(names_from = ano, values_from = y) %>% 
          rename(ano_0 = 2,
                 ano_1 = 3) %>% 
          mutate(var = (ano_1 / ano_0  - 1) * 100) %>% 
          filter(ncm != "99999999",
                 !is.na(ano_1)) %>% 
          slice_max(order_by = ano_1, n = 10, with_ties = FALSE)
        
      }else{
        
        # Variacion cantidades
        dotplot_productos <- subset_sub_cat() %>% 
          filter(ano >= max(ano) - 1 & mes <= mesec) %>% 
          group_by(ano, ncm) %>% 
          # En millones de toneladas
          summarise(y = round(sum(pnet_kg, na.rm = TRUE) / 1000000000, digits = 2)) %>% 
          pivot_wider(names_from = ano, values_from = y) %>% 
          rename(ano_0 = 2,
                 ano_1 = 3) %>% 
          mutate(var = (ano_1 / ano_0  - 1) * 100) %>% 
          filter(ncm != "99999999",
                 !is.na(ano_1)) %>% 
          slice_max(order_by = ano_1, n = 10, with_ties = FALSE)
        
      }
      
      dotplot_productos <- dotplot_productos %>% 
        left_join(cat_subcat_ncm) # cat_omc_1, cat_omc_2, descripcion ncm
      
    })
    
    
    output$productos <- renderPlotly({
      
      productos <- dotplot_productos() %>%
        ggplot(aes(ano_1, reorder(desc_ncm, ano_1))) +
        geom_segment(aes(
          x = ano_1,
          y = reorder(desc_ncm, ano_1),
          xend = ano_0,
          yend = reorder(desc_ncm, ano_1),
          color = cat_omc_1
        )) +
        geom_point(aes(
          color = cat_omc_1,
          text = paste0(
            "<b>",
            cat_omc_2, " ", max(evol_1()$ano),
            "</b>",
            "<br>",
            desc_ncm,
            "<br>",
            format(
              ano_1,
              digits = 2,
              big.mark = ".",
              decimal.mark = ","
            ),
            "<br>",
            format(
              var,
              digits = 2,
              big.mark = ".",
              decimal.mark = ","
            ), " %"
          )
        ),
        size = 2) +
        geom_point(
          aes(
            ano_0,
            reorder(desc_ncm, ano_1),
            color = cat_omc_1,
            text = paste0(
              "<b>",
              cat_omc_2, " ", max(evol_0()$ano),
              "</b>",
              "<br>",
              desc_ncm,
              "<br>",
              format(
                ano_0,
                digits = 2,
                big.mark = ".",
                decimal.mark = ","
              )
            )
          ),
          alpha = 0.3,
          size = 2
        ) +
        scale_color_manual(breaks = unique(expo_shiny$cat_omc_1),
                           values = color_cat_1) +
        theme_minimal() +
        scale_x_log10(labels = scales::comma_format(big.mark = ".",
                                                    decimal.mark = ",")) +
        theme(
          text = element_text(family = "Calibri"),
          plot.margin = unit(c(1, 1, 1, -1), "cm"),
          legend.position = "none",
          legend.title = element_blank(),
          panel.grid.minor = element_blank()
        ) +
        labs(
          y = "",
          x = if_else(
            input$toggle5 == FALSE,
            "En millones de USD",
            "En millones de toneladas"
          )
        )
      
      ggplotly(productos, tooltip = "text")
      
    })
    
    # output$pendiente <- renderPlotly({
    #     
    #     if(input$toggle2 == FALSE){
    #         
    #         # Variacion FOB
    #       subset_pendiente <- subset_cat() %>% 
    #         filter(ano >= max(ano) - 1 & mes <= mesec) %>% 
    #         group_by(ano, cat_omc_1, cat_omc_2) %>% 
    #         summarise(y = sum(fob, na.rm = TRUE) / 1000000) %>% 
    #         group_by(cat_omc_2) %>% 
    #         mutate(var = (y / lag(y, n = 1) - 1) * 100) %>% 
    #         as_tibble() 
    #       
    #     }else{
    #         
    #         # Variacion cantidades
    #         subset_pendiente <- subset_cat() %>% 
    #             filter(ano >= max(ano) - 1 & mes <= mesec) %>% 
    #             group_by(ano, cat_omc_1, cat_omc_2) %>% 
    #             # En millones de toneladas
    #             summarise(y = round(sum(pnet_kg, na.rm = TRUE) / 1000000000, digits = 2)) %>% 
    #             group_by(cat_omc_2) %>% 
    #             mutate(var = (y / lag(y, n = 1) - 1) * 100) %>% 
    #             as_tibble()
    #         
    #     }
    #     
    #     g_pend_1 <- subset_pendiente %>%
    #             ggplot(aes(as_factor(ano), y, group = cat_omc_2))
    #     
    #     g_pendiente <- g_pend_1 +
    #         geom_line(aes(color = cat_omc_1)) +
    #         geom_point(aes(color = cat_omc_1, 
    #                        text = paste0("<b>", cat_omc_1, "</b>",
    #                                      "<br>", cat_omc_2,
    #                                      "<br>", format(y, digits = 2,
    #                                                     big.mark = ".", 
    #                                                     decimal.mark = ",")
    #                                      )
    #                        ),
    #                    size = 3.5) +
    #         geom_text(
    #             data = subset(subset_pendiente, ano < max(ano)),
    #             aes(color = cat_omc_1),
    #             label = if_else(
    #                 str_length(unique(subset_pendiente$cat_omc_2)) > 22,
    #                 paste0(str_sub(
    #                     unique(subset_pendiente$cat_omc_2),
    #                     start = 1,
    #                     end = 15
    #                 ), "..."),
    #                 paste0(unique(subset_pendiente$cat_omc_2))
    #             ),
    #             size = 2.8,
    #             nudge_x = -0.3,
    #             hjust = 0,
    #         ) +
    #         geom_text(
    #             aes(color = cat_omc_1),
    #             label = if_else(
    #                 is.na(subset_pendiente$var),
    #                 paste0(""),
    #                 paste0(
    #                     format(
    #                         subset_pendiente$var,
    #                         digits = 2,
    #                         big.mark = ".",
    #                         decimal.mark = ","
    #                     ),
    #                     " %"
    #                 )
    #             ),
    #             size = 3,
    #             nudge_x = 0.20,
    #             hjust = 0
    #         ) +
    #         scale_color_manual(
    #             breaks = unique(expo_shiny$cat_omc_1),
    #             values = color_cat_1) +
    #         scale_y_log10() +
    #         theme_minimal() +
    #         theme(text = element_text(family = "Calibri"),
    #               plot.margin = unit(c(1.5,0,1,0), "cm"),
    #               legend.position = "none",
    #               legend.title = element_blank(),
    #               panel.grid = element_blank(),
    #               axis.text.y = element_blank()
    #         ) +
    #         labs(x = "",
    #              y = if_else(input$toggle2 == FALSE,
    #                          "En millones de USD",
    #                          "En millones de toneladas")
    #         )
    #     
    #     ggplotly(g_pendiente, tooltip = "text")
    # })
    
    # Ayuda, preguntas frecuentes y contacto ----
    
    
    
    
    # output$gif2 <- renderImage({
    #     return(list(src = "data/working.gif", contentType = "image/gif"))
    # }, deleteFile = FALSE)
    
}


# Run the application 
shinyApp(ui = ui, server = server)
