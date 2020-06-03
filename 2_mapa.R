
library(tidyverse)
library(leaflet) # para mapa
library(rgdal) # para leer shapefiles
library(sf) # para manipular shapefiles y convertirlos a simple feature
library(htmltools)
library(readxl)

# Descripcion ncm para popup
desc_ncm <-
        read_excel("data/Diccionario gral (con enmienda 6) final.xlsx") %>%
        select(NCM_1, DESCRIPCION_NCM) %>%
        janitor::clean_names() %>%
        mutate(ncm = str_remove_all(ncm_1, pattern = "\\.")) %>%
        select(-ncm_1) %>%
        add_row(descripcion_ncm = "Confidencial",
                ncm = "99999999") %>%
        add_row(descripcion_ncm = "Creosoles y aceites de creosota",
                ncm = "27075090") %>%
        ## aca quiero acrotar la descripcion
        mutate(
                desc_ncm = if_else(
                        nchar(descripcion_ncm) > 29,
                        paste0(str_sub(
                                string = descripcion_ncm,
                                start = 1,
                                end = 29
                        ),
                        "..."),
                        descripcion_ncm
                ),
                desc_ncm = as.factor(paste0(desc_ncm, " (", ncm, ")"))
        )

saveRDS(desc_ncm, file = "data/desc_ncm.RDS")

# Preparar dataset para mapa
ano_1 <- readRDS("data/expo_shiny_post2010.RDS") %>% 
        mutate(iso3 = as_factor(iso3)) %>% 
        filter(ano == max(ano)) %>% 
        left_join(desc_ncm)

ano_0 <- readRDS("data/expo_shiny_post2010.RDS") %>% 
        filter(ano == max(ano) - 1 & mes <= max(ano_1$mes)) %>% 
        left_join(desc_ncm)

# No puede ser usado para TOTALES, porque van a desaparecer destinos con iso NA
ano_1 %>% 
        filter(is.na(iso3)) %>% 
        distinct(nom_indec)

skimr::skim(ano_1)

# Mapa

mapa_crudo <- readOGR(dsn = "data/nuevo_mundo_simpli_es" ,
                      layer = "nuevo_mundo_simpli_es",
                      verbose = FALSE, 
                      # Si no me levanta los nombres en espanol con tildes y enies mal
                      encoding = "UTF-8") %>% 
        st_as_sf(mapa_crudo)

# Atributos

ano_0_fob <- ano_0 %>% 
        group_by(iso3) %>% 
        summarise(fob_mm_tot_0 = sum(fob) / 1000000)

top_p <- ano_1 %>% 
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

mapa <- left_join(mapa_crudo, top_p, by = c("ISO3_CO" = "iso3"))
# El iso3 debiera ser factor y tener los mismos leves que ISO_CO

# Con ani_join se que registro de exportaciones me queda afuera del mapa.
# Veo que solo me quedo afuera el NA (los que no tienen iso3)
anti_join(top_p, mapa_crudo, by = c("iso3" = "ISO3_CO"))

pal <- colorNumeric(palette = c("#79dcff", "#01548A"),  # mejorar la paleta
                    domain = mapa$fob_mm_tot, # podria cambiarlo para dejarlo fuera?
                    na.color = "#BFBFBF")

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
                "Principales productos y participación:", "<br>",
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


mapa_leaf <-
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

mapa_leaf

# Cosas por hacer
#------------------
# Formato de numeros LISTO
# Que hacer con los NA. LISTO
# Paleta de colores y diseno gral del mapa MASOMENOS LISTO
# Incorprorar mas datos en el popup: variacion interanual; top 3-5 prod. LISTO
# Crear un if en los productos del pop up para que, cuando hay NA, no mostrarlo.
# Mejorar el join de paises por ISO. LISTO
# Hacer el codigo mas compacto. LISTO
# Ponerlo a produccion. LISTO
# Variacion interanual? LISTO
# Aumentar la amplitud de la paleta (celeste mas claro) LISTO

