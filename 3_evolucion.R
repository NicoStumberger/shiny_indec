
library(tidyverse)
library(hrbrthemes)
library(plotly)
library(lubridate)

# Grafico comparativo mensual, evolucion.

expo_shiny <- readRDS("data/expo_shiny_post2010.RDS")

evol <-  expo_shiny %>% 
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

# Esta es la idea del grafico. Un poco mas tuneado y ver si lo puedo hacer
# con Plotly, mas interactivo
g <- evol_ant %>%
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
                title = "Evolución de las exportaciones mensuales",
                subtitle = "Comparativo con años anteriores",
                x = "",
                y = "USD FOB en mill.",
                color = "Años"
        ) +
        scale_y_continuous(labels = scales::comma_format(big.mark = ".",
                                                         decimal.mark = ",")) +
        scale_color_manual("",
                           breaks = c(paste0("2010-", max(evol_ant$ano)),
                                      paste0(max(evol_0$ano)),
                                      paste0(max(evol_1$ano)), 
                                      paste0(max(month(evol_1$mes, 
                                                label = TRUE, 
                                                abbr = FALSE)))
                                      ),
                           values = c("#BFBFBF", 
                                      "#00ADE6", 
                                      "#3175AC", 
                                      "#3175AC"))


ggplotly(g, tooltip = c("x", "y"))

        


