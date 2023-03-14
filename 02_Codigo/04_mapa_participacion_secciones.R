# Mapa de secciones

# Librerias: ----
library(tidyverse)
library(sf)
library(leaflet)

# Datos ----
# Cargamos los computos distritales de Tamaulipas, disponibles en este enlace: 
# https://computos2023-tamps.ine.mx/circunscripcion2/tamaulipas/votos-candidatura/grafica
bd <- read_delim("01_Datos/20230222_2200_COMPUTOS_SEN_TAMPS_ext/TAMPS_SEN_FED_ext_2023.csv", 
                 delim = "|", escape_double = FALSE, trim_ws = TRUE, 
                 skip = 6) %>% 
    janitor::clean_names() %>% 
    mutate(seccion = str_extract(seccion, pattern = "\\d+") %>% 
               as.numeric())

# Cargamos el shape secciones Tamaulipas: 
shp <- readRDS("01_Datos/shape_seccion.rds") %>% 
    select(entidad, CVE_INE, nom_mun, seccion) %>%  
    filter(entidad == "28")
# Cargamos el shape de municipios Tamaulipas: 
shp_munis <- readRDS("01_Datos/shape_munis.rds") %>% 
    select(entidad, nom_mun = nombre) %>%  
    filter(entidad == "28")
shp_munis$nom_mun[14] <- "GÜEMEZ"

# Cálculos ----
participacion <- bd %>% 
    mutate(lista_nominal = as.numeric(lista_nominal), 
           total_votos_calculados = as.numeric(total_votos_calculados)) %>% 
    group_by(entidad, id_distrito_federal, distrito_federal, 
             seccion) %>%
    summarise(lista_nominal = sum(lista_nominal, na.rm = T), 
              total_votos_calculados = sum(total_votos_calculados, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(pp = 100*(total_votos_calculados/lista_nominal))

# Mapa de participación ----
mapa <- left_join(shp, participacion %>% select(-entidad), 
                  by = c("seccion")) %>% 
    mutate(pp = ifelse(pp > 100, 
                       yes = 100, 
                       no = pp))

# Cuantiles de participación: 
quantile(na.omit(mapa$pp), seq(0, 1, 0.1))
mean(mapa$pp, na.rm = T)

mapa %>% 
    ggplot(aes(fill = #mapa$
                   pp)) + 
    geom_sf(color = "transparent") + 
    geom_sf(data = shp_munis,
            color = "black", 
            linetype = "11", 
            linewidth = 0.5,
            fill = "transparent") +   
    scale_fill_gradientn(colors = c("white",
                                    "#DEF5E5FF",
                                    "#A0DFB9FF",
                                    "#54C9ADFF",
                                    "#38AAACFF",
                                    "#348AA6FF",
                                    "#366A9FFF",
                                    "#40498EFF",
                                    "#3B2F5EFF",
                                    "#28192FFF",
                                    "#0B0405FF"), 
                         label  = scales::comma_format(suffix = "%")) + 
    labs(title = "Elecciones extraordinarias 2023 para Senador por mayoría relativa", 
         subtitle = "Porcentaje de Participación electoral", 
         fill = "Participación electoral (%)",
         caption = "Fuente: Elaboración propia con datos del PREP del INE.\nhttps://prep2023-tamps.ine.mx/senadurias/nacional/circunscripcion2/tamaulipas/votos-candidatura/grafica") + 
    theme_minimal() + 
    theme(legend.position = "bottom", 
          text = element_text(family = "Montserrat"), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          panel.grid = element_blank(), 
          plot.title.position = "plot",
          plot.title = element_text(hjust = 0, face = "bold", size = 12), 
          plot.subtitle = element_text(hjust = 0, size = 10)) + 
    guides(fill = guide_colorbar(barwidth = 20,
                                 barheight = 0.5,
                                 title.position = "top", 
                                 title.hjust = 0.5))

ggsave("03_Visualizaciones/mapa_participacion.png", 
       height = 5, 
       width = 6)


# shp %>% 
#     leaflet() %>% 
#     addPolygons(label = mapa$seccion, 
#                 # fill = mapa$color, 
#                 # fillOpacity = 1,
#                 # color = "black", 
#                 # size = 1,
#                 weight = 1) 

