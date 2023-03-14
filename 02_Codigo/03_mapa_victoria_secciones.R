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
# Resultados por alianza y seccion: 
votos_partido <- bd %>% 
    mutate(across(.cols = pan:lista_nominal, 
                  .fns = as.numeric)) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(candidato_vxt = sum(c_across(c(pan,pri,prd, pan_pri_prd, pan_pri, pan_prd, pri_prd)), na.rm = TRUE),
           candidato_jhh = sum(c_across(c(morena, pt, pt_morena)), na.rm = TRUE),
           candidato_pvem = pvem, 
           nulos_cand_no_reg = sum(c_across(c(candidato_a_no_registrado_a,
                                              votos_nulos)))) %>% 
    ungroup() %>% 
    group_by(entidad, id_distrito_federal, distrito_federal, seccion) %>%
    summarise(lista_nominal = sum(lista_nominal, na.rm = T), 
              total_votos_calculados = sum(total_votos_calculados, na.rm = T), 
              candidato_vxt = sum(candidato_vxt, na.rm = T),
              candidato_jhh = sum(candidato_jhh, na.rm = T),
              candidato_pvem = sum(candidato_pvem, na.rm = T), 
              nulos_cand_no_reg = sum(nulos_cand_no_reg))

# Obtenemos el candidato que gana, por coalición: 
diff_victoria <- votos_partido %>% 
    ungroup() %>% 
    select(seccion, contains("candidato"), lista_nominal) %>% 
    pivot_longer(cols = 2:4) %>% 
    group_by(seccion) %>% 
    mutate(rank = rank(-value)) %>% 
    arrange(seccion, rank) %>% 
    filter(rank != 3) %>% 
    mutate(diff = c(-1*diff(value), 0)) %>% 
    filter(rank == 1) %>% 
    mutate(pp_diff = 100*(diff/lista_nominal)) %>% 
    mutate(is.mas.5 = ifelse(pp_diff > 5, yes = T, no = F)) %>% 
    mutate(color = case_when(name == "candidato_jhh" ~ "brown", 
                             name == "candidato_pvem" ~ "green", 
                             name == "candidato_vxt" ~ "blue", 
                             TRUE ~ "gray")) 

# Visualizaciones ----
unique(mapa$nom_mun)

mapa <- left_join(shp, diff_victoria)

mapa %>% 
    ggplot(aes(fill = name)) + 
    geom_sf(color = "transparent",
            alpha = ifelse(mapa$is.mas.5, yes = 1, no = 0.5)) + 
    geom_sf(data = shp_munis,
            color = "black",
            linetype = "11",
            linewidth = 0.5,
            fill = "transparent") +
    scale_fill_manual(values = c("brown", "blue", "gray")) + 
    labs(title = "Elecciones extraordinarias 2023 para Senador por mayoría relativa", 
         subtitle = "Ganador por sección electoral", 
         caption = "Fuente: Elaboración propia con datos del PREP del INE.\nhttps://prep2023-tamps.ine.mx/senadurias/nacional/circunscripcion2/tamaulipas/votos-candidatura/grafica") + 
    theme_minimal() + 
    theme(legend.position = "bottom", 
          text = element_text(family = "Montserrat"), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          panel.grid = element_blank(), 
          plot.title.position = "plot",
          plot.title = element_text(hjust = 0, face = "bold", size = 12), 
          plot.subtitle = element_text(hjust = 0, size = 10))

ggsave("03_Visualizaciones/mapa_resultados.png", 
       height = 5, 
       width = 6)

# Reynosa: 
mapa <- left_join(shp, diff_victoria) %>% 
    filter(nom_mun == "Reynosa")

mapa %>% 
    ggplot(aes(fill = name)) + 
    geom_sf(color = "transparent",
            alpha = ifelse(mapa$is.mas.5, yes = 1, no = 0.5)) + 
    # geom_sf(data = shp_munis,
    #         color = "black", 
    #         linetype = "11", 
    #         linewidth = 0.5,
    #         fill = "transparent") +   
    scale_fill_manual(values = c("brown", "blue", "gray")) + 
    labs(title = "Elecciones extraordinarias 2023 para Senador por mayoría relativa", 
         subtitle = "Ganador por sección electoral", 
         caption = "Fuente: Elaboración propia con datos del PREP del INE.\nhttps://prep2023-tamps.ine.mx/senadurias/nacional/circunscripcion2/tamaulipas/votos-candidatura/grafica") + 
    theme_minimal() + 
    theme(legend.position = "bottom", 
          text = element_text(family = "Montserrat"), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          panel.grid = element_blank(), 
          plot.title.position = "plot",
          plot.title = element_text(hjust = 0, face = "bold", size = 12), 
          plot.subtitle = element_text(hjust = 0, size = 10))

ggsave("03_Visualizaciones/mapa_resultados_reynosa.png", 
       height = 5, 
       width = 6)

# Nuevo Laredo
mapa <- left_join(shp, diff_victoria) %>% 
    filter(nom_mun == "Nuevo Laredo")

mapa %>% 
    ggplot(aes(fill = name)) + 
    geom_sf(color = "transparent",
            alpha = ifelse(mapa$is.mas.5, yes = 1, no = 0.5)) + 
    # geom_sf(data = shp_munis,
    #         color = "black", 
    #         linetype = "11", 
    #         linewidth = 0.5,
    #         fill = "transparent") +   
    scale_fill_manual(values = c("brown", "blue", "gray")) + 
    labs(title = "Elecciones extraordinarias 2023 para Senador por mayoría relativa", 
         subtitle = "Ganador por sección electoral", 
         caption = "Fuente: Elaboración propia con datos del PREP del INE.\nhttps://prep2023-tamps.ine.mx/senadurias/nacional/circunscripcion2/tamaulipas/votos-candidatura/grafica") + 
    theme_minimal() + 
    theme(legend.position = "bottom", 
          text = element_text(family = "Montserrat"), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          panel.grid = element_blank(), 
          plot.title.position = "plot",
          plot.title = element_text(hjust = 0, face = "bold", size = 12), 
          plot.subtitle = element_text(hjust = 0, size = 10))

ggsave("03_Visualizaciones/mapa_resultados_nuevo_laredo.png", 
       height = 5, 
       width = 6)

# Matamoros
mapa <- left_join(shp, diff_victoria) %>% 
    filter(nom_mun == "Matamoros")

mapa %>% 
    ggplot(aes(fill = name)) + 
    geom_sf(color = "transparent",
            alpha = ifelse(mapa$is.mas.5, yes = 1, no = 0.5)) + 
    # geom_sf(data = shp_munis,
    #         color = "black", 
    #         linetype = "11", 
    #         linewidth = 0.5,
    #         fill = "transparent") +   
    scale_fill_manual(values = c("brown", "blue", "gray")) + 
    labs(title = "Elecciones extraordinarias 2023 para Senador por mayoría relativa", 
         subtitle = "Ganador por sección electoral", 
         caption = "Fuente: Elaboración propia con datos del PREP del INE.\nhttps://prep2023-tamps.ine.mx/senadurias/nacional/circunscripcion2/tamaulipas/votos-candidatura/grafica") + 
    theme_minimal() + 
    theme(legend.position = "bottom", 
          text = element_text(family = "Montserrat"), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          panel.grid = element_blank(), 
          plot.title.position = "plot",
          plot.title = element_text(hjust = 0, face = "bold", size = 12), 
          plot.subtitle = element_text(hjust = 0, size = 10))

ggsave("03_Visualizaciones/mapa_resultados_matamoros.png", 
       height = 5, 
       width = 6)

# Tampico
mapa <- left_join(shp, diff_victoria) %>% 
    filter(nom_mun == "Tampico")

mapa %>% 
    ggplot(aes(fill = name)) + 
    geom_sf(color = "transparent",
            alpha = ifelse(mapa$is.mas.5, yes = 1, no = 0.5)) + 
    scale_fill_manual(values = c("brown", "blue", "gray")) + 
    labs(title = NULL, 
         subtitle = NULL, 
         caption = NULL) + 
    theme_minimal() + 
    theme(legend.position = "bottom", 
          text = element_text(family = "Montserrat"), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          panel.grid = element_blank(), 
          plot.title.position = "plot",
          plot.title = element_text(hjust = 0, face = "bold", size = 12), 
          plot.subtitle = element_text(hjust = 0, size = 10))

ggsave("03_Visualizaciones/mapa_resultados_tampico.png", 
       height = 5, 
       width = 6)


## Mapa de leaflet ----
mapa_leaflet <- mapa %>% 
    mutate(color2 = case_when(name == "candidato_jhh" & is.mas.5 == TRUE ~ "brown", 
                              name == "candidato_vxt" & is.mas.5 == TRUE ~ "blue", 
                              name == "candidato_jhh" & is.mas.5 == FALSE ~ "#CB9997", 
                              name == "candidato_vxt" & is.mas.5 == FALSE ~ "#A3B0FB", 
                              TRUE ~ "gray"
    ))

mapa_leaflet %>% 
    leaflet() %>% 
    addPolygons(fillColor = mapa_leaflet$color2, 
                fillOpacity = 1, 
                color = "white", 
                weight = 1) 
