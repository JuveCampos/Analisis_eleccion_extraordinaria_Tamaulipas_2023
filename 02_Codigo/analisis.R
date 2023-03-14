# Librerias. ----
library(tidyverse)
library(sf)
library(leaflet)

# Datos ----
# PREP Tamaulipas: 
bd <- read_delim("01_Datos/20230222_2200_COMPUTOS_SEN_TAMPS_ext/TAMPS_SEN_FED_ext_2023.csv", 
                 delim = "|", escape_double = FALSE, trim_ws = TRUE, 
                 skip = 6) %>% 
    janitor::clean_names() %>% 
    mutate(seccion = str_extract(seccion, pattern = "\\d+") %>% 
               as.numeric())

# Shape secciones Tamaulipas: 
shp <- readRDS("01_Datos/shape_seccion.rds") %>% 
    select(entidad, CVE_INE, nom_mun, seccion) %>%  
    filter(entidad == "28")
  
# Shape municipios Tamaulipas: 
shp_munis <- readRDS("01_Datos/shape_munis.rds") %>% 
    select(entidad, nom_mun = nombre) %>%  
    filter(entidad == "28")
shp_munis$nom_mun[14] <- "GÜEMEZ"

# Participacion:     
participacion <- bd %>% 
    mutate(lista_nominal = as.numeric(lista_nominal), 
           total_votos_calculados = as.numeric(total_votos_calculados)) %>% 
    group_by(entidad, id_distrito_federal, distrito_federal, 
             seccion) %>%
    summarise(lista_nominal = sum(lista_nominal, na.rm = T), 
              total_votos_calculados = sum(total_votos_calculados, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(pp = 100*(total_votos_calculados/lista_nominal))

participacion

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

# Diferencia de la victoria: 
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

# Votos por seccion: 
votos_seccion <- votos_partido %>%
    ungroup() %>% 
    select(seccion, contains("candidato"), lista_nominal, total_votos_calculados, contains("nulos")) %>% 
    left_join(shp %>% as_tibble() %>% select(-geometry), 
              by = c("seccion")) %>% 
    select(CVE_INE, nom_mun, seccion, lista_nominal, total_votos_calculados, 
           candidato_pvem, candidato_jhh, candidato_vxt, 
           contains("nulos"))

# votos_seccion %>% 
#     transmute(seccion, 
#               lista_nominal,
#               total_votos_calculados, 
#               tvc2 = (candidato_jhh + candidato_pvem + candidato_vxt + nulos_cand_no_reg), 
#               v2 = total_votos_calculados - tvc2)
# names(votos_seccion)
votos_municipio = votos_seccion %>% 
    group_by(nom_mun) %>% 
    summarise(across(lista_nominal:nulos_cand_no_reg, 
                     sum)) %>% 
    mutate(abst = lista_nominal - total_votos_calculados) %>% 
    mutate(nom_mun = ifelse(is.na(nom_mun), yes = "Votos desde el extranjero", 
                            no = nom_mun)) %>% 
    mutate(nom_mun = ifelse(nom_mun == "Gñemez",
                            yes = "Güemez", 
                             no = nom_mun))

barras_mpios_datos <- votos_municipio %>% 
    pivot_longer(3:ncol(.)) %>% 
    mutate(pp = 100*(value/lista_nominal)) %>% 
    filter(name != "total_votos_calculados")
    
orden <- barras_mpios_datos %>% 
    filter(name == "abst") %>% 
    arrange(pp) %>% 
    pull(nom_mun)

barras_mpios_datos %>% 
    ggplot(aes(x = factor(nom_mun, rev(orden)),
               y = pp, 
               fill = name)) + 
    geom_col(color = "white") +
    coord_flip() +
    geom_text(data = barras_mpios_datos %>% filter(name == "abst"),
              aes(label = str_c(format(round(100-pp, 1), nsmall = 1), "%"), 
                  x = nom_mun, 
                  y = 105),
              size = 2) +
    scale_fill_manual(values = c("gray70", 
                                 "brown", 
                                 "olivedrab", 
                                 "skyblue", 
                                 "purple")) + 
    scale_y_continuous(expand = expansion(c(0, 0.1), 0), 
                       labels = scales::comma_format(suffix = "%")) + 
    
    labs(title = "¿Cómo fueron los resultados por municipio?", 
         subtitle = "Resultados por municipio y coalición.\nElección extraordinaria Tamaulipas 2023", 
         caption = "Fuente: Elaboración propia con datos del conteo distrital del INE", 
         x = NULL, y = NULL) +
    theme_minimal() +
theme(legend.position = "bottom",
      text = element_text(family = "Montserrat"),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0, face = "bold", size = 12),
      plot.subtitle = element_text(hjust = 0, size = 12))

ggsave("03_Visualizaciones/barras_municipios.jpeg", 
       height = 6, 
       width = 6, 
       device = "jpeg")

# Mapa de victoria por municipio ----
mapa_mpios <- barras_mpios_datos %>% 
    mutate(nom_mun = toupper(nom_mun)) %>% 
    filter(name %in% c("candidato_pvem", 
                       "candidato_jhh", 
                       "candidato_vxt")) %>% 
    group_by(nom_mun) %>% 
    mutate(rank = rank(-pp)) %>% 
    filter(rank == 1) %>% 
    filter(nom_mun != "VOTOS DESDE EL EXTRANJERO") 

mapa_mpios <- left_join(shp_munis, mapa_mpios, by = c("nom_mun"))

mapa_mpios %>% 
    ggplot(aes(fill = name)) +
    geom_sf(color = "white") + 
    scale_fill_manual(values = c("brown", "blue")) + 
    theme_minimal() + 
    theme(legend.position = "bottom", 
          text = element_text(family = "Montserrat"), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          panel.grid = element_blank(), 
          plot.title.position = "plot",
          plot.title = element_text(hjust = 0, face = "bold", size = 12), 
          plot.subtitle = element_text(hjust = 0, size = 10))

# mapa_mpios$nom_mun[!(mapa_mpios$nom_mun %in% shp_munis$nom_mun)]

# Mapa de victoria por sección ----
mapa <- left_join(shp, diff_victoria) 

# Generamos el mapa: 
prop.table(table(mapa$name))

# Mapa de victoria: 

# Todo el estado: 
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
    
ggsave("03_Visualizaciones/mapa_resultados.png", 
       height = 5, 
       width = 6)


# Mapa de participación ----
mapa <- left_join(shp, participacion %>% select(-entidad), 
                  by = c("seccion")) %>% 
    mutate(pp = ifelse(pp > 100, 
                       yes = 100, 
                       no = pp))

quantile(na.omit(mapa$pp), seq(0, 1, 0.1))


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

# Mapa de participación: 

# shp %>% 
#     leaflet() %>% 
#     addPolygons(label = mapa$seccion, 
#                 # fill = mapa$color, 
#                 # fillOpacity = 1,
#                 # color = "black", 
#                 # size = 1,
#                 weight = 1) 
