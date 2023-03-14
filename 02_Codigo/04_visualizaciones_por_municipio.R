# Cálculos a nivel municipal ----

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

# Cálculos: ----
# Obtenemos los Resultados por alianza y seccion: 
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

# Juntamos con la info del shape de secciones para sacar a que municipio pertenece cada sección
votos_seccion <- votos_partido %>%
    ungroup() %>% 
    select(seccion, contains("candidato"), lista_nominal, total_votos_calculados, contains("nulos")) %>% 
    left_join(shp %>% as_tibble() %>% select(-geometry), 
              by = c("seccion")) %>% 
    select(CVE_INE, nom_mun, seccion, lista_nominal, total_votos_calculados, 
           candidato_pvem, candidato_jhh, candidato_vxt, 
           contains("nulos"))

# Sacamos los datos por municipio: 
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
# Visualizaciones ----
barras_mpios_datos <- votos_municipio %>% 
    pivot_longer(3:ncol(.)) %>% 
    mutate(pp = 100*(value/lista_nominal)) %>% 
    filter(name != "total_votos_calculados")

barras_mpios_datos %>% 
    ggplot(aes(x = factor(nom_mun, levels = rev(unique(nom_mun))), 
               y = pp, 
               fill = name)) + 
    geom_col(color = "white") +
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
    coord_flip() +
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
          plot.subtitle = element_text(hjust = 0, size = 10))

# Guardamos gráfica: 
ggsave("03_Visualizaciones/barras_municipios.jpeg", 
       height = 6, 
       width = 6, 
       device = "jpeg")

# Mapa de victorias por municipio: 
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

ggsave("03_Visualizaciones/mapa_victoria_mpio.jpeg", 
       device = "jpeg", 
       height = 10, 
       width = 10, 
       dpi = 300)


mapa_mpios_bb <- mapa_mpios %>% 
    mutate(ctrd_x = (st_centroid(.) %>% st_coordinates())[,1],
           ctrd_y = (st_centroid(.) %>% st_coordinates())[,2]) %>% 
    as_tibble() %>% 
    select(-geometry) %>% 
    mutate(value2 = value/max(value)) %>% 
    mutate()

mapa_mpios_bb %>% 
    ggplot() + 
    geom_sf(data = shp_munis, 
            fill = "gray90", 
            color = "black") + 
    geom_point(aes(x = ctrd_x,
                   y = ctrd_y, 
                   # size = value2,
                   fill = name), 
               size = (mapa_mpios_bb$value)/3000,
               pch = 21, 
               color = "black", 
               alpha = 0.5)  + 
    ggrepel::geom_text_repel(aes(x = ctrd_x,
                  y = ctrd_y, 
                  label = ifelse(value > 10000, 
                                 str_wrap(nom_mun, 10), 
                                 "")), 
              size = 3, 
              fontface = "bold",
              family = "Montserrat") + 
    scale_fill_manual(values = c("brown", "blue")) + 
    theme_minimal() + 
    theme(legend.position = "none", 
          text = element_text(family = "Montserrat"), 
          axis.text = element_blank(), 
          axis.title = element_blank(), 
          axis.ticks = element_blank(), 
          panel.grid = element_blank(), 
          plot.title.position = "plot",
          plot.title = element_text(hjust = 0, face = "bold", size = 12), 
          plot.subtitle = element_text(hjust = 0, size = 10))

ggsave("03_Visualizaciones/mapa_victoria_burbujas_mpio.jpeg", 
       device = "jpeg", 
       height = 5, 
       width = 5, 
       dpi = 300)

# Tabla de excel: 
mapa_mpios_bb %>% 
    select(nom_mun, name) %>% 
    openxlsx::write.xlsx("01_Datos/Datos_resultados_mpio.xlsx")
