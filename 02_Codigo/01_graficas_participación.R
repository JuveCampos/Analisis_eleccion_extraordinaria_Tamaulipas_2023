library(tidyverse)
library(ggpattern)
library(treemap)
library(treemapify)

# Participacion
part_tamps <- tibble(proceso = c("2023", "2022", "2021", "2019", "2018", "2016", "2013", "2012", "2010"), 
       participacion = c(21.69,53.1,52.68, 32.59, 62.32, 56.23, 48.36, 58.52, 44.26), 
       tipo_eleccion = c("Extraordinaria", 
                         "Elección a gobernador", 
                         "Elección ayuntamiento y congreso local", 
                         "Elecciones congreso local", 
                         "Elecciones federales", 
                         "Elección de gobernador, ayuntamientos y congreso local", 
                         "Elección ayuntamiento y congreso local", 
                         "Elecciones federales",
                         "Elección de gobernador, ayuntamientos y congreso local"
                         )) 

part_tamps %>% 
    ggplot(aes(x = proceso, 
               y = participacion,  
               fill = tipo_eleccion)) + 
    # geom_col(aes(y = 100), fill = "gray80") +
    geom_col_pattern(
        aes(y = 100),
        width = 0.9,
        pattern_density = 0.001,
        fill = "gray80",
        pattern_color = "gray70",
        pattern_fill = "gray70",
        pattern_spacing = 0.01,
        pattern_angle = -10,
        pattern_alpha = 0.5,
        pattern_size = 0.5,
        alpha = 0.5) +
    geom_col() + 
    geom_text(aes(label = str_c(participacion, "%")), 
              family = "Montserrat", 
              size = 4, 
              vjust = -0.5, 
              fontface = "bold") + 
    scale_y_continuous(expand = expansion(c(0, 0), 0), 
                       labels = scales::comma_format(suffix = "%")) + 
    scale_fill_manual(values = c("#448558", 
                                 "#6B0F1A", 
                                 "#1f5430", 
                                 "#8f212e",
                                 "#285b8f",
                                 "red"
                                 )) +
    labs(x = NULL,
             # "Proceso electoral", 
         y = "% de Participación", 
         title = "Participación en los últimos procesos electorales de Tamaulipas", 
         subtitle = "2010-2023", 
         caption = "Fuente: Elaboración propia con datos del IFE-INE y del instituto electoral de Tamaulipas. 2010-2023", 
         fill = "Tipo de elección") + 
    theme_minimal() + 
    theme(text = element_text(family = "Montserrat"), 
          plot.title = element_text(hjust = 0, face = "bold", size = 12), 
          plot.title.position = "plot",
          plot.subtitle = element_text(hjust = 0, size = 10), 
          legend.position = "bottom", 
          axis.line.x = element_line(color = "black")) + 
    guides(fill = guide_legend(title.position = "top", 
                               title.hjust = 0.5, 
                               nrow = 3))

ggsave("03_Visualizaciones/participacion.jpeg", 
       height = 6, 
       width = 7)
    
datos_treemap <- tibble(coalicion = c("PVEM", "PAN-PRI-PRD", "PT-MORENA", "Nulos", "Abstencion"), 
                        candidato = c("Manuel\nMuñoz Cano", "Imelda Margarita\nSanmiguel Sánchez", "José Ramón\nGómez Leal","Nulos", "Abstencion"),
                        votos = c(23735, 130240, 429556, 10728, 2144736)) %>% 
    mutate(porcentaje = 100*(votos/sum(votos)))

datos_treemap %>% 
    ggplot(aes(area = votos, 
               fill = coalicion)) + 
    geom_treemap(start = "topright") +
    geom_treemap_text(
        aes(label = str_c(candidato,"\n", 
                          prettyNum(votos, big.mark = ","), "\n", 
                          format(round(porcentaje, 1), nsmall = 1), "%")),
        colour = "white",
        start = "topright",
        fontface = "bold",
        place = "topright",
        reflow = F,
        family = "Montserrat",
        size = 15) +
    labs(title = "Proporción de los votos de la lista nominal por coalición", 
         subtitle = "Considerando los no-votos de la abstención") + 
    scale_fill_manual(values = c("gray60", 
                                 "purple", 
                                 "#252b9c", 
                                 "brown", 
                                 "#1c703f")) + 
    theme_minimal() + 
    theme(legend.position = "none", 
          text = element_text(family = "Montserrat"), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          panel.grid = element_blank(), 
          plot.title.position = "plot",
          plot.title = element_text(hjust = 0, face = "bold", size = 12), 
          plot.subtitle = element_text(hjust = 0, size = 10))

ggsave("03_Visualizaciones/treemap_votos.jpeg", 
       dpi = 300, 
       height = 6, 
       width = 8)

