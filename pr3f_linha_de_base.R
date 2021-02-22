# Linha de Base . R -------------
# Este script desenha um mapa com a linha de base do projeto PR3F

# 1. Limpar pacotes instalados ---------------------
#    E instalar os que vamos utilziar
#   
detach_all_packages <- function() {
  basic_packages_blank <-  c("stats",
                             "graphics",
                             "grDevices",
                             "utils",
                             "datasets",
                             "methods",
                             "base")
  basic_packages <- paste("package:", basic_packages_blank, sep = "")
  package_list <- search()[
    ifelse(unlist(gregexpr("package:", search())) == 1, TRUE, FALSE)]
  package_list <- setdiff(package_list, basic_packages)
  if (length(package_list) > 0)  for (package in package_list) {
    detach(package, character.only = TRUE, unload = TRUE)
    print(paste("package ", package, " detached", sep = ""))
  }
}
detach_all_packages()
# Ajustar o diretorio de Trabalho do projeto
path_to_wd <- "/Users/Marcos/Documents/COPEL-DIS/ctrl-1/"
if (is.null(path_to_wd) | !dir.exists(path_to_wd)) {
  print("Warning: No working directory for the current user")
} else {
  setwd(path_to_wd)
}
# Ajustar a opção de notação científica
options(scipen = 999)
# Instalar os pacotes que serão utilizados
cat("
library(rstudioapi)
library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(magrittr) # pipes
library(lintr) # code linting
library(sf) # spatial data handling
library(raster) # raster handling (needed for relief)
library(viridis) # viridis color scale
library(cowplot) # stack ggplots
library(readxl)
library(stringr)
library(broom)",
file = "manifest.R")

source("manifest.R")
unlink("manifest.R")
sessionInfo()


# 2. Ler arquivo trifaseamento  --------------------
# 
read_trifaseamento <- function(...) {
  tri <- read_xlsx(path = "./data/Estudo Trifaseamento mod Rodrigo v1.xlsx", 
                   sheet = "Trifaseamento",
                   range = "A1:AG1416",
                   .name_repair = ~make.unique(.x, sep = "_")
  );
  
  colnames(tri) <- iconv(colnames(tri), from = 'UTF-8', to = 'ASCII//TRANSLIT')
  colnames(tri) <- str_to_lower(colnames(tri))
  colnames(tri) <- colnames(tri) %>% 
    str_replace_all("(\\[|\\s|-)", "_") %>% 
    str_replace_all("[^0-9a-zA-Z_]", "")
  
  tri <- tri %>% 
    mutate(
      regional = str_to_title(regional),
      num_al_eletrico = as.character(num_al_eletrico))
}
tri <- read_trifaseamento()

# 3. Ler GEO com dados dos alimentadores --------------
# Vamos usar o primeiro ponto do atributo shape como localização
# do alimentador.
# Vamos converter as coordenadas para unir com dados do IBGE
read_geo_alim <- function() {
  alim <- readr::read_delim(file = "./copel_geo/202012_ALIMENTADOR.TXT",
                              delim = "|",
                              col_types = readr::cols(.default = readr::col_character()))
  colnames(alim) <- str_to_lower(colnames(alim))
  # Retira as coordenadas X, Y do primeiro ponto do atributo shape
  alim <- alim %>% 
    filter(!is.na(shape)) %>% 
    dplyr::select(municipio,
           num_alim_gdmase,
           nome_alim,
           shape) %>% 
    mutate(shape = str_replace(shape, "POLYGON\\(\\(", "")) %>% 
    mutate(x_coord = str_extract(shape, "[:digit:]+"),
           y_coord = str_trim(str_extract(shape, "[:space:][:digit:]+"),
                              side = "both")) %>% 
    dplyr::select(!shape)
    
  alim <- alim %>% 
    filter(x_coord < 999800) %>% 
    mutate(num_alim_gdmase = as.character(num_alim_gdmase)) 
}
alim <- read_geo_alim()

# 4. Carregar Shapefile do PR ---------------
# read PR shapefile
pr <- read_sf(dsn = "./pr_municipios",
              layer = "PR_Municipios_2019")


# 5. Vamos cruzar alimentadores com GEO ----------------
# usando o código do alimentador e a base geo COPEL com a coordenada
# ajustada.
tri <- tri %>% left_join(alim, by = c("num_al_eletrico" = "num_alim_gdmase"))
# 5.1 Alimentadores em Coordenadas! --------------
# 7 alimentadores ficaram sem coordenadas.
library(visdat) # Preliminary Visualisation of Data, CRAN v0.5.3
tri %>% visdat::vis_dat()
tri %>% vis_miss()
# Ver lista em:
# write_csv2(tri %>% filter(is.na(x_coord)) %>% dplyr::select(num_al_eletrico, nom_al_eletrico),
#           file = "./data/pr3f_alim_sem_coord.csv")

# 6. Apenas alimentadores com coordenadas --------------
coord_alim <- tri %>% 
  filter(!is.na(x_coord)) %>%   # Retire algum alimentador que ficou sem coordenada
  dplyr::select(num_al_eletrico, 
                nome_alim,
                regional, 
                chi, 
                total_uc, 
                municipio, 
                priorizacao, 
                x_coord, 
                y_coord)


# 7. Converter  coordenadas para WGS84 --------------
## Cria objeto st com geometry de pontos
coord_alim <- sf::st_as_sf(coord_alim, coords = c("x_coord","y_coord"))
## Converte coordenadas para poder usar com mapa do IBGE
coord_alim$geometry <- st_sfc(coord_alim$geometry,crs = 5532) #29192 ) 
coord_alim$geometry <- coord_alim$geometry %>% st_transform(crs = 4326)
## EPSG:4326 - WGS 84 -- WGS84 - World Geodetic System 1984, used in GPS
## EPSG:5641 - SIRGAS 2000 / Brazil Mercator

## Pega as novas coordenadas na forma de matriz e converte para data.frame
coord_alim <- coord_alim %>% 
  mutate(
    lat = unlist(map(coord_alim$geometry,1)),
    long = unlist(map(coord_alim$geometry,2))
  )

# 8. Tema do mapa -----------
theme_map <- function() {
  theme_minimal() +
    theme(
      text = element_text(
        #font = default_font_family,
        color = default_font_color),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#dbdbd9", 
                                      size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = default_background_color,
                                     color = NA),
      panel.background = element_rect(fill = default_background_color,
                                      color = NA),
      legend.background = element_rect(fill = default_background_color,
                                       color = NA),
      # borders and margins
      panel.border = element_blank(),
      # titles
      legend.title = element_text(hjust = 0),
      legend.text = element_text(hjust = 0),
      plot.title = element_text(hjust = 0),
      plot.subtitle = element_text(hjust = 0),
      # captions
      plot.caption = element_text(hjust = 1, size = 4)
    )
}

# 9.Mapa com alimentadores previstos no escopo original ------------
#    Opcionalmente o tamanho do ponto do alimentador pode ser mapeado
#     em um dos valores "priorizacao", "chi" ou "total_uc"
# Constantes:
default_font_color <- "#4e4d47"
default_background_color <- "white"
#default_font_family <- "arial"
default_caption <- paste0("COPEL-DIS",
                          "\nAuthors: Marcos Abreu (@abreums), 2021")
(myplot <- ggplot(data = pr) +
    geom_sf(
      mapping = aes(geometry=geometry),
      color = "lightgrey",
      size = 0.1
    ) +
    # Tentei usar geom_sf para os pontos, mas não consegui
    #  mapear informações de priorizacao, total_uc ou chi.
    #
    # geom_sf(data = tri, 
    #         mapping = aes(color = regional, shape = 20, size = 3)) +
    # add titles
    #
    # então vamos de geom_point
    # O filtro para os dados de geom_point é opcional
    geom_point(data = coord_alim, #(dplyr::filter(coord_alim, priorizacao > 0.1)),
               aes(x = lat, 
                   y = long, 
                   color = regional, 
                   size = priorizacao,
                   alimentador = nome_alim,
                   chi = chi,
                   municipio = municipio,
                   total_uc = total_uc
                   )) +
    labs(x = NULL,
         y = NULL,
         title = "Paraná Trifásico",
         subtitle = "Linha de Base",
         caption = default_caption) +
    # add theme
    theme_map()
)
library(plotly)
ggplotly(myplot, tooltip = c("alimentador", "total_uc",
                             "chi", "municipio"))
