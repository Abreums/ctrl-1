# pr3f_status_report.R ----
#
#  Filtrar obras acabadas.
#  Obter data de término do projeto da obra com término mais tarde
#  Fazer mapa mostrando obra acabada no tempo

# Limpar todos os pacotes instalados ----
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
# ajustar a opção de notação científica
options(scipen = 999)

# Libraries ----
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.0 
library(lintr) # A 'Linter' for R Code, CRAN v2.0.1
library(sf) # Simple Features for R, CRAN v0.9-6 
library(readxl) # Read Excel Files, CRAN v1.3.1
library(stringr) # Simple, Consistent Wrappers for Common String Operations, CRAN v1.4.0
library(broom)  # Convert Statistical Objects into Tidy Tibbles, CRAN v0.7.3
library(ggrepel)

# PowerBI SAP PR3F data ----
read_bi_pr3f <- function(bi_file, bi_sheet = 1) {
  bi <- read_xlsx(path = bi_file, 
                  sheet = bi_sheet,
                  .name_repair = ~make.unique(.x, sep = "_"),
                  skip = 2)
  colnames(bi) <- iconv(colnames(bi), from = 'UTF-8', to = 'ASCII//TRANSLIT')
  colnames(bi) <- str_to_lower(colnames(bi))
  colnames(bi) <- colnames(bi) %>% 
    str_replace_all("(\\[|\\s|-)", "_") %>% 
    str_replace_all("[^0-9a-zA-Z_]", "")
  bi <- bi %>% 
    rename(dpo = obras_plan_dpo) %>% 
    mutate(
      dpo = factor(dpo),
      vpo = factor(vpo),
      tipo_de_obra = factor(tipo_de_obra,
                            levels = c("Topografia", 
                                       "Projeto", 
                                       "Investimento"))
    )
  bi <- bi %>% 
    mutate(municipio = str_to_upper(municipio))
  
  bi <- bi %>% arrange(dpo, vpo, po, id_da_obra, tipo_de_obra, 
                       projeto, estagio) %>% 
    tibble()
}
bi_pr3f = "./data/20210113pr3f.xlsx"
pr3f <- read_bi_pr3f(bi_pr3f)

# PowerBI SAP Obras data ----
read_bi_obras <- function(bi_file, bi_sheet = 1) {
  bi <- read_xlsx(path = bi_file, 
                  sheet = bi_sheet,
                  .name_repair = ~make.unique(.x, sep = "_"),
                  skip = 2)
  colnames(bi) <- iconv(colnames(bi), from = 'UTF-8', to = 'ASCII//TRANSLIT')
  colnames(bi) <- str_to_lower(colnames(bi))
  colnames(bi) <- colnames(bi) %>% 
    str_replace_all("(\\[|\\s|-)", "_") %>% 
    str_replace_all("[^0-9a-zA-Z_]", "")
  bi <- bi %>% 
    filter(po %in% c("PR TRIF 0", "PR TRIF 1", "PR TRIF 2", "PR TRIF 3")) %>% 
    filter(projeto != "") %>% 
    mutate(
      tipo_de_obra = factor(tipo_de_obra,
                            levels = c("Topografia", 
                                       "Projeto", 
                                       "Investimento"))
    )
}
bi_obras = "./data/2020-12-18 bi Obras.xlsx"
obras <- read_bi_obras(bi_obras)


# Sumário Obras 1 por linha ----
sumario_obras_emexecucao <- function(pr3f) {
  pr3f <- pr3f %>%
    filter(tipo_de_obra == "Investimento") %>% 
    filter(situacao == "Em execução") %>% 
    group_by(id_da_obra) %>% 
    summarize(vpo = first(vpo),
              po = first(po),
              descricao = first(descricao),
              situacao = first(situacao),
              dt_previsao = max(data_previsao, na.rm = TRUE),
              us_prevista = max(us_prevista, na.rm = TRUE),
              us_realizada = sum(us_realizada, na.rm = TRUE),
              dt_ini_aes = min(dt_ini_aes, na.rm = TRUE),
              dt_ini_real = min(dt_ini_real, na.rm = TRUE),
              n_prj = n()) %>% 
    arrange(n_prj)
}
sum_obras_emexecuacao <- sumario_obras_emexecucao(obras)

sumario_obras_emprojeto <- function(obras) {
  obras <- obras %>%
    filter(tipo_de_obra == "Investimento") %>% 
    filter(situacao %in% c("Projeto Concluído", "Em Projeto")) %>% 
    group_by(id_da_obra) %>% 
    summarize(vpo = first(vpo),
              po = first(po),
              descricao = first(descricao),
              situacao = first(situacao),
              dt_previsao = max(data_previsao, na.rm = TRUE),
              us_prevista = max(us_prevista, na.rm = TRUE),
              us_realizada = sum(us_realizada, na.rm = TRUE),
              dt_ini_aes = min(dt_ini_aes, na.rm = TRUE),
              dt_ini_real = min(dt_ini_real, na.rm = TRUE),
              n_prj = n()) %>% 
    arrange(n_prj)
}
sum_obras_emprojeto <- sumario_obras_emprojeto(obras)


# Extrai tipo obra do id_da_obra ----
separa_info_id_obra <- function(df) {
  df <- df %>% separate(id_da_obra,
                        sep = c(2,4,9),
                        into = c("tipo_obra", 
                                 "cod_vpo", 
                                 "cod_obra", 
                                 "ano_obra"),
                        remove = FALSE)
  # Carrega códigos
  prefixo_categoria <- read_xlsx(path = "./data/prefixos.xlsx", 
                                 sheet = "p12");
  prefixo_regional <-  read_xlsx(path = "./data/prefixos.xlsx",
                                 sheet = "p34")
  prefixo_regional <- prefixo_regional %>% 
    dplyr::select(cod_vpo, cidade, regional)
  
  df <- df %>% left_join(prefixo_categoria, by = c("tipo_obra" = "tipo_obra"))
  df <- df %>% left_join(prefixo_regional, by = c("cod_vpo" = "cod_vpo"))
}
sum_obras_emexecuacao <- separa_info_id_obra(sum_obras_emexecuacao)
sum_obras_emprojeto <- separa_info_id_obra(sum_obras_emprojeto)

# Ler Alimentadores x Apd x Obras ----
read_al_apd_obra <- function(file) {
  al_apd_obra <- read_xlsx(path = file,
                           sheet = 1)
  al_apd_obra <- al_apd_obra %>% 
    filter(!is.na(num_al_eletrico)) %>% 
    filter(!is.na(id_da_obra)) %>% 
    dplyr::select(num_al_eletrico, id_da_obra) %>% 
    tibble()
}
al_apd_obra = "./data/alim_apd_id_da_obra.xlsx"
al_obra <- read_al_apd_obra(al_apd_obra)

# 3. Ler COPEL GEO para buscar localização dos alimentadores ----
read_geo_alim <- function(geo_flie) {
  #geo_file = "./copel_geo/202012_ALIMENTADOR.TXT"
  alim <- readr::read_delim(file = geo_file,
                            delim = "|",
                            col_types = readr::cols(.default = readr::col_character()))
  colnames(alim) <- str_to_lower(colnames(alim))
  # Retira as coordenadas X, Y do primeiro ponto do atributo shape
  alim <- alim %>% 
    filter(!is.na(shape)) %>% 
    dplyr::select(municipio,
                  num_alim_gdmase,
                  nome_alim,
                  qtde_uc_resid_hali,
                  qtde_uc_comerc_hali,
                  qtde_uc_indust_hali,
                  shape) %>% 
    mutate(shape = str_replace(shape, "POLYGON\\(\\(", "")) %>% 
    mutate(x_coord = str_extract(shape, "[:digit:]+"),
           y_coord = str_trim(str_extract(shape, "[:space:][:digit:]+"),
                              side = "both")) %>% 
    dplyr::select(!shape)
  
  alim <- alim %>% 
    filter(x_coord < 999800) %>% 
    mutate(num_alim_gdmase = as.character(num_alim_gdmase)) %>% 
    tibble()
  
  # Falta transformar as coordenadas para padrão WGS
  # Convert crs ----
  alim <- sf::st_as_sf(alim, coords = c("x_coord","y_coord"))
  # Converte coordenadas para poder usar com mapa do IBGE
  alim$geometry <- st_sfc(alim$geometry,crs = 5532) #29192 ) 
  alim$geometry <- alim$geometry %>% st_transform(crs = 4326)
  # EPSG:4326 - WGS 84 -- WGS84 - World Geodetic System 1984, used in GPS
  # EPSG:5641 - SIRGAS 2000 / Brazil Mercator
  alim <- alim %>% 
    dplyr::mutate(lat = sf::st_coordinates(geometry)[,1],
                  lon = sf::st_coordinates(geometry)[,2])
  alim <- alim %>% 
    select(num_alim_gdmase, 
           lat, 
           lon,
           qtde_uc_resid_hali,
           qtde_uc_comerc_hali,
           qtde_uc_indust_hali) %>% 
    st_drop_geometry()
}
# Busca coordenadas de alimentadores
geo_file = "./copel_geo/202012_ALIMENTADOR.TXT"
alim_geo <- read_geo_alim(geo_file)
al_obra <- al_obra %>% 
  mutate(num_al_eletrico = as.character(num_al_eletrico))
# left join para buscar as coordenadas
al_obra <- al_obra %>% 
  left_join(alim_geo, 
            by = c("num_al_eletrico" = "num_alim_gdmase"))

# Obras left_join com dados geo de alimentadores ----
sum_obras_emexecuacao <- sum_obras_emexecuacao %>% 
  left_join(al_obra,
            by = c("id_da_obra" = "id_da_obra"))

sum_obras_emprojeto <- sum_obras_emprojeto %>% 
  left_join(al_obra,
            by = c("id_da_obra" = "id_da_obra"))


# Dados faltantes de geo ----
library(visdat) # Preliminary Visualisation of Data, CRAN v0.5.3
# library(naniar) # Data Structures, Summaries, and Visualisations for Missing Data, CRAN v0.6.0
sum_obras_emexecuacao %>% vis_dat()
sum_obras_emprojeto %>% vis_dat()
# 5. Ler PR shapefile ----
read_pr <- function() {
  pr <- read_sf(dsn = "./pr_municipios",
                layer = "PR_Municipios_2019")
}
pr <- read_pr()
# Vamos utilizar centroide do município para 
# localizar as obras sem coordenadas
read_mun_pr <- function() {
  pr <- pr %>% 
    dplyr::mutate(clat = sf::st_coordinates(st_centroid(geometry))[,1],
                  clon = sf::st_coordinates(st_centroid(geometry))[,2])
  mun_pr <- pr # precisamos apenas da lat e long
  mun_pr <- mun_pr %>% 
    select(CD_MUN, NM_MUN, clat, clon) %>% 
    st_drop_geometry()
}
mun_pr <- read_mun_pr()
# mapa do paraná com centróides
# (ggplot() +
#     geom_sf(data = pr,
#             mapping = aes(geometry=geometry),
#             color = "lightgrey",
#             size = 0.1) +
#     geom_point(data = mun_pr,
#                mapping = aes(x = lat, y = lon), 
#                size =1)
# )

# Muda Nome Cidades 
# Vamos retirar acentos, tils e cedilhas dos nomes das cidades
muda_nome_cidades <- function(df) {
  df <- df %>% 
    mutate(NM_MUN = str_to_upper(NM_MUN),
           NM_MUN = str_replace_all(NM_MUN, "Á", "A"),
           NM_MUN = str_replace_all(NM_MUN, "Ã", "A"),
           NM_MUN = str_replace_all(NM_MUN, "Â", "A"),
           NM_MUN = str_replace_all(NM_MUN, "É", "E"),
           NM_MUN = str_replace_all(NM_MUN, "Ê", "E"),
           NM_MUN = str_replace_all(NM_MUN, "Í", "I"),
           NM_MUN = str_replace_all(NM_MUN, "Ó", "O"),
           NM_MUN = str_replace_all(NM_MUN, "Õ", "O"),
           NM_MUN = str_replace_all(NM_MUN, "Ô", "O"),
           NM_MUN = str_replace_all(NM_MUN, "Ú", "U"),
           NM_MUN = str_replace_all(NM_MUN, "Ç", "C"))
}
mun_pr <- muda_nome_cidades(mun_pr)

# 6. Vincular cidade a cada obra --------
vincula_cidade_geo <- function(df, mun_pr) {
  df <- df %>% 
    left_join(mun_pr,
            by = c("cidade" = "NM_MUN")) %>% 
    mutate(lat = case_when(is.na(lat) ~ clat,
                           TRUE ~ lat),
           lon = case_when(is.na(lon) ~ clon,
                           TRUE ~ lon)) %>% 
    select(!c(clat, clon))
  
}
sum_obras_emexecuacao <- vincula_cidade_geo(sum_obras_emexecuacao, mun_pr)
sum_obras_emprojeto <- vincula_cidade_geo(sum_obras_emprojeto, mun_pr)

sum_obras_emexecuacao %>% vis_dat()
sum_obras_emprojeto %>% vis_dat()


# 7. Extrai tipo obra do id_da_obra ----

# 8. Tema do mapa ----
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
      #legend.text = element_text(hjust = 0),
      plot.title = element_text(hjust = 0),
      plot.subtitle = element_text(hjust = 0),
      # captions
      plot.caption = element_text(hjust = 1, size = 4)
    )
}



# Constantes
default_font_color <- "#4e4d47"
default_background_color <- "white"
#default_font_family <- "arial"
default_caption <- paste0("COPEL-DIS",
                          "\nAuthors: Marcos Abreu (@abreums), 2021")

# 9. Mapa Em execução ----
(myplot <- ggplot() +
    geom_sf(data = pr,
            mapping = aes(geometry=geometry),
            color = "lightgrey",
            size = 0.1
    ) +
    geom_jitter(data = (sum_obras_emexecuacao %>% filter(!is.na(lat))),
                mapping = aes(x = lat, y = lon, 
                              size = us_prevista,
                              colour = regional,
                              fill = regional),
                width = 0.05, height = 0.05,
                alpha = .6) +
  #   geom_label_repel(data = concluidas,
  #                    aes(x = lat, y = lon,
  #                        label = id_da_obra),
  #                    box.padding = unit(0.6, "lines"),
  #                    size = 2,
  #                    max.overlaps = Inf) +
    theme_map() +
    labs(x = NULL,
         y = NULL,
         title = "Paraná Trifásico",
         subtitle = "Obras Em Execução - 2021-01-28",
         caption = default_caption))


# 9. Mapa Em Projeto  ----

(myplot <- ggplot() +
   geom_sf(data = pr,
           mapping = aes(geometry=geometry),
           color = "lightgrey",
           size = 0.1
   ) +
   geom_jitter(data = (sum_obras_emprojeto %>% filter(!is.na(lat))),
               mapping = aes(x = lat, y = lon, 
                             size = us_prevista,
                             colour = regional,
                             fill = regional),
               width = 0.05, height = 0.05,
               alpha = .6) +
   #   geom_label_repel(data = concluidas,
   #                    aes(x = lat, y = lon,
   #                        label = id_da_obra),
   #                    box.padding = unit(0.6, "lines"),
   #                    size = 2,
   #                    max.overlaps = Inf) +
   theme_map() +
   labs(x = NULL,
        y = NULL,
        title = "Paraná Trifásico",
        subtitle = "Obras Em Projeto - 2021-01-28",
        caption = default_caption))



# 10. Benefícios --------------
# id_da_obra -> PowerBI
# descrição -> Power BI
# desc_tipo -> PowerBI
# regional -> PowerBI
# Km_entregues -> PowerBI
# alimentador -> alim_apd_obra
# município -> geo_copel
# UC_beneficiadas -> geo_copel
# UC_comerciais beneficiadas -> geo_copel
# CHI anterior -> qlic ???
# CHI posterior -> qlic ???
library(flextable)
concluidas <- concluidas %>% 
  select(regional,vpo, po, id_da_obra, descricao, 
         cidade.x, km_entregues, dt_fim_real,
         num_al_eletrico,
         desc_tipo_obra,
         qtde_uc_resid_hali,
         qtde_uc_comerc_hali,
         qtde_uc_indust_hali) %>% 
  arrange(regional, vpo, po, id_da_obra, km_entregues) %>% 
  rename(cidade = cidade.x)
ft <- flextable(concluidas)
ft <- theme_vanilla(ft)
ft <- add_footer_lines(ft, "Status 2021-01-18")
ft <- color(ft, part = "footer", color = "#666666")
ft <- set_caption(ft, caption = "Paraná Trifásico - Obras Entregues")
ft

