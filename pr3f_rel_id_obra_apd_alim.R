# rel_id_obra_apd_alim.R
#
#  Vamos criar uma relação entre id_da_obra e código num_alim_eletrico
#  para podermos usar como base para o relatório de status.

# 1. Vamos limpar todos os pacotes instalados ---------------------
#    E instalar os que vamos utilziar

### Limpar pacotes carregados
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

# Vamos instalar os pacotes que serão utilizados
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

# ================================================
#

# 2. A planilha de Trifaseamento pode ser lida no script pr3f_linha_de_base.R
tri

# 3. Vamos ler os dados do PowerBI (origem dos dados SAP-PS) ---------------
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
                       projeto, estagio)
}
# Opcionalmente pode-se ler o arquivo formato Obras
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
# Vamos ler os dados do PowerBI (origem dos dados SAP-PS) ---------------
bi_file = "./data/20210113pr3f.xlsx"
pr3f <- read_bi_pr3f(bi_file)
obras_file = "./data/2020-12-18 bi Obras.xlsx"
pr3f <- read_bi_obras(obras_file)

# 4. Vamos sumarizar para 1 linha por obra --------------------------
sumario_obras <- function(pr3f) {
  pr3f <- pr3f %>%           # categoriza o estagio para poder ordenar
    mutate(estagio = factor(estagio,
                          levels = c("ORC/ABER",
                                     "OCS/LIB",
                                     "AMP",
                                     "AES",
                                     "CONC_BMD",
                                     "CONC/ENCE",
                                     "PARALISADO",
                                     "CANCELADO")))
  pr3f <- pr3f %>%                     # Dados sumarizados 1 linha por obra
    group_by(id_da_obra) %>% 
    summarize(vpo = first(vpo),
              po = first(po),
              situacao = first(situacao),
              descricao = first(descricao),
              n_prj = n())
}
pr3f <- sumario_obras(pr3f)

# 5. a partir do id_da_obra, extrai informações da obra ------------------
separa_info_id_obra <- function(pr3f) {
  pr3f <- pr3f %>% separate(id_da_obra,
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
  
  pr3f <- pr3f %>% left_join(prefixo_categoria, by = c("tipo_obra" = "tipo_obra"))
  pr3f <- pr3f %>% left_join(prefixo_regional, by = c("cod_vpo" = "cod_vpo"))
}
pr3f <- separa_info_id_obra(pr3f)

# 6 Extrai alimentador da descrição --------------------------
extrai_alim_da_descricao <- function(pr3f) {
  pr3f <- pr3f %>%
    mutate(alimentador = str_extract(
      str_to_upper(iconv(
        descricao,
        from = 'UTF-8',
        to = 'ASCII//TRANSLIT'
      )),
      "(AL|ALIM)(S.\\s|.\\s|\\s|_|/|/.)([A-Z\\s]*)"
    )) %>%
    mutate(alimentador = str_replace(alimentador,
                                     "(AL|ALIM)(S.\\s|.\\s|\\s|_|/|/.)",
                                     ""))
}
pr3f <- extrai_alim_da_descricao(pr3f)

# 7. O APD está na descrição da Obra. Vamos extrair para coluna própria -------------
get_apd <- function(bi) {
  bi <- bi %>% mutate(
    apd = str_extract(descricao, "(APD|apd)(_|| )[0-9]{6}"),
    apd = str_extract(apd, "[0-9]{6}"),
    apd2 = str_extract(descricao, "[0-9]_[0-9]{6}"),
    apd2 = str_extract(apd2, "[0-9]{6}"),
    apd = if_else(is.na(apd), "", apd),
    apd2 = if_else(is.na(apd2), "", apd2)
  ) %>%
  dplyr::select(!apd2)
}
pr3f <- get_apd(pr3f)


# Vamos salvar a relação alimentador x id_da_obra x apd para correções manuais
from_pr3f <- pr3f %>% dplyr::select(id_da_obra, 
                                    descricao, 
                                    alimentador, 
                                    apd)
from_pr3f <- from_pr3f %>% mutate(apd = as.character(apd))

from_tri <- tri %>% dplyr::select(num_al_eletrico, 
                                  nome_alim, 
                                  apd)
from_tri <- from_tri %>% mutate(apd = as.character(apd))
            
rel_alim_apd_obra <- from_tri %>% full_join(from_pr3f,
                                            by = c("apd" = "apd"))

write_csv2(rel_alim_apd_obra, file = "./data/alim_apd_obra.csv")

