# pr3f_portfolio.R ----
#
#  Vamos criar uma relação entre id_da_obra e código num_alim_eletrico
#  para podermos usar como base para o relatório de status.

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
library(gganimate)
library(gifski)
library(png)
library(readxl)

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
bi_file = "./data/20210113pr3f.xlsx"
pr3f <- read_bi_pr3f(bi_file)

# Km concluídos ------
sumario_obras <- function(pr3f) {
  geral <- pr3f %>%
    filter(estagio %in% c("CONC/ENCE", "CONC_BMD")) %>% 
    dplyr::select(km_concluido, dt_fim_real) %>% 
    na.omit() %>% 
    arrange(dt_fim_real) %>% 
    mutate(dt_fim_real = as.Date(dt_fim_real))
}
# Km concluídos por divisão ----------
sumario_divisao <- function(pr3f) {
  divisao <- pr3f %>%
    filter(estagio %in% c("CONC/ENCE", "CONC_BMD")) %>% 
    dplyr::select(vpo, km_concluido, dt_fim_real) %>% 
    na.omit() %>% 
    arrange(dt_fim_real) %>% 
    group_by(vpo) %>% 
    mutate(km_total = cumsum(km_concluido),
           dt_fim_real = as.Date(dt_fim_real))
}
geral <- sumario_obras(pr3f)
divisao  <- sumario_divisao(pr3f)

# Plot geral ------------
(pgeral <- geral %>% 
  ggplot(aes(x = dt_fim_real, 
             y = cumsum(km_concluido))) +
  geom_point(aes(group = seq_along(dt_fim_real)), 
               color = "orange",
               size = 2) +
  geom_line(alpha = .5) +
  labs(x = "", 
       y = "Km", 
       title = "Paraná Trifásico - Km entregues") +
  scale_x_date(date_labels = "%b/%y") +
  theme(axis.text.x = element_text(angle = 45)) +
   annotate("text", 
            label = "2255 Km entregues!", 
            x = as.Date("2019-04-01"), 
            y = 2100, 
            size = 6, 
            colour = "firebrick"))

# Plot por divisão -----------
(pdivisao <- divisao %>% 
  ggplot(aes(x = as.Date(dt_fim_real), 
             y = km_total)) +
  geom_point(aes(group = seq_along(as.Date(as.character(dt_fim_real)))),
             color = "orange",
             size = 1) +
  geom_line(alpha = .5) +
#  geom_text(aes(label = round(cumsum(km_concluido),0))) +
  labs(x = "", 
       y = "Km", 
       title = "Paraná Trifásico - Km entregues por divisão") +
   facet_wrap(~ vpo) +
  scale_x_date(date_labels = "%b/%y") +
  theme(axis.text.x = element_text(angle = 45)))

# Animate geral ------
pgeral <- pgeral + 
  geom_text(aes(label = formatC(cumsum(km_concluido), format="f", digits=2))) +
  labs(subtitle = "Status: {as.Date(frame_along)}") +
  theme(axis.text.x = element_blank()) +
  transition_reveal(dt_fim_real)
animate(pgeral, duration = 10, end_pause = 10)
anim_save("./pr3f.gif", pgeral)
# Animate facet divisao ------
pdivisao <- pdivisao + 
  geom_text(aes(label = formatC(km_total, format="f", digits=2))) +
  labs(subtitle = "Status: {as.Date(frame_along)}") +
  theme(axis.text.x = element_blank()) +
  transition_reveal(dt_fim_real)
animate(pdivisao, duration = 10, end_pause = 10)
anim_save("./pr3f_vpo.gif", pdivisao)
