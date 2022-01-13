library(readxl)
library(tidyverse)
library(rvest)

projetos <- read_xlsx("C:/Users/acaesar/Downloads/o-voto-status-tramitacao-monitoramento.xlsx")

#### SENADO - CD
cd_projetos <- projetos %>%
  janitor::clean_names() %>%
  select(1, 2, 4) %>%
  filter(link_camara_4 != "-") 

get_status <- function(i){
  cd_projetos$link_camara_4[i] %>%
    read_html(options = "HUGE") %>%
    html_nodes("div#subSecaoSituacaoOrigemAcessoria>p>span") %>%
    html_text() %>%
    as.data.frame() %>%
    rename(status = ".") %>%
    mutate(status = str_trim(status),
           link = cd_projetos$link_camara_4[i])
}

all_status <- map_dfr(1:length(cd_projetos$link_camara_4), get_status)

cd_final <- all_status %>%
  left_join(cd_projetos, by = c("link" = "link_camara_4"))


#### CÂMARA - CD
cd_projetos_2 <- projetos %>%
  janitor::clean_names() %>%
  select(8, 9, 11) %>%
  filter(link_camara_11 != "-") 

get_status <- function(i){
  cd_projetos_2$link_camara_11[i] %>%
    read_html(options = "HUGE") %>%
    html_nodes("div#subSecaoSituacaoOrigemAcessoria>p>span") %>%
    html_text() %>%
    as.data.frame() %>%
    rename(status = ".") %>%
    mutate(status = str_trim(status),
           link = cd_projetos_2$link_camara_11[i])
}

all_status <- map_dfr(1:length(cd_projetos_2$link_camara_11), get_status)

cd_final <- all_status %>%
  left_join(cd_projetos_2, by = c("link" = "link_camara_11"))


#### SENADO - SF

sf_projetos <- projetos %>%
  janitor::clean_names() %>%
  select(1:3) %>%
  filter(link_senado_3 != "-" &
           !str_detect(link_senado_3, "congressonacional")) 

get_status <- function(i){
  sf_projetos$link_senado_3[i] %>%
    read_html(options = "HUGE") %>%
    html_nodes("dl.dl-horizontal.sf-atividade-dl-dd-spacer") %>%
    html_text() %>%
    as.data.frame() %>%
    rename(status = ".") %>%
    mutate(status = str_remove_all(status, ".*(Último estado:)"),
           status = str_trim(status),
           link = sf_projetos$link_senado_3[i])
}

all_status <- map_dfr(1:length(sf_projetos$link_senado_3), get_status)


sf_final <- all_status %>%
  left_join(sf_projetos, by = c("link" = "link_senado_3"))


#### CÂMARA - SF

sf_projetos_2 <- projetos %>%
  janitor::clean_names() %>%
  select(8:10) %>%
  filter(link_senado_10 != "-" &
           !str_detect(link_senado_10, "congressonacional")) 

get_status <- function(i){
  sf_projetos_2$link_senado_10[i] %>%
    read_html(options = "HUGE") %>%
    html_nodes("dl.dl-horizontal.sf-atividade-dl-dd-spacer") %>%
    html_text() %>%
    as.data.frame() %>%
    rename(status = ".") %>%
    mutate(status = str_remove_all(status, ".*(Último estado:)"),
           status = str_trim(status),
           link = sf_projetos_2$link_senado_10[i])
}

all_status <- map_dfr(1:length(sf_projetos_2$link_senado_10), get_status)


sf_final <- all_status %>%
  left_join(sf_projetos_2, by = c("link" = "link_senado_10"))

