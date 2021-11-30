library(tidyverse)
library(rvest)

url <- "https://www25.senado.leg.br/web/senadores/em-exercicio/-/e/por-nome"

senadores_hj <- url %>%
  read_html() %>%
  html_node("#senadoresemexercicio-tabela-senadores")%>%
  html_table(fill=T) %>%
  janitor::clean_names()%>%
  select(nome, partido, uf)%>%
  mutate(nome_upper = toupper(abjutils::rm_accent(nome)))%>%
  mutate(partido = case_when(partido == "PODEMOS" ~ "PODE",
                             TRUE ~ partido))
