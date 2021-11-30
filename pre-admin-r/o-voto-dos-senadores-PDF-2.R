#### NAO CONCLUIDO
#### EM DESENVOLVIMENTO

# leitura de pacotes
library(tidyverse)
library(pdftools)
library(rvest)

## coleta de senadores em exercicio
url <- "https://www25.senado.leg.br/web/senadores/em-exercicio/-/e/por-nome"

senadores_exercicio <- url %>%
  read_html() %>%
  html_table() %>%
  as.data.frame() %>%
  janitor::clean_names() %>%
  select(nome, partido, uf) %>%
  mutate(nome_upper = abjutils::rm_accent(toupper(nome)))

####################################

# scrape tables from pdf
# https://rl.senado.gov.br/reports/rwservlet?legis&report=/forms/parlam/vono_r01.RDF&paramform=no&p_cod_materia_i=148543&p_cod_materia_f=148543&p_cod_sessao_votacao_i=6406&p_cod_sessao_votacao_f=6406&p_order_by=nom_parlamentar



