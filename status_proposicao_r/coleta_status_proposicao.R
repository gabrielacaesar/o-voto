library(tidyverse)
library(rvest)

links_tramitacao <- read_csv("C:/Users/acaesar/Downloads/o-voto-status-tramitacao-monitoramento-proposicoes.csv")

## TESTE 1

sf_tramitacao <- links_tramitacao %>%
  select(1:4) %>%
  janitor::clean_names() %>%
  select(votacoes_senado, link_camara_4) %>%
  mutate(link_camara_4 = str_remove_all(link_camara_4, "-")) %>%
  filter(!is.na(.) & link_camara_4 != "")
 
get_content <- function(i){
  
a <- sf_tramitacao$link_camara_4[i] %>%
  read_html() %>%
  html_nodes("h3.inteiroTeor>span.nomeProposicao") %>%
  html_text() -> nome

b <- sf_tramitacao$link_camara_4[i] %>%
  read_html() %>%
  html_nodes("div#subSecaoSituacaoOrigemAcessoria>p>span") %>%
  html_text() -> status

tibble(a, b)

}

map_df(1:length(sf_tramitacao$link_camara_4), get_content)

### TESTE 2

h <- sf_tramitacao$link_camara_4 %>% map(read_html)

h %>% map_df(~{
  a <- html_nodes(., "h3.inteiroTeor>span.nomeProposicao") %>% html_text()
  b <- html_nodes(., "div#subSecaoSituacaoOrigemAcessoria>p>span") %>% html_text()
  tibble(a,b)
}) 


