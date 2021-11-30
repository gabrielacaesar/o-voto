################################################################
###                                                          ###
###                    O voto do Congresso                   ###
###                                                          ###
###                      gabriela caesar                     ###
###                                                          ###
################################################################

################################################################
###                 Inclusão de votação nova                 ###
################################################################

################################################################
###                     Votação virtual                      ###
################################################################

library(tidyverse)
library(rvest)

### SENADO FEDERAL

sf_url <- "https://www.congressonacional.leg.br/materias/vetos/-/veto/detalhe/14585/1" 

sf_votacao <- sf_url %>%
  read_html() %>%
  html_node(xpath = "//*[@id='votacao_SF']/div/table") %>%
  html_table(fill = T)%>%
  as.data.frame() %>%
  janitor::clean_names() %>%
  mutate(nome_upper = toupper(abjutils::rm_accent(parlamentar)),
         partido = toupper(abjutils::rm_accent(partido))) %>%
  mutate(partido = case_when(partido == "PROGRES" ~ "PP",
                             partido == "PODEMOS" ~ "PODE",
                             partido == "REPUBLICA" ~ "REPUBLICANOS",
                             TRUE ~ partido))


url <- "https://www25.senado.leg.br/web/senadores/em-exercicio/-/e/por-nome"
# verificar se alguem saiu do mandato 
# fazer isso apenas no dia da votacao
# calcula ausentes por exclusao 

sf_exercicio <- url %>%
  read_html() %>%
  html_node("#senadoresemexercicio-tabela-senadores")%>%
  html_table(fill=T) %>%
  janitor::clean_names()%>%
  select(nome, partido, uf) %>%
  mutate(nome = case_when(nome == "Fernando Bezerra Coelho" ~ "Fernando Coelho",
                                 TRUE ~ nome)) %>%
  mutate(nome_upper = toupper(abjutils::rm_accent(nome)))%>%
  mutate(partido = case_when(partido == "PROGRES" ~ "PP",
                             partido == "PODEMOS" ~ "PODE",
                             partido == "REPUBLICA" ~ "REPUBLICANOS",
                             TRUE ~ partido))
  

sf_resultado <- sf_exercicio %>%
  left_join(sf_votacao, by = "nome_upper") %>%
  mutate(`partido.y` = case_when(is.na(`partido.y`) ~ `partido.x`,
                                 TRUE ~ `partido.y`)) %>%
  mutate(check_partido = `partido.x` == `partido.y`) %>%
  select(parlamentar = nome_upper, partido = `partido.y`, voto) %>%
  mutate(voto = case_when(is.na(voto) ~ "Ausente",
                          TRUE ~ voto)) 

# verificar se bate com placar do site
sf_resultado %>%
  count(voto)

write.csv(sf_resultado, "sf_resultado.csv")

### CAMARA DOS DEPUTADOS

cd_url <- "https://www.congressonacional.leg.br/materias/vetos/-/veto/detalhe/14605/0"

cd_votacao <- cd_url %>%
  read_html() %>%
  html_node(xpath = "//*[@id='votacao_CD']/div/table") %>%
  html_table(fill = T)%>%
  as.data.frame() %>%
  janitor::clean_names()

# FALTAM DEPUTADOS AUSENTES /// 
