# leitura dos pacotes
library(tidyverse)
library(rvest)

# URL dos senadores em exercício
url_1 <- "https://www25.senado.leg.br/web/senadores/em-exercicio/-/e/por-nome"

# URL da votação nominal
###### ATENÇÃO: INFORME ABAIXO O LINK DA VOTAÇÃO NOMINAL
url_2 <- "https://www.congressonacional.leg.br/materias/medidas-provisorias/-/mpv/149367/votacoes#votacao_6476"

# coleta dos partios dos senadores em exercício
senadores <- url_1 %>%
  read_html() %>%
  html_node("#senadoresemexercicio-tabela-senadores")%>%
  html_table(fill=T) %>%
  janitor::clean_names()%>%
  select(nome, partido, uf)%>%
  mutate(nome_upper = toupper(abjutils::rm_accent(nome)))%>%
  mutate(partido = case_when(partido == "PODEMOS" ~ "PODE",
                             TRUE ~ partido))

# coleta dos dados da votação, como nome e voto
votos_possiveis <- c("Não Compareceu", "Não",  "Sim ", "Abstenção", "Presidente (art.  RISF) ", "- art. , caput - Atividade parlamentar", "Abstenção")

votacao <- url_2 %>%
  read_html() %>%
  html_nodes("table.table.table-striped.table-condensed") %>%
  html_nodes("tbody>tr") %>%
  html_text() %>%
  as.data.frame() %>%
  rename(content = ".") %>%
  mutate(content = str_remove_all(content, '[0-9]')) %>% 
  mutate(voto = case_when(str_detect(content, "Sim") ~ "Sim",
                          str_detect(content, "Não Compareceu") ~ "Não Compareceu",
                          str_detect(content, "Não registrou voto") ~ "Não registrou voto",
                          str_detect(content, "Licença particular") ~ "Licença particular",
                          str_detect(content, "Licença saúde") ~ "Licença saúde",
                          str_detect(content, "Missão") ~ "Missão",
                          str_detect(content, "Não") ~ "Não",
                          str_detect(content, "Abstenção") ~ "Abstenção",
                          str_detect(content, "Obstrução") ~ "Obstrução",
                          str_detect(content, "Presidente") ~ "Presidente",
                          str_detect(content, "Atividade parlamentar") ~ "Atividade parlamentar",
                          str_detect(content, "Abstenção") ~ "Abstenção",
                          TRUE ~ "Nova votação")) %>%
  mutate(content = str_remove_all(content, paste(votos_possiveis, collapse = "|"))) %>%
  separate(content, c("nome_politico", "delete"), sep = "-") %>%
  select(-delete) %>%
  mutate(nome_politico = str_trim(nome_politico),
         voto = str_trim(voto),
         nome_upper = toupper(abjutils::rm_accent(nome_politico))) %>%
  filter(nome_politico != "") %>%
  mutate(id_politico = row_number(),
         id_votacao = case_when(id_politico %in% 1:81 ~ "V1", 
                                id_politico %in% 82:162 ~ "V2", 
                                id_politico %in% 163:243 ~ "V3", 
                                TRUE ~ "Na")) %>%
  mutate(voto = case_when(voto == "Sim" ~ "Sim",
                          voto %in% c("Missão", "Não Compareceu", "Não registrou voto", 
                                      "Licença particular", "Licença saúde", "Atividade parlamentar") ~ "Ausente",
                          voto == "Presidente" ~ "Não votou",
                          voto == "Não" ~ "Não",
                          voto == "Abstenção" ~ "Abstenção",
                          voto == "Obstrução" ~ "Obstrução")) %>%
  mutate(nome = case_when(nome == "Maria Eliza" ~ "Maria Eliza de Aguiar e Silva",
                                   TRUE ~ nome))

# cruzamento dos partidos atuais + dados dos votantes
###### ATENÇÃO: DEFINA ABAIXO QUAL VOTAÇÃO DO LINK VOCê DESEJA
###### OBS: V1 é a primeira. V2 é a segunda. ETC

votacao_escolhida <- "V1"

votacao_final <- votacao %>%
  left_join(senadores, by = "nome_upper") %>%
  filter(id_votacao == votacao_escolhida) %>%
  select(nome, partido, voto)

votacao_final %>% count(voto)

# download do CSV padronizado para o ADMIN
write.csv(votacao_final, paste0("votacao_final_SF_", Sys.Date() ,".csv"), row.names = F)
