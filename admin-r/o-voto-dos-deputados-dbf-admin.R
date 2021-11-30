#install.packages("tidyverse")
#install.packages("foreign")

library(tidyverse)
library(foreign)

#setwd("~/Downloads/")
setwd("C:/Users/acaesar/Downloads/")
dbf_file <- read.dbf("CD210710.dbf")

corrigir_uf <- function(dbf_file){
  dbf_file %>%
    janitor::clean_names() %>%
    mutate(estado = case_when(estado == "RORAIMA" ~ "RR",
                              estado == "RONDONIA" ~ "RO",
                              estado == "ACRE" ~ "AC",
                              estado == "AMAZONAS" ~ "AM",
                              estado == "PARA" ~ "PA",
                              estado == "AMAPA" ~ "AP",
                              estado == "TOCANTINS" ~ "TO",
                              estado == "MATO GROSSO" ~ "MT",
                              estado == "MATO GROSSO DO SUL" ~ "MS",
                              estado == "MARANHAO" ~ "MA",
                              estado == "CEARA" ~ "CE",
                              estado == "PIAUI" ~ "PI",
                              estado == "RIO GRANDE DO NORTE" ~ "RN",
                              estado == "PARAIBA" ~ "PB",
                              estado == "ALAGOAS" ~ "AL",
                              estado == "PERNAMBUCO" ~ "PE",
                              estado == "SERGIPE" ~ "SE",
                              estado == "BAHIA" ~ "BA",
                              estado == "MINAS GERAIS" ~ "MG",
                              estado == "ESPIRITO SANTO" ~ "ES",
                              estado == "RIO DE JANEIRO" ~ "RJ",
                              estado == "SAO PAULO" ~ "SP",
                              estado == "DISTRITO FEDERAL" ~ "DF",
                              estado == "GOIAS" ~ "GO",
                              estado == "PARANA" ~ "PR",
                              estado == "SANTA CATARINA" ~ "SC",
                              estado == "RIO GRANDE DO SUL" ~ "RS"))
}

corrigir_partidos <- function(dbf_file){
  dbf_file %>%
    janitor::clean_names() %>%
    mutate(partido = case_when(partido == "Solidaried" ~ "SOLIDARIEDADE",
                               partido == "Republican" ~ "Republicanos",
                               partido == "PCdoB" ~ "PC do B",
                               partido == "S.Part." ~ "S/Partido",
                               partido == "Podemos" ~ "PODE",
                               TRUE ~ as.character(partido)))
}

corrigir_nomes <- function(dbf_file){
  dbf_file %>%
    janitor::clean_names() %>%
    mutate(nome_par = case_when(nome_par == "PROFESSORA DORINHA SEABRA REZEN" ~ "PROFESSORA DORINHA SEABRA REZENDE",
                                nome_par == "CHICO D`ANGELO" ~ "CHICO D'ANGELO",
                                nome_par == "LUIZ PHILIPPE DE ORLEANS E BRAG" ~ "LUIZ PHILIPPE DE ORLEANS E BRAGANCA",
                                nome_par == "OTTACI NASCIMENTO" ~ "OTACI NASCIMENTO",
                                nome_par == "PASTOR GIL" ~ "PASTOR GILDENEMYR",
                                nome_par == "JOSE AIRTON FELIX CIRILO" ~ "JOSE AIRTON CIRILO",
                                nome_par == "JUNIO AMARAL" ~ "CABO JUNIO AMARAL",
                                nome_par == "BOZZELLA" ~ "JUNIOR BOZZELLA",
                                nome_par == "GLAUSTIN DA FOKUS" ~ "GLAUSTIN FOKUS",
                                nome_par == "VITOR HUGO" ~ "MAJOR VITOR HUGO",
                                nome_par == "ROMAN" ~ "EVANDRO ROMAN",
                                nome_par == "RUBENS PEREIRA JUNIOR" ~ "RUBENS PEREIRA JR",
                                nome_par == "CHICO D\xefANGELO" ~ "CHICO D'ANGELO",
                                nome_par == "JOAO PAULO KLEIN\x9aBING" ~ "JOAO PAULO KLEINUBING",
                                nome_par == "JOZI  ARAUJO" ~ "JOZI ARAUJO",
                                nome_par == "MARIO NEGROMONTE JR" ~ "MARIO NEGROMONTE JR.",
                                nome_par == "PEDRO DALUA" ~ "DALUA DO ROTA",
                                TRUE ~ as.character(nome_par)))
}

corrigir_votos <- function(dbf_file){
  dbf_file %>%
    janitor::clean_names()%>%
    mutate(voto = case_when(voto == "ART. 17" ~ "Não votou",
                            voto == "<------->" ~ "Ausente",
                            voto == "NAO" ~ "Não",
                            voto == "SIM" ~ "Sim",
                            voto == "ABSTENCAO" ~ "Abstenção",
                            voto == "OBSTRUCAO" ~ "Obstrução",
                            TRUE ~ as.character(voto)))
}

votacao_nova_admin <- dbf_file %>%
  corrigir_nomes() %>%
  corrigir_uf() %>%
  corrigir_partidos() %>%
  corrigir_votos() %>%
  select(nome_par, partido, voto) %>%
  arrange(nome_par)

votacao_nova_admin %>%
  count(voto)

write.csv(votacao_nova_admin, "votacao_nova_admin.csv", row.names = F)
