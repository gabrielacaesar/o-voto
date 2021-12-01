# carregando as bibliotecas
library(jsonlite)
library(tidyverse)

# definindo colunas para a coleta dos dados abaixo
column_votacao = c("sequencial", "CodigoParlamentar", "NomeParlamentar", "SiglaPartido", "SiglaUF", "Voto")
column_infos = c("SequencialSessao", "DataSessao", "DescricaoIdentificacaoMateria", "DescricaoVotacao", "Resultado", "Votos.VotoParlamentar")
column_df = c("DataSessao", "DescricaoIdentificacaoMateria", 'DescricaoVotacao', "NomeParlamentar", "SiglaPartido", "SiglaUF", "Voto")

# url da API
base <- "https://legis.senado.leg.br/dadosabertos/plenario/lista/votacao/"

### ATENÇÃO: INFORME ABAIXO A DATA DA VOTACAO. ANO MES E DIA
ano <- 2021
mes <- 11
dia <- 09
requisicao <- fromJSON(paste0(base, ano, mes, dia), flatten=TRUE)

### coletando dados das votacoes do dia
all_votacao <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]] %>% select(column_infos)
all_votacao$DescricaoVotacao

### ATENÇÃO: ESCOLHA A VOTACAO A SER COLETADA
i <- 1 # <--- aqui indica que voce deseja a primeira votacao. altere o numero caso isso nao seja verdade.
votacao_escolhida <- all_votacao[["Votos.VotoParlamentar"]][[i]] %>% mutate(sequencial = as.character(i)) %>% select(column_votacao)

### gerando dataframe final
dados_finais <- votacao_escolhida %>% 
  left_join(all_votacao, by = c("sequencial" = "SequencialSessao")) %>% 
  select(column_df) %>%
  rename(voto = Voto, nome_politico = NomeParlamentar, partido = SiglaPartido)

# corrigindo nomes de politicos, partidos e votos
dados_finais_v2 <- dados_finais %>%
  mutate(voto = case_when(voto == "Presidente (art. 51 RISF)" ~ "Não votou",
                          voto %in% c("MIS", "AP") ~ "Ausente",
                          TRUE ~ voto),
        nome_politico = case_when(nome_politico == "Maria Eliza" ~ "Maria Eliza de Aguiar e Silva",
                                  TRUE ~ nome_politico),
        partido = case_when(partido == "PODEMOS" ~ "PODE",
                            is.na(partido) ~ "API não informa partido",
                            TRUE ~ partido))

# selecionando colunas para o arquivo final
votacao_nova <- dados_finais_v2 %>% select(c(nome_politico, partido, voto))

# contagem de votos para checagem final
votacao_nova %>% count(voto)

# download de arquivo final
write.csv(votacao_nova, paste0("votacao_nova_sequencial_", i, ".csv"))
