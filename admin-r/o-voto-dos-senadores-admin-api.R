# carregando as bibliotecas
library(jsonlite)
library(tidyverse)

base <- "https://legis.senado.leg.br/dadosabertos/plenario/lista/votacao/"
### ATENÇÃO: INFORME ABAIXO A DATA DA VOTACAO. ANO MES E DIA
data <- "20211027"
requisicao <- fromJSON(paste0(base, data), flatten=TRUE)

### criando DF com info de votacoes do dia
data <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]][["DataSessao"]]
proposicao <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]][["DescricaoIdentificacaoMateria"]]
casa <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]][["SiglaCasa"]]
resumo <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]][["DescricaoVotacao"]]
sequencial <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]][["SequencialSessao"]]

df_info <- data.frame(data, casa, sequencial, proposicao, resumo)
### ATENÇÃO: rode o codigo abaixo e verifique no console se esta é a votacao que voce deseja
df_info

### coletando votos de cada deputado
i = 1
id_politico <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]][["Votos"]][[i]][["CodigoParlamentar"]]
nome_politico <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]][["Votos"]][[i]][["NomeParlamentar"]]
partido <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]][["Votos"]][[i]][["SiglaPartido"]]
uf <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]][["Votos"]][[i]][["SiglaUF"]]
voto <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]][["Votos"]][[i]][["Voto"]]

df_votacao <- data.frame(id_politico, nome_politico, partido, uf, voto)
dados_finais <- info %>% bind_cols(df_votacao)

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
write.csv(votacao_nova, paste0("dados_finais_", dados_finais$data[1], "_sequencial_", dados_finais$sequencial[1], ".csv"))
