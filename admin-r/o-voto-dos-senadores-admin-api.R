library(jsonlite)
library(tidyverse)

base <- "https://legis.senado.leg.br/dadosabertos/plenario/lista/votacao/"
data <- "20130516"

requisicao <- fromJSON(paste0(base, data), flatten=TRUE)

### criando DF com info de votacoes do dia
data <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]][["DataSessao"]]
proposicao <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]][["DescricaoIdentificacaoMateria"]]
casa <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]][["SiglaCasa"]]
resumo <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]][["DescricaoVotacao"]]
sequencial <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]][["SequencialSessao"]]

df_info <- data.frame(data, casa, sequencial, proposicao, resumo)

### coletando votacao do sequencial informado
i = 1
id_politico <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]][["Votos.VotoParlamentar"]][[i]][["CodigoParlamentar"]]
nome_politico <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]][["Votos.VotoParlamentar"]][[i]][["NomeParlamentar"]]
partido <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]][["Votos.VotoParlamentar"]][[i]][["SiglaPartido"]]
uf <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]][["Votos.VotoParlamentar"]][[i]][["SiglaUF"]]
voto <- requisicao[["ListaVotacoes"]][["Votacoes"]][["Votacao"]][["Votos.VotoParlamentar"]][[i]][["Voto"]]
info <- df_info %>% filter(sequencial == i) 

df_votacao <- data.frame(id_politico, nome_politico, partido, uf, voto)
dados_finais <- info %>% bind_cols(df_votacao)

write.csv(dados_finais, paste0("dados_finais_", dados_finais$data[1], "_sequencial_", dados_finais$sequencial[1], ".csv"))
