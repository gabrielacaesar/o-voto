# feito por Gabriela Caesar, Beatriz Milz e José de Jesus Filho 

buscar_dados_votacao <- function(data) {
  # url da API
  base <-
    "https://legis.senado.leg.br/dadosabertos/plenario/lista/votacao/"
  
  json <- jsonlite::fromJSON(paste0(base, data))
  
  dados <- json %>%
    purrr::pluck("ListaVotacoes") %>%
    purrr::pluck("Votacoes") %>%
    purrr::pluck("Votacao") %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()
  
  
  if (nrow(dados) > 0) {
    usethis::ui_done("Dados obtidos para o dia {data}")
  } else {
    usethis::ui_oops("Dados não obtidos para o dia {data}.")
  }
  
  dados
}


### 2019
data_inicial <- lubridate::ymd("20190201")
data_final <- lubridate::ymd("20191231")

all_dates <- seq(data_inicial, data_final, by = "day")
endpoint <- all_dates %>% stringr::str_remove_all("-")

dados_finais_2019 <- purrr::map(endpoint, buscar_dados_votacao) %>%
  purrr::compact() %>%
  purrr::map_dfr(~tidyr::unnest(.x,"Votos")) %>%
  tidyr::unnest("Votos", names_repair = "unique")

### 2020
data_inicial <- lubridate::ymd("20200201")
data_final <- lubridate::ymd("20201231")

all_dates <- seq(data_inicial, data_final, by = "day")
endpoint <- all_dates %>% stringr::str_remove_all("-")

dados_finais_2020 <- purrr::map(endpoint, buscar_dados_votacao) %>%
  purrr::compact() %>%
  purrr::map_dfr(~tidyr::unnest(.x,"Votos")) %>%
  tidyr::unnest("Votos", names_repair = "unique")

### 2021
data_inicial <- lubridate::ymd("20210201")
data_final <- lubridate::ymd("20211231")

all_dates <- seq(data_inicial, data_final, by = "day")
endpoint <- all_dates %>% stringr::str_remove_all("-")

dados_finais_2021 <- purrr::map(endpoint, buscar_dados_votacao) %>%
  purrr::compact() %>%
  purrr::map_dfr(~tidyr::unnest(.x,"Votos")) %>%
  tidyr::unnest("Votos", names_repair = "unique")

### 2019, 2020, 2021: selecionando e renomeando colunas

d_2019 <- dados_finais_2019 %>%
  select(DataSessao, 
         SiglaMateria, 
         NumeroMateria,
         contains("CodigoParlamentar"),
         contains("NomeParlamentar"),
         contains("SiglaPartido"),
         contains("SiglaUF"),
         contains("Voto"),
         CodigoSessao,
         NumeroSessao,
         CodigoTramitacao,
         CodigoSessaoVotacao,
         CodigoMateria) %>%
  select(-c(5, 7, 9, 11:14, 16:18)) %>%
  rename(CodigoParlamentar = `CodigoParlamentar...23`,
         NomeParlamentar = `NomeParlamentar...24`,
         SiglaPartido = `SiglaPartido...26`,
         SiglaUF = `SiglaUF...27`,
         Voto = `Voto...31`)

d_2020 <- dados_finais_2020 %>%
  select(DataSessao, 
         SiglaMateria, 
         NumeroMateria,
         contains("CodigoParlamentar"),
         contains("NomeParlamentar"),
         contains("SiglaPartido"),
         contains("SiglaUF"),
         contains("Voto"),
         contains("DescricaoVoto"),
         contains("DescricaoObjetivoProcesso"),
         CodigoSessao,
         NumeroSessao,
         CodigoTramitacao,
         CodigoSessaoVotacao,
         CodigoMateria) %>%
  select(-c(4, 6, 8, 10, 12, 13, 15:19)) %>%
  rename(CodigoParlamentar = `CodigoParlamentar...32`,
         NomeParlamentar = `NomeParlamentar...33`,
         SiglaPartido = `SiglaPartido...35`,
         SiglaUF = `SiglaUF...36`,
         Voto = `Voto...40`)

d_2021 <- dados_finais_2021 %>%
  select(DataSessao, 
         SiglaMateria, 
         NumeroMateria,
         contains("CodigoParlamentar"),
         contains("NomeParlamentar"),
         contains("SiglaPartido"),
         contains("SiglaUF"),
         contains("Voto"),
         contains("DescricaoVoto"),
         contains("DescricaoObjetivoProcesso"),
         CodigoSessao,
         NumeroSessao,
         CodigoSessaoVotacao,
         CodigoMateria) %>%
  select(-c(4, 6, 8, 10, 12, 13, 15:19)) %>%
  rename(CodigoParlamentar = `CodigoParlamentar...31`,
         NomeParlamentar = `NomeParlamentar...32`,
         SiglaPartido = `SiglaPartido...34`,
         SiglaUF = `SiglaUF...35`,
         Voto = `Voto...39`) %>%
  mutate(CodigoTramitacao = "NA") %>%
  relocate(CodigoTramitacao, .before = CodigoSessaoVotacao)

# juntando tudo
dados_finais <- d_2019 %>% bind_rows(d_2020, d_2021)

write.csv(dados_finais, "dados_finais.csv", row.names = F)
#write_rds(dados_finais, "dados_finais.rds")


## checando se alguma variavel é o ID da votação
dados_finais %>%
  count(CodigoMateria) %>%
  arrange(desc(n))

dados_finais %>%
  count(CodigoTramitacao) %>%
  arrange(desc(n))

dados_finais %>%
  count(NumeroMateria) %>%
  arrange(desc(n))
