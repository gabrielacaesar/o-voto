library(rvest)
library(tidyverse)
library(googledrive)
library(readxl)

setwd("~/Documents")
dir.create(paste0("votacao_cd_", Sys.Date()))
setwd(paste0("votacao_cd_", Sys.Date()))

setwd("~/Documents")
dir.create(paste0("votacao_sf_", Sys.Date()))
setwd(paste0("votacao_sf_", Sys.Date()))

drive_auth(email = "gabriela.caesar.g1@gmail.com")
## cd - abaixo
# drive_download(file = as_id("1Rxi_INy51j9BXCHRc4B-H13QfKegICtUtshd_rGULag"), type = "xlsx")
## sf - abaixo
drive_download(file = as_id("1D57BJfGXxwizxK1yD1qbzx8CH6v7q8jSltK9puaw2_Q"), type = "xlsx")
arquivo <- list.files()
proposicoes <- read_xlsx(arquivo, sheet = 'projetos')
votos <- read_xlsx(arquivo, sheet = 'votos')

# base_url <- "https://interativos.g1.globo.com/politica/2019/como-votam/camara-dos-deputados/brasil/projetos/"
# sf
base_url <- "https://interativos.g1.globo.com/politica/2019/como-votam/senado/brasil/projetos/"

url_proposicoes <- proposicoes %>% 
  mutate(link = paste0(base_url, permalink)) %>%
  select(link) %>%
  filter(!str_detect(link, "perdao-a-governantes-que-nao-investirem-o-minimo-em-educacao-na-pandemia"))

########################################################
#################### VOTINGS RESULT ####################
########################################################

get_votacoes <- function(i){
  link <- url_proposicoes$link[i]

  nome_proposicao <- link %>%
    read_html() %>%
    html_nodes("h3.ProjectCard_title__1wX4b") %>%
    html_text() %>%
    as.data.frame() %>%
    rename(nome_proposicao = ".")

  votos_proposicao <- link %>%
    read_html() %>%
    html_nodes("div.Accordion_container__3K-ma") %>%
    html_text() %>%
    as.data.frame() %>%
    rename(content = ".") %>%
    mutate(numero = str_sub(content, start = 1, end = 3),
           numero = str_remove_all(numero, "\\|"),
           tipo_voto = c("Sim", "Não", "Não votou", "Abstenção", "Obstrução", "Ausente"),
           nome_proposicao = nome_proposicao,
           link = link) %>%
    select(nome_proposicao, tipo_voto, numero, link)
}

dados_votacoes <- map_dfr(1:length(url_proposicoes$link), get_votacoes)


## xlsx vs. html / checagem

sf_original <- votos %>%
  count(voto, permalink) %>%
  filter(!str_detect(permalink, "perdao-a-governantes-que-nao-investirem-o-minimo-em-educacao-na-pandemia")) %>%
  left_join(proposicoes, by = "permalink") %>%
  select(vulgar, voto, n, permalink) %>%
  mutate(voto = case_when(voto == "sim" ~ "Sim",
                          voto == "nao" ~ "Não",
                          voto == "naovotou" ~ "Não votou",
                          voto == "ausente" ~ "Ausente",
                          voto == "obstrucao" ~ "Obstrução",
                          voto == "abstencao" ~ "Abstenção")) %>%
  mutate(link = paste0("https://interativos.g1.globo.com/politica/2019/como-votam/senado/brasil/projetos/", permalink)) %>%
  left_join(dados_votacoes, by = c("link", "voto" = "tipo_voto")) %>%
  relocate(n, .before = numero) %>%
  mutate(check_n =  case_when(as.integer(n) == as.integer(numero) ~ "ok", TRUE ~ "not_ok"))
  

########################################################
#################### STATUS WEBPAGE ####################
########################################################

#### ORIGINAL CODE BELOW 
# https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r

#' @param x a single URL
#' @param non_2xx_return_value what to do if the site exists but the
#'        HTTP status code is not in the `2xx` range. Default is to return `FALSE`.
#' @param quiet if not `FALSE`, then every time the `non_2xx_return_value` condition
#'        arises a warning message will be displayed. Default is `FALSE`.
#' @param ... other params (`timeout()` would be a good one) passed directly
#'        to `httr::HEAD()` and/or `httr::GET()`
url_exists <- function(x, non_2xx_return_value = FALSE, quiet = FALSE,...) {
  
  suppressPackageStartupMessages({
    require("httr", quietly = FALSE, warn.conflicts = FALSE)
  })
  
  # you don't need thse two functions if you're alread using `purrr`
  # but `purrr` is a heavyweight compiled pacakge that introduces
  # many other "tidyverse" dependencies and this doesnt.
  
  capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
    tryCatch(
      list(result = code, error = NULL),
      error = function(e) {
        if (!quiet)
          message("Error: ", e$message)
        
        list(result = otherwise, error = e)
      },
      interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
      }
    )
  }
  
  safely <- function(.f, otherwise = NULL, quiet = TRUE) {
    function(...) capture_error(.f(...), otherwise, quiet)
  }
  
  sHEAD <- safely(httr::HEAD)
  sGET <- safely(httr::GET)
  
  # Try HEAD first since it's lightweight
  res <- sHEAD(x, ...)
  
  if (is.null(res$result) || 
      ((httr::status_code(res$result) %/% 200) != 1)) {
    
    res <- sGET(x, ...)
    
    if (is.null(res$result)) return(NA) # or whatever you want to return on "hard" errors
    
    if (((httr::status_code(res$result) %/% 200) != 1)) {
      if (!quiet) warning(sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range", x))
      return(non_2xx_return_value)
    }
    
    return(TRUE)
    
  } else {
    return(TRUE)
  }
  
}

urls <- url_proposicoes$link %>% as.vector()

checagem_urls <- data.frame(
  exists = sapply(urls, url_exists, USE.NAMES = FALSE),
  urls,
  stringsAsFactors = FALSE) %>% 
  dplyr::tbl_df()

checagem_urls_F <- checagem_urls %>% 
  filter(exists == FALSE) 
