#leitura das bibliotecas
library(tidyverse)
library(rvest)

## ATENÇÃO
##### INFORME ABAIXO A URL DA VOTAÇÃO NOMINAL
url <- "https://www.camara.leg.br/presenca-comissoes/votacao-portal?reuniao=62259&itemVotacao=9862"

# coleta dos dados do html
resultado_url <- url %>%
  read_html() %>%
  html_nodes(".titulares") %>%
  html_nodes("li") %>%
  html_text() %>%
  as.data.frame() %>%
  rename("info" = ".")

# tratamento dos dados
resultado_url_split <- resultado_url %>%
  separate(info, into = c("nome", "info"), sep = "\\(") %>%
  separate(info, into = c("partido", "uf", "voto"), sep = "-") %>%
  mutate(uf = str_remove_all(uf, " *\\)")) %>%
  mutate(nome = str_trim(nome),
         voto = str_trim(voto),
         voto = str_remove_all(voto, "votou"),
         voto = str_replace_na(voto, "ausente"),
         voto = str_replace_all(voto, "Sim", "sim"),
         voto = str_replace_all(voto, "Não", "nao"),
         voto = str_replace_all(voto, "Abstenção", "abstencao"),
         voto = str_replace_all(voto, "Obstrução", "obstrucao"),
         voto = str_replace_all(voto, "Presidente", "nao votou"),
         voto = str_replace_all(voto, "Art. 17", "nao votou"))

resultado_votacao <- resultado_url_split

#padronização dos nomes
resultado_votacao <- resultado_url_split %>%
  mutate(nome = str_replace_all(nome,
                                "Alencar S. Braga", "Alencar Santana Braga"), 
         nome = str_replace_all(nome,
                                "AlexandreSerfiotis", "Alexandre Serfiotis"),
         nome = str_replace_all(nome,
                                "Arthur O. Maia", "Arthur Oliveira Maia"),
         nome = str_replace_all(nome,
                                "Cap. Alberto Neto", "Capitão Alberto Neto"),
         nome = str_replace_all(nome,
                                "Carlos Gaguim", "Carlos Henrique Gaguim"),
         nome = str_replace_all(nome,
                                "Cezinha Madureira", "Cezinha de Madureira"),
         nome = str_replace_all(nome,
                                "Charlles Evangelis", "Charlles Evangelista"),
         nome = str_replace_all(nome,
                                "Chico D´Angelo", "Chico D'Angelo"),
         nome = str_replace_all(nome,
                                "Christiane Yared", "Christiane de Souza Yared"),
         nome = str_replace_all(nome,
                                "CoronelChrisóstom", "Coronel Chrisóstomo"),
         nome = str_replace_all(nome,
                                "Daniela Waguinho", "Daniela do Waguinho"),
         nome = str_replace_all(nome,
                                "Danrlei", "Danrlei de Deus Hinterholz"),
         nome = str_replace_all(nome,
                                "DelAntônioFurtado", "Delegado Antônio Furtado"),
         nome = str_replace_all(nome,
                                "Deleg. Éder Mauro", "Delegado Éder Mauro"),
         nome = str_replace_all(nome,
                                "Delegado Marcelo", "Delegado Marcelo Freitas"),
         nome = str_replace_all(nome,
                                "Dr Zacharias Calil", "Dr. Zacharias Calil"),
         nome = str_replace_all(nome,
                                "Dr. Sinval", "Dr. Sinval Malheiros"),
         nome = str_replace_all(nome,
                                "Dr.Luiz Antonio Jr", "Dr. Luiz Antonio Teixeira Jr."),
         nome = str_replace_all(nome,
                                "Dra.Soraya Manato", "Dra. Soraya Manato"),
         nome = str_replace_all(nome,
                                "EdmilsonRodrigues", "Edmilson Rodrigues"),
         nome = str_replace_all(nome,
                                "EduardoBolsonaro", "Eduardo Bolsonaro"),
         nome = str_replace_all(nome,
                                "Emanuel Pinheiro N", "Emanuel Pinheiro Neto"),
         nome = str_replace_all(nome,
                                "EuclydesPettersen", "Euclydes Pettersen"),
         nome = str_replace_all(nome,
                                "Evair de Melo", "Evair Vieira de Melo"),
         nome = str_replace_all(nome,
                                "FelipeFrancischini", "Felipe Francischini"),
         nome = str_replace_all(nome,
                                "Félix Mendonça Jr", "Félix Mendonça Júnior"),
         nome = str_replace_all(nome,
                                "FernandaMelchionna", "Fernanda Melchionna"),
         nome = str_replace_all(nome,
                                "Fernando Coelho", "Fernando Coelho Filho"),
         nome = str_replace_all(nome,
                                "FernandoMonteiro", "Fernando Monteiro"),
         nome = str_replace_all(nome,
                                "FernandoRodolfo", "Fernando Rodolfo"),
         nome = str_replace_all(nome,
                                "Frei Anastacio", "Frei Anastacio Ribeiro"),
         nome = str_replace_all(nome,
                                "GilbertoNasciment", "Gilberto Nascimento"),
         nome = str_replace_all(nome,
                                "Gildenemyr", "Pastor Gildenemyr"),
         nome = str_replace_all(nome,
                                "Hercílio Diniz", "Hercílio Coelho Diniz"),
         nome = str_replace_all(nome,
                                "HermesParcianello", "Hermes Parcianello"),
         nome = str_replace_all(nome,
                                "Isnaldo Bulhões Jr", "Isnaldo Bulhões Jr."),
         nome = str_replace_all(nome,
                                "Israel Batista", "Professor Israel Batista"),
         nome = str_replace_all(nome,
                                "João C. Bacelar", "João Carlos Bacelar"),
         nome = str_replace_all(nome,
                                "João Marcelo S.", "João Marcelo Souza"),
         nome = str_replace_all(nome,
                                "JoaquimPassarinho", "Joaquim Passarinho"),
         nome = str_replace_all(nome,
                                "José Airton", "José Airton Cirilo"),
         nome = str_replace_all(nome,
                                "Jose Mario Schrein", "Jose Mario Schreiner"),
         nome = str_replace_all(nome,
                                "Julio Cesar Ribeir", "Julio Cesar Ribeiro"),
         nome = str_replace_all(nome,
                                "Junio Amaral", "Cabo Junio Amaral"),
         nome = str_replace_all(nome,
                                "Lafayette Andrada", "Lafayette de Andrada"),
         nome = str_replace_all(nome,
                                "Leur Lomanto Jr.", "Leur Lomanto Júnior"),     
         nome = str_replace_all(nome,
                                "Luiz P. O.Bragança", "Luiz Philippe de Orleans e Bragança"),
         nome = str_replace_all(nome,
                                "LuizAntônioCorrêa", "Luiz Antônio Corrêa"),
         nome = str_replace_all(nome,
                                "Marcos A. Sampaio", "Marcos Aurélio Sampaio"),
         nome = str_replace_all(nome,
                                "MargaridaSalomão", "Margarida Salomão"),
         nome = str_replace_all(nome,
                                "MárioNegromonte Jr", "Mário Negromonte Jr."),
         nome = str_replace_all(nome,
                                "Maurício Dziedrick", "Maurício Dziedricki"),
         nome = str_replace_all(nome,
                                "Mauro Benevides Fº", "Mauro Benevides Filho"),
         nome = str_replace_all(nome,
                                "Nivaldo Albuquerq", "Nivaldo Albuquerque"),
         nome = str_replace_all(nome,
                                "Ottaci Nascimento", "Otaci Nascimento"),
         nome = str_replace_all(nome,
                                "Otto Alencar", "Otto Alencar Filho"),
         nome = str_replace_all(nome,
                                "Pastor Isidório", "Pastor Sargento Isidório"),
         nome = str_replace_all(nome,
                                "Paulo Martins", "Paulo Eduardo Martins"),
         nome = str_replace_all(nome,
                                "Paulo Pereira", "Paulo Pereira da Silva"),
         nome = str_replace_all(nome,
                                "Pedro A Bezerra", "Pedro Augusto Bezerra"),
         nome = str_replace_all(nome,
                                "Pedro Lucas Fernan", "Pedro Lucas Fernandes"),
         nome = str_replace_all(nome,
                                "Policial Sastre", "Policial Katia Sastre"),
         nome = str_replace_all(nome,
                                "Pr Marco Feliciano", "Pr. Marco Feliciano"),
         nome = str_replace_all(nome,
                                "Prof Marcivania", "Professora Marcivania"),
         nome = str_replace_all(nome,
                                "Profª Dorinha", "Professora Dorinha Seabra Rezende"),
         nome = str_replace_all(nome,
                                "Profª Rosa Neide", "Professora Rosa Neide"),
         nome = str_replace_all(nome,
                                "Professora Dayane", "Professora Dayane Pimentel"),
         nome = str_replace_all(nome,
                                "Rogério Peninha", "Rogério Peninha Mendonça"),
         nome = str_replace_all(nome,
                                "Roman", "Evandro Roman"),
         nome = str_replace_all(nome,
                                "SóstenesCavalcante", "Sóstenes Cavalcante"),
         nome = str_replace_all(nome,
                                "Stephanes Junior", "Reinhold Stephanes Junior"),
         nome = str_replace_all(nome,
                                "SubtenenteGonzaga", "Subtenente Gonzaga"),
         nome = str_replace_all(nome,
                                "ToninhoWandscheer", "Toninho Wandscheer"),
         nome = str_replace_all(nome,
                                "Vitor Hugo", "Major Vitor Hugo"),
         nome = str_replace_all(nome,
                                "Wellington", "Wellington Roberto"),
         nome = str_replace_all(nome,
                                "WladimirGarotinho", "Wladimir Garotinho"),
         nome = str_replace_all(nome,
                                "Cap. Fábio Abreu", "Capitão Fábio Abreu"),
         nome = str_replace_all(nome,
                                "JosimarMaranhãozi", "Josimar Maranhãozinho"),
         nome = str_replace_all(nome,
                                "Bozzella", "Júnior Bozzella"),
         nome = str_replace_all(nome,
                                "Tadeu  Filippelli", "Tadeu Filippelli"),
         nome = str_replace_all(nome,
                                "Glaustin da Fokus", "Glaustin Fokus"),
         nome = str_replace_all(nome,
                                "Pastor Gil", "Pastor Gildenemyr"),
         nome = str_replace_all(nome,
                                "Marcelo Álvaro", "Marcelo Álvaro Antônio"),
         nome = str_replace(nome,
                            "Pedro Augusto$", "Pedro Augusto Palareti"),
         nome = str_replace_all(nome,
                                "Pedro A Bezerra", "Pedro Augusto Bezerra"),
         nome = str_replace_all(nome,
                                "Paulo V. Caleffi", "Paulo Vicente Caleffi"),
         nome = str_replace_all(nome,
                                "Henrique Paraíso", "Henrique do Paraíso"))

#padronização dos partidos
resultado_votacao <- resultado_votacao %>%
  mutate(partido = str_replace_all(partido,
                                   "PODEMOS", "PODE"),
                  partido = str_replace_all(partido,
                                   "Republican", "Republicanos"),
         partido = str_replace_all(partido,
                                   "Podemos", "PODE"),
         partido = str_replace_all(partido,
                                   "Solidaried", "SD"),
         partido = str_replace_all(partido,
                                   "S.Part.", "S/Partido"))

# definindo colunas de interesse
dados_finais <- resultado_votacao %>%
  select(nome, partido, voto)

#download do arquivo padronizado para o admin
write.csv(dados_finais, paste0("dados_finais_", Sys.Date(), ".csv"), row.names = F)
