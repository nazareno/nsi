theme_report <- function(base_size = 11,
                         strip_text_size = 12,
                         strip_text_margin = 5,
                         subtitle_size = 13,
                         subtitle_margin = 10,
                         plot_title_size = 16,
                         plot_title_margin = 10,
                         ...) {
    ret <- ggplot2::theme_minimal(base_family = "Roboto-Regular",
                                  base_size = base_size, ...)
    ret$strip.text <- ggplot2::element_text(hjust = 0, size=strip_text_size,
                                            margin=margin(b=strip_text_margin),
                                            family="Roboto-Bold")
    ret$plot.subtitle <- ggplot2::element_text(hjust = 0, size=subtitle_size,
                                               margin=margin(b=subtitle_margin),
                                               family="PT Sans")
    ret$plot.title <- ggplot2::element_text(hjust = 0, size = plot_title_size,
                                             margin=margin(b=plot_title_margin),
                                            family="Oswald")
    ret
}

import_data <- function(){
    library(tidyverse)
    library(magrittr)
    deputados = XML::xmlToDataFrame("http://www.camara.leg.br/sitcamaraws/deputados.asmx/ObterDeputados")
    deputados = deputados %>% 
        mutate(nome_simples = tolower(nomeParlamentar))
    anos = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
    
    props18 = read_csv2("https://dadosabertos.camara.leg.br/arquivos/proposicoes/proposicoes-2018.csv")
    props17 = read_csv2("https://dadosabertos.camara.leg.br/arquivos/proposicoes/proposicoes-2017.csv")
    props16 = read_csv2("https://dadosabertos.camara.leg.br/arquivos/proposicoes/proposicoes-2016.csv")
    props15 = read_csv2("https://dadosabertos.camara.leg.br/arquivos/proposicoes/proposicoes-2015.csv")
    props14 = read_csv2("https://dadosabertos.camara.leg.br/arquivos/proposicoes/proposicoes-2014.csv")
    
    
    autores18 = read_csv2("https://dadosabertos.camara.leg.br/arquivos/proposicoesAutores/proposicoesAutores-2018.csv")
    autores17 = read_csv2("https://dadosabertos.camara.leg.br/arquivos/proposicoesAutores/proposicoesAutores-2017.csv")
    autores16 = read_csv2("https://dadosabertos.camara.leg.br/arquivos/proposicoesAutores/proposicoesAutores-2016.csv")
    autores15 = read_csv2("https://dadosabertos.camara.leg.br/arquivos/proposicoesAutores/proposicoesAutores-2015.csv")
    autores14 = read_csv2("https://dadosabertos.camara.leg.br/arquivos/proposicoesAutores/proposicoesAutores-2014.csv")
    
    props = rbind(props18, props17)
    props = rbind(props, props16)
    props = rbind(props, props15)
    props = rbind(props, props14)
    
    autores = rbind(autores18, autores17)
    autores = rbind(autores, autores16)
    autores = rbind(autores, autores15)
    autores = rbind(autores, autores14)
    
    #props = read_csv2("https://dadosabertos.camara.leg.br/arquivos/proposicoes/proposicoes-2018.csv")
    #autores = read_csv2("https://dadosabertos.camara.leg.br/arquivos/proposicoesAutores/proposicoesAutores-2018.csv")
    
    completo = props %>% 
        left_join(autores, 
                  by = c("id" = "idProposicao")) %>% 
        filter(codTipoAutor == "Deputado", 
               siglaTipo %in% c("PEC", "PLC", "PL", "PFC", "SIT", "SBT", "EMO", "PLN")) %>% 
        select(id, siglaTipo, ano, keywords, idAutor, nomeAutor) %>% 
        mutate(nome_simples = tolower(nomeAutor)) %>% 
        left_join(deputados, 
                  by = c("nome_simples"))
    
    final = completo %>% 
        select(keywords, siglaTipo, sexo) %>% 
        separate_rows(keywords, sep = ",") 
    final %>% 
        write_csv("data/palavras-chave.csv")
}

read_projectdata <- function(){
    read_csv(here::here("data/palavras-chave.csv"))
}
