
library(tidyverse)
library(rvest)
library(webdriver)
library(googlesheets4)


le_artigos <- function(endereco = "pesquisas/_game theory_ _software engineering_ - Google Scholar.html"){


  pagina <- read_html(endereco) %>% 
    html_elements(css = ".gs_rt")
  
  links <- pagina %>% 
    html_elements(
      css = "a"
    ) %>% 
    html_attr(
      name = "href"
    )
  
  
  titulos <- pagina %>% 
    html_elements(
      css = "a"
    ) %>% 
    html_text() %>% 
    enframe(
      value = "titulo"  
    )
  
  
  nomes_autores <- read_html(endereco) %>% 
    html_elements(css = ".gs_a") %>% 
    html_elements(css = "a") %>% 
    html_text()
  
  anos <- read_html(endereco) %>% 
    html_elements(css = ".gs_a") %>% 
    html_text() %>% 
    str_extract(pattern = "[0-9]{4}" ) 
  

  
  autores <- read_html(endereco) %>% 
    html_elements(css = ".gs_a") %>% 
    map_int(
      .f = ~{
        html_elements(.x, css = "a") %>% 
          length()
      }
    ) %>% 
    enframe() %>% 
    filter(value != 0) %>% 
    mutate(
      autor = map(.x = value, .f = ~{1:.x})
    ) %>% 
    unnest(autor) %>% 
    select(
      id_artigo = name,
      id_autor = autor
    ) %>% 
    mutate(
      nome_autor = nomes_autores
    ) %>% 
    group_by(
      id_artigo 
    ) %>% 
    summarise(
      nome_autor = str_flatten(nome_autor, collapse = ", ")
    ) 
  

  tudo <- titulos %>%  
    left_join(
      autores,
      by = c("name" = "id_artigo")
    ) %>% 
    mutate(
      link = links,
      ano = anos
    )
 
  
  
  tudo 
  
}

arquivos <- list.files(path = "pesquisas", pattern = "*.html", full.names = TRUE)


artigos <- arquivos %>% 
  map_df(
    .f = le_artigos
  ) %>% 
  select(-name) %>% 
  distinct()


# arquivo_antigos <- "https://docs.google.com/spreadsheets/d/18pnMrFKuuu8WYCyD1xkWUhVx9sIN8VHlSQOS-Hz6X_8"
# 
# antigos <- googlesheets4::read_sheet(ss = arquivo_antigos, sheet = "post_Carlos" )








