
library(tidyverse)
library(rvest)
library(webdriver)

le_artigos <- function(endereco = "pesquisas-scopus/Scopus - 44 references cited by 1 selected document.html"){
  
  
  pagina <- read_html(endereco) 
  

  links <- pagina %>% 
    html_elements(
      css = ".ddmDocTitle"
    ) %>% 
    html_attr(
      name = "href"
    )
  
    
  titulos <- pagina %>% 
    html_elements(
      css = ".ddmDocTitle"
    ) %>% 
    html_text()
  

  tibble(
    title = titulos,
    link = links
  ) 
  
}

arquivos <- list.files(path = "pesquisas-scopus", pattern = "*.html", full.names = TRUE)

artigos <- arquivos %>% 
  map_df(
    .f = le_artigos
  ) %>% 
  distinct()





