#Function download_tratamento (Tratamento de água)

download_tratamento <- function(periodo,regiao=NULL,unidade_federativa=NULL){

  #trazer aqui avisos para periodo, regiao e unidade federativa

  list_uf <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG",
               "MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR",
               "RS","SC","SE","SP","TO")

  list_regiao <- c("NORTE","NORDESTE","SUL","CENTRO-OESTE","SUDESTE")

  if(is.null(regiao)==TRUE){
    regiao <- list_regiao
  }

  if(is.null(unidade_federativa)==TRUE){
    unidade_federativa <-  list_uf
  }

  #Download and unzip
  temp <- tempfile(fileext = ".zip")
  utils::download.file("https://sage.saude.gov.br/dados/sisagua/cadastro_tratamento_de_agua.zip", temp)
  datazip <- utils::unzip(temp)


  #Load data and filter
  trat <- data.table::fread(datazip, sep=";") %>%
    janitor::clean_names() %>%
    dplyr::filter(ano_de_referencia %in% periodo) %>%
    dplyr::filter(regiao_geografica %in% regiao) %>%
    dplyr::filter(uf %in% unidade_federativa)


  return (trat)
}
