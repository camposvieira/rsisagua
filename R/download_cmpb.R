

#Function download_cmpb (Controle mensal parametros basicos)

download_cmpb <- function(periodo,regiao=NULL,unidade_federativa=NULL){

  list_uf <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG",
               "MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR",
               "RS","SC","SE","SP","TO")

  list_regiao <- c("NORTE","SUL","CENTRO-OESTE","SUDESTE","NORDESTE")


  if(is.null(unidade_federativa)==TRUE){
    unidade_federativa <- list_uf
  }

  if(is.null(regiao)==TRUE){
    regiao <- list_regiao
  }


  #Auxiliary function for download csv files in a loop
  download_url_sisagua <- function(url, name_file){
    temp <- tempfile(fileext = ".zip")
    utils::download.file(url = url, destfile = temp)
    datazip <- utils::unzip(temp, files = name_file)
    output <- data.table::fread(datazip, sep=";")
    return(output)
  }

  #Loop for binding all years file selected
  cmpb <- NULL
  for(ano in periodo){
    name <- paste0("controle_mensal_parametros_basicos_", ano)
    url_completa <- paste0("https://sage.saude.gov.br/dados/sisagua/", name, ".zip")
    name_file_completo <- paste0(name, ".csv")
    cmpb_ano <- download_url_sisagua(url_completa, name_file_completo)
    cmpb <- base::rbind(cmpb, cmpb_ano)
  }

  #Load data and filter
  cmpb <- cmpb %>%
    janitor::clean_names() %>%
    dplyr::filter(ano_de_referencia %in% periodo) %>%
    dplyr::filter(regiao_geografica %in% regiao) %>%
    dplyr::filter(uf %in% unidade_federativa)

  return(cmpb)
}
