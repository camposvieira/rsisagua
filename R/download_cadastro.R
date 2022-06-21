
# Download  do conjunto de cobertura de abastecimento e filtros por periodo, região e uf
download_cadastro <- function(periodo,regiao=NULL,unidade_federativa=NULL){


  # Função para download e carregamento em formato csv dos conjuntos de dados
  download_url_sisagua <- function(url, name_file){
    temp <- tempfile()
    utils::download.file(url = url, destfile = temp)
    datazip <- utils::unzip(temp, files = name_file)
    output <- data.table::fread(datazip, sep=";")
    return(output)
  }

  # Download inicial - Cadastro
  download_cobert_1 <- function(conjunto){
    cobertura <- NULL
    conjunto <- conjunto
    url_completa <- paste0("https://sage.saude.gov.br/dados/sisagua/", conjunto, ".zip")
    name_file_completo <- paste0(conjunto, ".csv")
    cobertura <- download_url_sisagua(url_completa, name_file_completo)

    return(cobertura)
  }

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

  cobertura <- download_cobert_1("cadastro_populacao_abastecida")
  cobertura <- cobertura %>%
    janitor::clean_names() %>%
    dplyr::filter(ano_de_referencia %in% periodo) %>%
    dplyr::filter(regiao_geografica %in% regiao) %>%
    dplyr::filter(uf %in% unidade_federativa)


  return (cobertura)
}



####testar
# # Download  do conjunto de cobertura de abastecimento e filtros por periodo, região e uf
# download_cadastro <- function(periodo,regiao=NULL,unidade_federativa=NULL){
#
#   #trazer aqui avisos para periodo, regiao e unidade federativa
#
#   list_uf <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG",
#                "MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR",
#                "RS","SC","SE","SP","TO")
#
#   list_regiao <- c("NORTE","NORDESTE","SUL","CENTRO-OESTE","SUDESTE")
#
#   if(is.null(regiao)==TRUE){
#     regiao <- list_regiao
#   }
#
#   if(is.null(unidade_federativa)==TRUE){
#     unidade_federativa <-  list_uf
#   }
#
#   #Download e unzip
#   temp <- tempfile()
#   utils::download.file(url = "https://sage.saude.gov.br/dados/sisagua/cadastro_populacao_abastecida.zip",destfile = temp, mode = "wb")
#   datazip <- utils::unzip(temp, files = "cadastro_populacao_abastecida.zip")
#
#   # # Try to download file
#   # tryCatch({
#   #   utils::download.file(file, temp, mode = "wb")
#   #   partial <- read.dbc::read.dbc(temp)
#   #   file.remove(temp)
#   # },
#
#   #Leitura e filtros
#   cadastro <- data.table::fread(datazip, sep=";") %>%
#     janitor::clean_names() %>%
#     filter(ano_de_referencia %in% periodo) %>%
#     filter(regiao_geografica %in% regiao) %>%
#     filter(uf %in% unidade_federativa)
#
#
#   return (cadastro)
# }

