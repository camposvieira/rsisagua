
# Download  do conjunto de cobertura de abastecimento e filtros por periodo, região e uf
download_cobertura <- function(periodo,regiao=NULL,uf=NULL){


  # Função para download e carregamento em formato csv dos conjuntos de dados
  download_url_sisagua <- function(url, name_file){
    temp <- tempfile()
    download.file(url = url, destfile = temp)
    datazip <- unzip(temp, files = name_file)
    output <- read.csv(datazip, sep=";")
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

  if(is.null(uf)==TRUE){

    uf <-  list_uf
  }

  cobertura <- download_cobert_1("cadastro_populacao_abastecida")
  cobertura <- cobertura %>% filter(Ano.de.referência %in% periodo) %>%
    filter(Região.Geográfica %in% regiao) %>%
    filter(UF %in% uf) %>%
    janitor::clean_names()

  return (cobertura)
}
