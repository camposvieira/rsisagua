#'Function download_ciano (Cianotoxinas e cianobactérias)
#'
#'@param periodo Selecao do ou dos anos de referencia, a partir de 2014.
#'@param regiao Selecao da ou das regioes geograficas.
#'@param unidade_federativa Selecao da ou das unidades federativas.
#'@return Data frame do conjunto de dados com filtros a partir dos parametros da funcao
#'@export
#'@examples
#'df <- download_ciano(c(2014:2020), "NORTE", c("AM", "PA", "RO"))
#'df2 <- download_ciano(2020, unidade_federativa = "SC")
#'df3 <- download_ciano(2015, regiao = "NORTE")
#'df4 <- download_ciano(2022)
#'\dontrun{
#'df5 <- download_ciano(regiao = "NORTE")
#'df6 <- download_ciano(unidade_federativa = "CE")
#'}
#'
#'@note O periodo de dados e um campo obrigatorio e inicia em 2014 ate o ano atual.
#'Os parametros regiao e unidade_federativa devem sempre ser inseridos com letra
#'maiuscula e entre aspas, conforme exemplo.

download_ciano <- function(periodo,regiao=NULL,unidade_federativa=NULL){

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
  utils::download.file("https://sage.saude.gov.br/dados/sisagua/vigilancia_cianobacterias_cianotoxinas.zip", temp)
  datazip <- utils::unzip(temp)


  #Load data and filter
  ciano <- data.table::fread(datazip, sep=";") %>%
    janitor::clean_names() %>%
    dplyr::filter(ano %in% periodo) %>%
    dplyr::filter(regiao_geografica %in% regiao) %>%
    dplyr::filter(uf %in% unidade_federativa)


  return (ciano)
}



