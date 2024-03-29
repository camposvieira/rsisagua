#'Download dos dados de controle mensal - demais parametros
#'
#'@param periodo Selecao do ou dos anos de referencia, a partir de 2014.
#'@param regiao Selecao da ou das regioes geograficas.
#'@param unidade_federativa Selecao da ou das unidades federativas.
#'@details Essa funcao da acesso aos dados do monitoramento da qualidade da agua
#'   realizado pelo prestador de servico em frequencia inferior a mensal, contemplando
#'   os resultados das analises de qualidade da agua de alta complexidade.
#'@return Data frame do conjunto de dados com filtros a partir dos parametros da funcao
#'@examples
#'\dontrun{
#'df5 <- download_cmdp(regiao = "NORTE")
#'df6 <- download_cmdp(unidade_federativa = "CE")
#'}
#'@note O periodo de dados e um campo obrigatorio e inicia em 2014 ate o ano atual.
#'Os parametros regiao e unidade_federativa devem sempre ser inseridos com letra
#'maiuscula e entre aspas, conforme exemplo.
#'@export

download_cmdp <- function(periodo,regiao=NULL,unidade_federativa=NULL){

  #Initial Warnings
  if (missing(periodo) & missing(regiao) & missing(unidade_federativa)){
    usethis::ui_stop("Voce deve inserir, ao menos, o argumento periodo para
    baixar os dados de todas as regioes e unidades federativas")
  }


  if (missing(periodo)){
    usethis::ui_stop("Voce deve inserir os anos de selecao dos dados no argumento
                     periodo")
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

  #Download and unzip
  temp <- tempfile(fileext = ".zip")
  utils::download.file("https://sage.saude.gov.br/dados/sisagua/controle_mensal_demais_parametros.zip", temp)
  datazip <- utils::unzip(temp)


  #Load data and filter
  cmdp <- data.table::fread(datazip, sep=";", encoding = "Latin-1") %>%
    janitor::clean_names() %>%
    dplyr::filter(ano_de_referencia %in% {{periodo}}) %>%
    dplyr::filter(regiao_geografica %in% {{regiao}}) %>%
    dplyr::filter(uf %in% {{unidade_federativa}})


  return (cmdp)
}
