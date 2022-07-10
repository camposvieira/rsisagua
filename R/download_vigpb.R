#'Download dos dados de vigilancia - parametros basicos
#'
#'@param periodo Selecao do ou dos anos de referencia, a partir de 2014.
#'@param regiao Selecao da ou das regioes geograficas.
#'@param unidade_federativa Selecao da ou das unidades federativas.
#'@details Essa funcao da acesso aos dados do monitoramento da qualidade da agua
#'   para consumo humano realizado rotineiramente pelo setor saude, contemplando
#'   os resultados das analises de qualidade da agua de baixa complexidade:
#'   Bacterias Heterotroficas, Cloro Residual Combinado, Cloro Residual Livre,
#'   Coliformes Totais, Cor, Dioxido de Cloro, Escherichia Coli, Fluoreto, pH e Turbidez.
#'@return Data frame do conjunto de dados com filtros a partir dos parametros da funcao
#'@examples
#'\dontrun{
#'df5 <- download_cmpb(regiao = "NORTE")
#'df6 <- download_cmpb(unidade_federativa = "CE")
#'}
#'@note O periodo de dados e um campo obrigatorio e inicia em 2014 ate o ano atual.
#'Os parametros regiao e unidade_federativa devem sempre ser inseridos com letra
#'maiuscula e entre aspas, conforme exemplo.
#'@export

download_vigpb <- function(periodo,regiao=NULL,unidade_federativa=NULL){

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
  vigpb <- NULL
  for(ano in periodo){
    name <- paste0("vigilancia_parametros_basicos_", ano)
    url_completa <- paste0("https://sage.saude.gov.br/dados/sisagua/", name, ".zip")
    name_file_completo <- paste0(name, ".csv")
    vigpb_ano <- download_url_sisagua(url_completa, name_file_completo)
    vigpb <- base::rbind(vigpb, vigpb_ano)
  }

  #Load data and filter
  vigpb <- vigpb %>%
    janitor::clean_names() %>%
    dplyr::filter(vigpb$ano %in% periodo) %>%
    dplyr::filter(vigpb$regiao_geografica %in% regiao) %>%
    dplyr::filter(vigpb$uf %in% unidade_federativa)

  return(vigpb)
}
