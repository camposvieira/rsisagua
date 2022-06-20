
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
    rename(regiao=Região.Geográfica,
           uf=UF,
           regional_saude=Regional.de.Saúde,
           municipio=Município,
           cod_ibge=Código.IBGE,
           tipo_inst=Tipo.da.Instituição,
           sigla_inst=Sigla.da.Instituição,
           nome_inst=Nome.da.Instituição,
           cnpj_inst=CNPJ.da.Instituição,
           nome_escrit_reg_loc=Nome.do.escritório.regional.local,
           cnpj_escrit_reg_loc=CNPJ.do.escritório.regional.local,
           tipo_forma_abast=Tipo.da.Forma.de.Abastecimento,
           cod_forma_abast=Código.Forma.de.abastecimento,
           nome_forma_abast=Nome.da.Forma.de.Abastecimento,
           ano_ref=Ano.de.referência,
           data_registro=Data.de.registro,
           data_preenchimento=Data.de.preenchimento,
           carro_pipa=Carro.Pipa,
           chafariz=Chafariz,
           fonte=Fonte,cisterna=Cisterna,
           canalizacao=Canalização,
           caixa_dagua=Caixa.d.água,
           sem_reservacao=Sem.reservação,
           outro_suprimento=Outro.tipo.de.suprimento,
           capt_superficial=Captação.superficial,
           capt_subterranea=Captação.subterrânea,
           capt_agua_chuva=Captação.de.Água.de.chuva,
           filtracao=Filtração,
           desinfeccao=Desinfecção,
           econ_resid_perm=Número.de.economias.residenciais..domicílios.permanentes.,
           econ_resid_ocasional=Número.de.economias.residenciais..de.uso.ocasional.,
           raz_hab_dom=Razão.habitantes.domicílio,
           pop_recebe_saa=Pop.recebe.Água.de.SAA,
           pop_recebe_saa_sac=Pop.recebe.Água.de.SAA.SAC)

  return (cobertura)
}
