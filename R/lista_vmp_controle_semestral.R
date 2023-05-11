#' Valores maximos permitidos (VMP) de cada parametro de controle semestral
#'
#'
#'
#' @format A tibble with 86 rows and 5 variables:
#' \describe{
#'   \item{chave_parametro}{chr Parametro e VMP}
#'   \item{parametro}{chr Nome do parametro}
#'   \item{unidade}{chr unidade de medida do VMP}
#'   \item{vmp_ate_2021}{dbl VMP considerado para dados ate 2021, esse nao incluso}
#'   \item{vmp_novo}{dbl VMP considerado para dados de 2021 em diante}
#'  }
#'
#' @source \url{https://bvsms.saude.gov.br/bvs/saudelegis/gm/2017/prc0005_03_10_2017.html}
"lista_vmp_controle_semestral"
