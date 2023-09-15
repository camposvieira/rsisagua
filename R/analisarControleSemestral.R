# Controle semestral ------------------------------------
#Faz a análise dos parâmetros de controle semestral por período, região, uf ou municípios.
#Cria uma tabela dos valores de referência com os VMPs da nova norma e da norma antiga.
#Assim, para valores até 2021 (não incluso), o VMP considerado é da antiga norma. De 2021 para frente
#são considerados os novos valores de VMP, bom como a retirada ou inclusão de parâmetros (deve ser
#ajustado quando o Sisagua for atualizado com o módulo de controle semestral.)





# Lista de parâmetros e VMP -----------------------------------------------
#ETILBENZENO (alterado para 0,3 mg/L. na portaria está como 300 microg/l),
#XILENOS (alterado para 0,5 mg/L. na portaria está como 500 microg/l)
#TOLUENO (alterado para 0,03 mg/L. na portaria está como 30 microg/l)

#'Analisa os dados de controle semestral de acordo com o VMP de cada parâmetro
#'
#'Cria uma tabela dos valores de referencia com os VMPs da nova norma de potabilidade
#' e da norma antiga. Assim, para valores ate 2021 (nao incluso), o VMP considerado e da antiga norma.
#' De 2021 em diante sao considerados os novos valores de VMP, bem como a retirada
#' ou inclusao de parametros (deve ser ajustado quando o Sisagua for atualizado
#' com o modulo de controle semestral, bem como as unidades para etilbenzeno, xilenos e tolueno).
#'
#'@param periodo numeric vector Selecao do ou dos anos de referencia, a partir de 2014.
#'@param regiao string vector Selecao da ou das regioes geograficas.
#'@param uf string vector Selecao da ou das unidades federativas.
#'@param municipios string vector Selecao do ou dos municipios.
#'
#'@details Essa funcao carrega os dados de controle semestral e os analisa de acordo com
#' a norma de potabilidade vigente no periodo selecionado
#'
#'@return Data frame do conjunto de dados de controle semestral com o total de analises
#' realizadas e o percentual de amostras acima do VMP para cada parâmetro
#'
#'@examples
#'\dontrun{
#'df5 <- analisarControleSemestral(periodo = 2023, municipios = "AMPARO")
#'}
#'
#'@note O periodo de dados e um campo obrigatorio e inicia em 2014 ate o ano atual.
#'Os parametros regiao, unidade_federativa e municipios devem sempre ser inseridos com letra
#'maiuscula e entre aspas, conforme exemplo.
#'
#'@export

analisarControleSemestral <- function(periodo, regiao = NULL, uf = NULL, municipios = NULL){

  #Mensagem ao usuário
  usethis::ui_info("Analisando os dados com base no Anexo XX da Portaria de Consolidação N°5...")

  # Carregando as bases de dados com o pacote rsisagua --------------------------------------------

  cs <- rsisagua::download_cs(periodo = periodo, regiao = regiao, unidade_federativa = uf)


  #Filtrar muncípios a partir do argumento da função existente
  if(is.null(municipios) == FALSE){
    cs <- cs |> dplyr::filter(municipio %in% {{municipios}})
  }

  #Se não houver município no argumento, segue sem o filtro
  if(is.null(municipios) == TRUE) {
    cs <- cs
  }

  #Ajuste do grupo de parâmetros dos produtos secundários de desinfecção: transforma 1 e 2 trimestre em um parâmetro único

  cs$grupo_de_parametros <-
    stringr::str_replace_all(
      cs$grupo_de_parametros,
      c(
        "Produtos Secundários de Desinfecção - 1º Trimestre" =
          "Produtos Secundários de Desinfecção",
        "Produtos Secundários de Desinfecção - 2º Trimestre" =
          "Produtos Secundários de Desinfecção"
      )
    )

  #Ajuste do formato de números ou caracteres nos campos lq, ld e resultado----

  cs <- cs |>
    dplyr::mutate(
      ld = as.numeric(gsub(",", ".", ld)),
      lq = as.numeric(gsub(",", ".", lq)),
      resultado = ifelse(
        resultado %in% c("MENOR_LQ",
                         "MENOR_LD"),
        resultado,
        as.numeric(gsub(",", ".", resultado))
      )
    )


  # Análises ----------------------------------------------------------------


  analise_cs <-
    cs |> dplyr::left_join(rsisagua::lista_vmp_controle_semestral, by = c(parametro = "chave_parametro")) |>
    dplyr::mutate(resultado_numerico = ifelse(
      resultado %in% c("MENOR_LQ",
                       "MENOR_LD"),
      NA,
      as.numeric(resultado)
    )) |>
    dplyr::mutate(
      conformidade =
        dplyr::case_when(
          (ano_de_referencia >= 2022 & resultado_numerico > vmp_novo & !is.na(resultado_numerico)) ~ FALSE,
          (ano_de_referencia < 2021 & resultado_numerico > vmp_ate_2021 & !is.na(resultado_numerico)) ~ FALSE,
          (ano_de_referencia = 2021 & semestre_de_referencia == 1 & resultado_numerico > vmp_ate_2021 & !is.na(resultado_numerico)) ~ FALSE,
          (ano_de_referencia = 2021 & semestre_de_referencia == 2 & resultado_numerico > vmp_novo & !is.na(resultado_numerico)) ~ FALSE,
          (ano_de_referencia > 2021 & resultado == "MENOR_LQ" & lq > vmp_novo) ~ NA,
          (ano_de_referencia = 2021 & semestre_de_referencia == 2 & resultado == "MENOR_LQ" & lq > vmp_novo) ~ NA,
          (ano_de_referencia = 2021 & semestre_de_referencia == 1 & resultado == "MENOR_LQ" & lq > vmp_ate_2021) ~ NA,
          (ano_de_referencia < 2021 & resultado == "MENOR_LQ" & lq > vmp_ate_2021) ~ NA,
          (ano_de_referencia > 2021 & resultado == "MENOR_LD" & ld > vmp_novo) ~ NA,
          (ano_de_referencia = 2021 & semestre_de_referencia == 2 & resultado == "MENOR_LD" & ld > vmp_novo) ~ NA,
          (ano_de_referencia = 2021 & semestre_de_referencia == 1 & resultado == "MENOR_LD" & ld > vmp_ate_2021) ~ NA,
          (ano_de_referencia < 2021 & resultado == "MENOR_LD" & ld > vmp_ate_2021) ~ NA,
          TRUE ~ TRUE
        )
    )





















  #Agrupa por uf, municipio o que mais interessar

  resume_cs <- analise_cs |>
    dplyr::group_by(municipio, uf,
             ano_de_referencia,
             semestre_de_referencia,
             ponto_de_monitoramento,
             grupo_de_parametros,
             parametro.y,
             vmp_ate_2021,
             vmp_novo
    ) |>
    dplyr::summarise(
      n_conformidade = sum(conformidade, na.rm = T),
      n_nao_conforme = sum(!conformidade, na.rm = T),
      n_inconclusivo = sum(is.na(conformidade)),
      total_analises = dplyr::n()
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(perc_analises_acima_vmp = round((n_nao_conforme / total_analises) *
                                             100, 2))



  # Organização do data frame -----------------------------------------------

  resume_cs <- resume_cs |>
    dplyr::select(-n_inconclusivo, -n_conformidade) |>
    dplyr::rename("analises_acima_vmp" = n_nao_conforme,
           "parametro" = parametro.y)


  # Ajuste de valores em notação científica

  options(scipen = 999)


  return(resume_cs)

}
