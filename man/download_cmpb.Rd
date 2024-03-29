% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_cmpb.R
\name{download_cmpb}
\alias{download_cmpb}
\title{Download dos dados de controle mensal - parametros basicos}
\usage{
download_cmpb(periodo, regiao = NULL, unidade_federativa = NULL)
}
\arguments{
\item{periodo}{Selecao do ou dos anos de referencia, a partir de 2014.}

\item{regiao}{Selecao da ou das regioes geograficas.}

\item{unidade_federativa}{Selecao da ou das unidades federativas.}
}
\value{
Data frame do conjunto de dados com filtros a partir dos parametros da funcao
}
\description{
Download dos dados de controle mensal - parametros basicos
}
\details{
Essa funcao da acesso aos dados do monitoramento mensal da qualidade da agua para
consumo humano realizado pelo prestador de servico de abastecimento
de agua por forma de abastecimento, conforme preconizado na norma
de potabilidade do Ministerio da Saude.
}
\note{
O periodo de dados e um campo obrigatorio e inicia em 2014 ate o ano atual.
Os parametros regiao e unidade_federativa devem sempre ser inseridos com letra
maiuscula e entre aspas, conforme exemplo.
}
\examples{
\dontrun{
df5 <- download_cmpb(regiao = "NORTE")
df6 <- download_cmpb(unidade_federativa = "CE")
}
}
