
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rsisagua

<!-- badges: start -->
<!-- badges: end -->

The goal of rsisagua is to download and filter Sisagua files from Portal
Brasileiro de Dados Abertos

## Installation

You can install the development version of rsisagua from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("camposvieira/rsisagua")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# X <- download_vigdp(periodo = c(2020:2022),
#                     regiao = "NORTE",
#                     unidade_federativa = c("AC", "AM", "RO"))
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
#summary(x)
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
