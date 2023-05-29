#Carregando a base dos VMPs de controle semestral

lista_vmp_controle_semestral <- read.csv2("vmp_cs.csv", row.names = NULL , options(scipen = 999) )

options(scipen = 999)

#Exportando para a pasta /data
usethis::use_data(lista_vmp_controle_semestral, overwrite = T)
