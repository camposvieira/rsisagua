#Carregando a base dos VMPs de controle semestral

lista_vmp_controle_semestral <- read.csv2("vmp_cs.csv", row.names = NULL)

#Exportanto para a pasta /data
usethis::use_data(lista_vmp_controle_semestral, overwrite = T)
