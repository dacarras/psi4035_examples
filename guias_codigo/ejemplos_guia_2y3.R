# apertura de datos
data_crime <- read.csv(
  url(
    'https://raw.githubusercontent.com/dacarras/psi4035_examples/master/data/datos_fl_crime.csv'
  )
)


# inspeccionar datos
dplyr::glimpse(data_crime)


# mostrar datos en tabla
library(dplyr)
data_crime %>%
knitr::kable(., digits = 2)


# variables de interes
#
# C  = tasa de criminalidad
# I  = renta mediana de ingresos (income)
# HS = porcentaje de la poblacion con al menos educación secundaria
# U  = porcentaje de urbanización


# dispersiogramas (codigo que no funciona)
# pairs(data_crime)

## forma de seleccionar columnas con dplyr
data_crime %>%
dplyr::select(C, I, HS, U) %>%
pairs()


## forma de seleccionar columnas en matriz
pairs(data_crime[,c('C', 'I', 'HS', 'U')])


# dicotomizar variables
## dicotomizar en codigo base
data_crime$I_high  <- data_crime$I  > mean(data_crime$I)
data_crime$HS_high <- data_crime$HS > mean(data_crime$HS)
data_crime$U_high  <- data_crime$U  > mean(data_crime$U)

## mostrar datos en tabla
library(dplyr)
data_crime %>%
knitr::kable(., digits = 2)

## dicotomizar variable con if_else (dplyr)
data_crime <- data_crime %>%
              mutate(dummy_i = dplyr::if_else(I > mean(I),1,0)) %>%
              mutate(dummy_h = dplyr::if_else(HS > mean(HS),1,0)) %>%
              mutate(dummy_u = dplyr::if_else(U > mean(U),1,0)) %>%
              dplyr::glimpse()

## dicotomizar (deviation coding) variable con if_else (dplyr)
data_crime <- data_crime %>%
              mutate(deviation_i = dplyr::if_else(I > mean(I),1,-1)) %>%
              mutate(deviation_h = dplyr::if_else(HS > mean(HS),1,-1)) %>%
              mutate(deviation_u = dplyr::if_else(U > mean(U),1,-1)) %>%
              dplyr::glimpse()


data_crime %>%
as.data.frame() %>%
t.test(c ~ dummy_i, data = .)



t.test(C ~ dummy_i, var.equal = TRUE, data = data_crime)
t.test(C ~ dummy_i, data = data_crime)


# apertura de datos
data_crime <- read.csv(
  url(
    'https://raw.githubusercontent.com/dacarras/psi4035_examples/master/data/datos_fl_crime.csv'
  )
)

## dicotomizar variable con if_else (dplyr)
data_crime <- data_crime %>%
              mutate(dummy_i = dplyr::if_else(I > mean(I),1,0)) %>%
              mutate(dummy_h = dplyr::if_else(HS > mean(HS),1,0)) %>%
              mutate(dummy_u = dplyr::if_else(U > mean(U),1,0)) %>%
              dplyr::glimpse()


## mostrar todos los datos
knitr::kable(data_crime)


