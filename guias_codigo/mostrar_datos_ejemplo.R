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