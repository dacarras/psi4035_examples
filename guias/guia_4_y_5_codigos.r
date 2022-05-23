# apertura de datos
data_crime <- read.csv('/Users/d/Dropbox (Personal)/_git/psi4035_examples/guias/faculty.csv'
  url(
    'https://raw.githubusercontent.com/dacarras/psi4035_examples/master/data/datos_fl_crime.csv'
  )
)

#----------------------------------------------------------
# apertura de datos
#----------------------------------------------------------

data_salary <- read.csv('faculty.csv')

#----------------------------------------------------------
# inspeccionar datos
#----------------------------------------------------------

dplyr::glimpse(data_salary)

#----------------------------------------------------------
# preparar datos
#----------------------------------------------------------



#----------------------------------------------------------
# modelos
#----------------------------------------------------------

f01 <- as.formula(salary ~ market + yearsdg + male + rank)


#----------------------------------------------------------
# ajustar modelos
#----------------------------------------------------------

m01 <- lm(f01, data = data_salary)

#----------------------------------------------------------
# mostrar estimados
#----------------------------------------------------------



## dicotomizar variable con if_else (dplyr)
data_crime <- data_crime %>%
              mutate(dummy_i = dplyr::if_else(I > mean(I),1,0)) %>%
              mutate(dummy_h = dplyr::if_else(HS > mean(HS),1,0)) %>%
              mutate(dummy_u = dplyr::if_else(U > mean(U),1,0)) %>%
              dplyr::glimpse()


## mostrar todos los datos
knitr::kable(data_crime)