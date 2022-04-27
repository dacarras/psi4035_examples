# apertura de datos
data_crime <- read.csv(
  url(
    'https://raw.githubusercontent.com/dacarras/psi4035_examples/master/data/datos_fl_crime.csv'
  )
)

#----------------------------------------------------------
# codigo base
#----------------------------------------------------------


#--------------------------------------
# crear vectores
#--------------------------------------

#
# dicotomizar variables
## dicotomizar en codigo base
I_high  <- data_crime$I  > mean(data_crime$I)
HS_high <- data_crime$HS > mean(data_crime$HS)
U_high  <- data_crime$U  > mean(data_crime$U)


class(I_high)

#--------------------------------------
# ejemplo empleado
#--------------------------------------

#
# dicotomizar variables
## dicotomizar en codigo base
data_crime$I_high  <- data_crime$I  > mean(data_crime$I)
data_crime$HS_high <- data_crime$HS > mean(data_crime$HS)
data_crime$U_high  <- data_crime$U  > mean(data_crime$U)



#--------------------------------------
# gramatica
#--------------------------------------

# nombre_variable_nueva <- operacion_aritmetica
# data_table$nombre_variable_nueva <- operacion_aritmetica

