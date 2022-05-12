# ejemplo generico
# _como_abrir_un_archivo_spss

file <- 'datos_en_spss.sav'

folder <- 'C:/' # 'C:\'

# haven # Nota: libreria para abrir datos.


datos_de_interes <- haven::read_spss(paste0(folder, file))
datos_de_interes <- haven::read_sav(paste0(folder, file))


# ejemplo real

# /Users/d/Dropbox (Personal)/_data/ICCS2009/ICGCHLC2.sav

# /Users/d/Dropbox (Personal)/_data/ICCS2009/
# ICGCHLC2.sav

folder <- '/Users/d/Dropbox (Personal)/_data/ICCS2009/'
file   <- 'ICGCHLC2.sav'

iccs_09_chl <- haven::read_sav(paste0(folder, file))

ls()

# inspeccionar los datos
dplyr::glimpse(iccs_09_chl)

r4sda::variable_label(iccs_09_chl$IC2G05D)

# > r4sda::variable_label(iccs_09_chl$IC2G05D)
# [1] "PARTICIPATION-TEACHER REPRESENTATIVE"


dplyr::count(iccs_09_chl, PRIVATE)

r4sda::variable_label(iccs_09_chl$PRIVATE)
# [1] "*PRIVATE SCHOOL INDICATOR*"

r4sda::value_labels(iccs_09_chl$PRIVATE)
# [1] "*PRIVATE SCHOOL INDICATOR*"


# > r4sda::value_labels(iccs_09_chl$PRIVATE)
# # A tibble: 4 × 2
#   value label         
#   <chr> <chr>         
# 1 0     PUBLIC SCHOOL 
# 2 1     PRIVATE SCHOOL
# 3 7     INVALID       
# 4 9     OMITTED 


datos_planos <- r4sda::remove_labels(iccs_09_chl)
dplyr::glimpse(datos_planos)
r4sda::variable_label(datos_planos$PRIVATE)







# -------------------------------------
# datos originales
# -------------------------------------

folder <- '/Users/d/Dropbox (Personal)/_data/ICCS2009/'
file   <- 'ICGCHLC2.sav'

iccs_09_chl <- haven::read_sav(paste0(folder, file))


# -------------------------------------
# datos planos
# -------------------------------------


datos_planos <- r4sda::remove_labels(iccs_09_chl)


# -------------------------------------
# resto del codigo
# -------------------------------------

r4sda::variable_label(iccs_09_chl$IC2G19)
r4sda::value_labels(iccs_09_chl$IC2G19)









