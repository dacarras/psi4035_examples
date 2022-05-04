# Taller 7: ANOVA de dos factores

## Cargar datos


#----------------------------------------------------------
# cargar datos
#----------------------------------------------------------

#--------------------------------------
# cargar dplyr
#--------------------------------------

library(dplyr)

#--------------------------------------
# datos Vik (2014, p128)
#--------------------------------------

table_11_1 <- read.table(text = "
person  score   sex    diag
      1     3     m    none
      2     5     m    none
      3     6     m    none
      4     8     m    none
      5     9     m    none
      6    11     m    none
      7    19     m    depressed
      8    15     m    depressed
      9    16     m    depressed
     10    16     m    depressed
     11    19     m    depressed
     12    17     m    depressed
     13     3     f    none
     14     5     f    none
     15     4     f    none
     16     6     f    none
     17     8     f    none
     18    10     f    none
     19    24     f    depressed
     20    24     f    depressed
     21    22     f    depressed
     22    23     f    depressed
     23    19     f    depressed
     24    20     f    depressed
", header = TRUE, stringsAsFactors = FALSE)

#--------------------------------------
# mostrar datos
#--------------------------------------

knitr::kable(table_11_1, digits = 3)
