
#----------------------------------------------------------
# tabla 3.2
#----------------------------------------------------------

data_table_3_2 <- read.table(
text="
person   y    x    x_q   xy   z
1        2    8     64   16   1
2        3    9     81   27   2
3        3    9     81   27   1
4        4   10    100   40   2
5        7    6     36   42   1
6        5    7     49   35   2
7        5    4     16   20   1
8        7    5     25   35   2
9        8    3      9   24   1
10       9    1      1    9   2
11       9    2      4   18   1
12      10    2      4   20   2

",
header=TRUE, stringsAsFactors = FALSE)

#--------------------------------------
# mostrar tabla
#--------------------------------------

knitr::kable(data_table_3_2)


#------------------------------------------------------------------------------
# ajustar modelos
#------------------------------------------------------------------------------

#--------------------------------------
# preparar datos codigo base
#--------------------------------------

data_model <- data_table_3_2[, c('y','x')]
data_model$x_cgm <- data_table_3_2$x - mean(data_table_3_2$x, na.rm = TRUE)


#--------------------------------------
# preparar datos (dplyr)
#--------------------------------------

data_model <- data_table_3_2 %>%
              mutate(x_g = mean(x, na.rm = TRUE)) %>%
              mutate(x_cgm = x - x_g) %>%
              dplyr::select(y, x, x_cgm, z) %>%
              dplyr::glimpse()


#--------------------------------------
# mostrar datos
#--------------------------------------

knitr::kable(data_model)

#--------------------------------------
# formulas
#--------------------------------------

f00 <- as.formula(y ~ + 1)
f01 <- as.formula(y ~ + 1 + x)
f02 <- as.formula(y ~ + 1 + x_cgm)
f03 <- as.formula(y ~ + 1 + z)

#--------------------------------------
# ajustar modelos
#--------------------------------------

m00 <- lm(f00, data = data_model)
m01 <- lm(f01, data = data_model)
m02 <- lm(f02, data = data_model)
m03 <- lm(f03, data = data_model)

#--------------------------------------
# comparar modelos de forma sintética
#--------------------------------------

texreg::screenreg(
    list(m00, m01, m02, m03),
    star.symbol = "*", 
    center = TRUE, 
    doctype = FALSE,
    dcolumn = TRUE, 
    booktabs = TRUE,
    single.row = FALSE
    )


anova(m01,m02)
