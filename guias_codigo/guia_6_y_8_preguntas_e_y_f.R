#----------------------------------------------------------
# apertura de datos
#----------------------------------------------------------

data_salary <- read.csv(
url('https://raw.githubusercontent.com/dacarras/psi4035_examples/master/guias/salarios8081.csv')
    )

#----------------------------------------------------------
# inspeccionar datos
#----------------------------------------------------------

dplyr::glimpse(data_salary)

#----------------------------------------------------------
# codeboook
#----------------------------------------------------------


# nr        — Identificador de sujeto
# lwage     — Logaritmo del ingreso por hora del trabajador
# year      — año en que se observo el salario
# ethnicity — Etnia del trabajador 
#             (0 = blanco; 1 = negro; 2 = hispánico)
# union     — Membresía en un sindicato 
#             (0 = no, 1 = si)
# educ      — Numero de años de educación

#----------------------------------------------------------
# pregunta e)
#----------------------------------------------------------

# Usando los datos correspondientes a los años 1980 y 1981
# realice un ANOVA con predictores inter e intra sujeto evaluando el efecto sobre logaritmo del salario por hora
# (“lwage”) de tres predictores: si pertenece a un sindicato
# (“union"), la educación de los trabajadores (“educ”)
# y el año en el que se registraron los datos (year).
# No incluya interacciones en el modelo y asegúrese 
# de considerar en el modelo la correlación de las
# observaciones intra sujeto. Interprete los resultados.


#----------------------------------------------------------
# pregunta e), requisitos
#----------------------------------------------------------


# - Notas
#     + todos los datos (1980, 1981)
#     + aov()
#     + lwage ~ union + educ + year
#     + agregar modelo de errores
#     + interpretar resultados

#----------------------------------------------------------
# pregunta e), descriptivos de las covariables
#----------------------------------------------------------

dplyr::count(data_salary, union)

dplyr::count(data_salary, educ)

dplyr::count(data_salary, year)


#----------------------------------------------------------
# anova within-between
#----------------------------------------------------------

data_salary[1:10,]  %>%
knitr::kable()


# union = covariable no fija al tiempo (i.e., dinamica)
# educ  = covariable fija al tiempo (i.e., diferencia entre personas)


aov(lwage ~ union + educ + year + Error(as.factor(nr)),
    data = data_salary) %>%
summary()

#----------------------------------------------------------
# resultados obtenidos
#----------------------------------------------------------

# > aov(lwage ~ union + educ + year + Error(as.factor(nr)),
# +     data = data_salary) %>%
# + summary()
# 
# Error: as.factor(nr)
#            Df Sum Sq Mean Sq F value   Pr(>F)    
# union       1  12.59  12.588   32.83 1.67e-08 ***
# educ        1  13.76  13.761   35.89 3.81e-09 ***
# Residuals 542 207.83   0.383                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Error: Within
#            Df Sum Sq Mean Sq F value  Pr(>F)    
# union       1    0.4   0.403   2.493   0.115    
# year        1    3.9   3.896  24.119 1.2e-06 ***
# Residuals 543   87.7   0.162                    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#----------------------------------------------------------
# pregunta f)
#----------------------------------------------------------

# Usando los datos correspondientes a los años 1980 y 1981
# realice una regresión múltiple con predictores inter e 
# intra sujeto evaluando el efecto sobre logaritmo del
# salario por hora (“lwage”) de tres predictores: si 
# pertenece a un sindicato (“union"), la educación de 
# los trabajadores (“educ”) y el año en el que se
# registraron los datos (year). No incluya interacciones
# en el modelo y asegúrese de considerar en el modelo
# la correlación de las observaciones intra sujeto
# usando un intercepto aleatorio. Interprete los resultados.


#----------------------------------------------------------
# pregunta f), requisitos
#----------------------------------------------------------


# - Notas
#     + todos los datos (1980, 1981)
#     + aov()
#     + lwage ~ union + educ + year
#     + agregar modelo de errores
#     + interpretar resultados


#----------------------------------------------------------
# función de cluster mean
#----------------------------------------------------------

c_mean <- function (x, j) 
{
    ave(x, j, FUN = function(x) mean(x, na.rm = T))
}

#----------------------------------------------------------
# preparar datos
#----------------------------------------------------------

data_model <- data_salary %>%
              mutate(id_j = as.numeric(as.factor(nr))) %>%
              mutate(id_k = 1) %>%
              # centrado de variables
              ## union (centrado intra sujeto)
              mutate(union_c = c_mean(union, id_j)) %>%
              mutate(union_g = c_mean(union, id_k)) %>%
              mutate(union_w = union   - union_c) %>%
              mutate(union_b = union_c - union_g) %>%
              mutate(union_m = union   - union_g) %>%
              ## union (centrado intra sujeto)
              mutate(educ_g = c_mean(educ, id_k)) %>%
              mutate(educ_b = educ - educ_g) %>%
              ## year
              mutate(time = case_when(
                year == 1980 ~ 0,
                year == 1981 ~ 1
                )) %>%
              dplyr::glimpse()


#----------------------------------------------------------
# mlm within-between
#----------------------------------------------------------

# Nota modelo desagregado (Rights et al., 2019)

lmer(lwage ~ union_w + union_b + educ_b + time + (1|id_j), 
    data = data_model, REML = TRUE) %>%
anova()

# Analysis of Variance Table
#         npar Sum Sq Mean Sq F value
# union_w    1 0.4027  0.4027  2.4933
# union_b    1 5.3022  5.3022 32.8279
# educ_b     1 5.7961  5.7961 35.8860
# time       1 3.8955  3.8955 24.1189

# Nota: se replican los valors F de ANOVA.

#----------------------------------------------------------
# mlm within-between, con maximum likelihood
#----------------------------------------------------------


lmer(lwage ~ union_m + educ_b + time + (1|id_j), 
    data = data_model, REML = FALSE) %>%
summary()


#----------------------------------------------------------
# resultados obtenidos
#----------------------------------------------------------

# > lmer(lwage ~ union_m + educ_b + time + (1|id_j), 
# +     data = data_model, REML = FALSE) %>%
# + summary()
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: lwage ~ union_m + educ_b + time + (1 | id_j)
#    Data: data_model
# 
#      AIC      BIC   logLik deviance df.resid 
#   1590.1   1620.1   -789.1   1578.1     1084 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -4.8320 -0.2950  0.0847  0.4418  3.0981 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  id_j     (Intercept) 0.1105   0.3325  
#  Residual             0.1620   0.4024  
# Number of obs: 1090, groups:  id_j, 545
# 
# Fixed effects:
#             Estimate Std. Error t value
# (Intercept)  1.39329    0.02236  62.311
# union_m      0.20710    0.03789   5.465
# educ_b       0.06428    0.01074   5.987
# time         0.11977    0.02438   4.913
# 
# Correlation of Fixed Effects:
#         (Intr) unin_m educ_b
# union_m -0.002              
# educ_b   0.000  0.004       
# time    -0.545  0.003  0.000



