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

# Usando los datos correspondientes a los años 1980 y 1981 realice un ANOVA con predictores inter e intra sujeto evaluando el efecto sobre logaritmo del salario por hora (“lwage”) de tres predictores: si pertenece a un sindicato (“union"), la educación de los trabajadores (“educ”) y el año en el que se registraron los datos (year). No incluya interacciones en el modelo y asegúrese de considerar en el modelo la correlación de las observaciones intra sujeto. Interprete los resultados.


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
# probando covariables como factores
#----------------------------------------------------------
library(dplyr)
# sin especificar factores
aov(lwage ~ union + educ + year, data = data_salary) %>%
summary()

# cor especificar factores
aov(lwage ~ as.factor(union) + educ + as.factor(year), data = data_salary) %>%
summary()

#----------------------------------------------------------
# probando covariables como factores
#----------------------------------------------------------
library(dplyr)
# sin especificar factores
aov(lwage ~ union + educ + year, data = data_salary) %>%
summary()

# cor especificar factores
aov(lwage ~ union + as.factor(educ) + year, data = data_salary) %>%
summary()


#----------------------------------------------------------
# orden de los factores (cambia las sumas de cuadrados)
#----------------------------------------------------------

library(dplyr)
# orden
aov(lwage ~ union + educ + as.factor(year), data = data_salary) %>%
summary()

# orden
aov(lwage ~ as.factor(year) + educ + union, data = data_salary) %>%
summary()


# Nota: no cambia la prueba global de F.


#----------------------------------------------------------
# orden de los factores en regresion
#----------------------------------------------------------

library(dplyr)
# orden
lm(lwage ~ union + educ + as.factor(year), data = data_salary) %>%
summary()

# orden
lm(lwage ~ as.factor(year) + educ + union, data = data_salary) %>%
summary()


# Nota: no cambia la prueba global de F.


#----------------------------------------------------------
# modelos con error, y sin error compartido
#----------------------------------------------------------


## especificacion del modelo
modelo_tradicional <- as.formula(lwage ~ union + educ + year)
modelo_error_relac <- as.formula(lwage ~ union + educ + year + Error(nr))

# ajuste del modelo
modelo_01 <- aov(modelo_tradicional, data = data_salary)
modelo_02 <- aov(modelo_error_relac, data = data_salary)

# resultados
summary(modelo_01)
summary(modelo_02)


#----------------------------------------------------------
# informacion sobre las covariables
#----------------------------------------------------------

data_salary[1:10,]  %>%
knitr::kable()


# union = covariable no fija al tiempo (i.e., dinamica)
# educ  = covariable fija al tiempo (i.e., diferencia entre personas)


aov(lwage ~ union + educ + year + Error(as.factor(nr)),
    data = data_salary) %>%
summary()


#----------------------------------------------------------
# varianza en proporciones
#----------------------------------------------------------

# Eta_2 = SSeffect / SStotal

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

# Usando los datos correspondientes a los años 1980 y 1981 realice una regresión múltiple con predictores inter e intra sujeto evaluando el efecto sobre logaritmo del salario por hora (“lwage”) de tres predictores: si pertenece a un sindicato (“union"), la educación de los trabajadores (“educ”) y el año en el que se registraron los datos (year). No incluya interacciones en el modelo y asegúrese de considerar en el modelo la correlación de las observaciones intra sujeto usando un intercepto aleatorio. Interprete los resultados.


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
# 
#----------------------------------------------------------


data_model <- data_salary %>%
              mutate(id_i = as.factor(nr)) %>%
              dplyr::glimpse()


lmer(lwage ~ union + educ + year + (1|nr), data = data_salary, REML = FALSE) %>%
summary()


lmer(lwage ~ union + educ + year + (1|id_i), data = data_model, REML = FALSE) %>%
summary()


#----------------------------------------------------------
# ajuste sin manipular covariables
#----------------------------------------------------------


lmer(lwage ~ union + educ + year + (1|nr), 
    data = data_salary, REML = FALSE) %>%
summary()


#----------------------------------------------------------
# función de cluster mean
#----------------------------------------------------------

c_mean <- function (x, j) 
{
    ave(x, j, FUN = function(x) mean(x, na.rm = T))
}

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



# modelo desagregado (Rights et al., 2019)

lmer(lwage ~ union_w + union_b + educ_b + time + (1|id_j), 
    data = data_model, REML = FALSE) %>%
summary()

# Random effects:
#  Groups   Name        Variance Std.Dev.
#  id_j     (Intercept) 0.1102   0.3320  
#  Residual             0.1609   0.4011  
# Number of obs: 1090, groups:  id_j, 545

lmer(lwage ~ union_m + educ_b + time + (1|id_j), 
    data = data_model, REML = FALSE) %>%
summary()


# Random effects:
#  Groups   Name        Variance Std.Dev.
#  id_j     (Intercept) 0.1105   0.3325  
#  Residual             0.1620   0.4024  
# Number of obs: 1090, groups:  id_j, 545

library(nlme)

nlme::lme(
    lwage ~ union_w + union_b + educ_b + time,
    random = ~1|id_j,
    data = data_model
    ) %>%
anova()



lmer(lwage ~ union_w + union_b + educ_b + time + (1|id_j), 
    data = data_model) %>%
anova()



