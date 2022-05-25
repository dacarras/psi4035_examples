#----------------------------------------------------------
# apertura de datos
#----------------------------------------------------------

data_salary <- read.csv('faculty.csv')

#----------------------------------------------------------
# inspeccionar datos
#----------------------------------------------------------

dplyr::glimpse(data_salary)

#----------------------------------------------------------
# descriptives
#----------------------------------------------------------

r4sda::get_desc(data_salary) %>%
knitr::kable(., digits = 2)

#----------------------------------------------------------
# preparar datos
#----------------------------------------------------------

library(dplyr)
data_model <- data_salary %>%
              mutate(full = case_when(
                rank == 1 ~ 0, # profesor asistente
                rank == 2 ~ 0, # profesor asociado
                rank == 3 ~ 1  # profesor titular
                )) %>%
              mutate(id_j = 1) %>%
              mutate(market_g = r4sda::c_mean(market, id_j)) %>%
              mutate(market_m = market - market_g) %>%
              mutate(male_g   = r4sda::c_mean(male, id_j)) %>%
              mutate(male_m   = male - male_g) %>%
              dplyr::glimpse()

#----------------------------------------------------------
# cantidad de casos por dummy
#----------------------------------------------------------

dplyr::count(data_model, market)
dplyr::count(data_model, yearsdg)
dplyr::count(data_model, male)
dplyr::count(data_model, full)

#----------------------------------------------------------
# modelos
#----------------------------------------------------------

f01 <- as.formula(salary ~ market + yearsdg)

f02 <- as.formula(salary ~ market + yearsdg + market:yearsdg)

f03 <- as.formula(salary ~ market + male)
f04 <- as.formula(salary ~ market + male + market:male)

f05 <- as.formula(salary ~ market + male + market:male + full + full:male)

f06 <- as.formula(salary ~ market_m + male_m)
f07 <- as.formula(salary ~ market_m + male_m + market_m:male_m)

f08 <- as.formula(salary ~ market_m + male)
f09 <- as.formula(salary ~ market_m + male + market_m:male)

#----------------------------------------------------------
# ajustar modelos
#----------------------------------------------------------

m01 <- lm(f01, data = data_model)
m02 <- lm(f02, data = data_model)
m03 <- lm(f03, data = data_model)
m04 <- lm(f04, data = data_model)
m05 <- lm(f05, data = data_model)
m06 <- lm(f06, data = data_model)
m07 <- lm(f07, data = data_model)
m08 <- lm(f08, data = data_model)
m09 <- lm(f09, data = data_model)


#----------------------------------------------------------
# mostrar estimados
#----------------------------------------------------------

texreg::screenreg(
    list(m01, m02, m03, m04, m05, m06, m07),
    star.symbol = "*", 
    center = TRUE, 
    doctype = FALSE,
    dcolumn = TRUE, 
    booktabs = TRUE,
    single.row = FALSE,
    include.loglik = TRUE
    )

#----------------------------------------------------------
# mostrar estimados
#----------------------------------------------------------

texreg::screenreg(
    list(m03, m04, m06, m07, m08, m09),
    star.symbol = "*", 
    center = TRUE, 
    doctype = FALSE,
    dcolumn = TRUE, 
    booktabs = TRUE,
    single.row = FALSE
    )


#----------------------------------------------------------
# mostrar estimados
#----------------------------------------------------------

texreg::screenreg(
    list(m08, m09),
    star.symbol = "*", 
    center = TRUE, 
    doctype = FALSE,
    dcolumn = TRUE, 
    booktabs = TRUE,
    single.row = FALSE
    )


#----------------------------------------------------------
# comparacion modelos
#----------------------------------------------------------

anova(m03, m04)
anova(m06, m07)
anova(m08, m09)
