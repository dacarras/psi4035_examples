data_ptsd <- psi2301::ptsd_data

data_ptsd %>%
psi2301::

objeto_simple <- "hola,soy un objeto simple"
str(objeto_simple)


str(data_ptsd)

library(dplyr)
data_ptsd %>%
psi2301::remove_labels() %>%
dplyr::glimpse()


hist(data_ptsd$age)