#--------------------------------------
# load data
#--------------------------------------

data_raw <- read.csv(
  url(
'https://github.com/dacarras/psi2301_examples/raw/master/data/dem_16.csv'
  )
)

library(dplyr)
dem_16 <- data_raw %>%
          mutate(civ_level = case_when(
            level == 3 ~ 1,
            level == 2 ~ 1,
            level == 1 ~ 0,
            level == 0 ~ 0
            ))

#--------------------------------------
# check data
#--------------------------------------

dplyr::glimpse(dem_16)

#--------------------------------------
# anova
#--------------------------------------

aov(aut ~ dem_group*civ_level, data = dem_16) %>%
summary()


#--------------------------------------
# comparaciones multiples
#--------------------------------------

emmeans::emmeans(aov(aut ~ dem_group*civ_level, data = dem_16), 
    specs = pairwise ~ dem_group*civ_level, 
    type = "response", 
    adjust = "bonferroni"
    )

table_emmeans <- emmeans::emmeans(aov(aut ~ dem_group*civ_level, data = dem_16), 
    specs = pairwise ~ dem_group*civ_level, 
    type = "response", 
    adjust = "bonferroni"
    )$contrasts


table_emmeans %>%
knitr::kable(., digits = 2)

# |complex civ_level0 - limited civ_level1       |    -0.01| 1.54| 2477|    0.00|    1.00|
# |complex civ_level0 - minimalist civ_level1    |     3.44| 1.29| 2477|    2.67|    0.12|
# |limited civ_level0 - minimalist civ_level0    |    -1.09| 0.60| 2477|   -1.83|    1.00|

#--------------------------------------
# medias con intervalo de confianza
#--------------------------------------

library(dplyr)
library(srvyr)
dem_16_srs <- dem_16 %>% 
              as_survey_design(ids = 1)


dem_16_srs %>%
group_by(dem_group, civ_level) %>%
summarize(
  score = survey_mean(aut, na.rm = TRUE, vartype = c('ci', 'se')),
  n = n()
) %>%
knitr::kable(., digits = 2)

#--------------------------------------
# plot de medias por grupos
#--------------------------------------

data_plot <- dem_16_srs %>%
group_by(dem_group, civ_level) %>%
summarize(
  score = survey_mean(aut, na.rm = TRUE, vartype = c('ci', 'se')),
  n = n()
)


library(ggplot2)
data_plot %>%
arrange(desc(score)) %>%
mutate(civ_dem = paste0(civ_level, '_', dem_group)) %>%
mutate(civ_dem_label = forcats::as_factor(civ_dem)) %>%
ggplot(., aes(civ_dem_label, score)) +
geom_point(size = 3) +
geom_linerange(aes(ymin = score_low, ymax = score_upp), size = 2) +
ylab('') +
xlab('') +
scale_y_continuous(breaks = seq(30, 70, by = 5), limits = c(30, 70)) +
coord_flip() +
theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1)
    )