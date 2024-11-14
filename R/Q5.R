# Question 2:
# Gibt es einen Zusammenhang zwischen dem Anteil der landwirtschaftlichen 
# Nutzfläche und den Pro-Kopf-CO₂-Emissionen in verschiedenen Ländern? 
# Spielt die Größe des Landes eine Rolle?
library(dplyr)
library(tidyr)
library(ggplot2)

Worldbank <- readr::read_rds("Data/cleaned/Worldbank.RDS")

### plot Acricultural Land vs CO2 emmisions faceted by Country
p1 <- Worldbank %>%
  ggplot(aes(x = `Agricultural land (% of land area)`,
             y = `CO2 emissions (metric tons per capita)`)) +
  geom_point(aes(color = `Country Name`)) +
  theme_light()

p1  
p1  + scale_y_log10()
p1 + facet_wrap(vars(`Country Name`), scale = "free_x") +
  guides(color = "none") +
  scale_y_log10()

### as above but indexed variables
p2 <- Worldbank_Q2 %>%
  ggplot(aes(x = Agriculture.Index2014,
             y = CO2.Index2014)) +
  geom_point(aes(color = `Country Name`)) +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  theme_light() +
  scale_y_log10()

p2
p2 + facet_wrap(vars(`Country Name`), scale = "free_x")  +
  guides(color = "none")
p2 + facet_wrap(vars(`Country Name`), scale = "fixed") +
  guides(color = "none")