# Question 2:
# Gibt es einen Zusammenhang zwischen dem Anteil der landwirtschaftlichen 
# Nutzfläche und den Pro-Kopf-CO₂-Emissionen in verschiedenen Ländern? 
# Spielt die Größe des Landes eine Rolle?
library(dplyr)
library(tidyr)
library(ggplot2)

Worldbank <- readr::read_rds("Data/cleaned/Worldbank.RDS")

colo <- country_colors$ColorHex
names(colo) <- country_colors$Country

### plot Acricultural Land vs CO2 emmisions faceted by Country
p1 <- Worldbank %>%
  ggplot(aes(x = `Agricultural land (% of land area)`,
             y = `Carbon dioxide (CO2) emissions (total) excluding LULUCF (Mt CO2e)`)) +
  geom_point(aes(color = `Country Name`)) +
  theme_light() +
  scale_color_manual(values = colo)

p1  
p1  + scale_y_log10()
p1 + facet_wrap(vars(`Country Name`), scale = "free_x") +
  guides(color = "none") +
  geom_smooth(method = "lm", se = FALSE, color = "grey", linewidth = .85) +
  scale_y_log10() +
  labs(x = "Agricultural Land (%)", y = "CO2 Emmissions (megatonnes)", title = "Agricultural Land vs. CO2 Emmissions")



WB_index <- Worldbank %>%
  filter(Year == 2000) %>%
  select(`Country Code`, `Agricultural land (% of land area)`, `Carbon dioxide (CO2) emissions (total) excluding LULUCF (Mt CO2e)`) %>%
  rename(Agri.Index = `Agricultural land (% of land area)`, CO2.Index = `Carbon dioxide (CO2) emissions (total) excluding LULUCF (Mt CO2e)`)

wb <- Worldbank %>% full_join(WB_index, by = "Country Code") %>%
  mutate(Agri.Index = `Agricultural land (% of land area)`/Agri.Index * 100, 
         CO2.Index = `Carbon dioxide (CO2) emissions (total) excluding LULUCF (Mt CO2e)`/CO2.Index * 100) %>%
  select(`Country Name`, Year, Agri.Index, CO2.Index)

ggplot(wb, aes(Agri.Index, CO2.Index, colour = `Country Name`)) +
  geom_point() +
  geom_path(color = "lightgrey") +
  facet_wrap(~`Country Name`, scale = "free") + 
  #scale_y_log10() +
  guides(color = "none") +
  #ggrepel::geom_text_repel(aes(label = Year), direction = "y", force = 0.5) +
  theme_light() +
  scale_color_manual(values = colo)


wb %>%
  filter(`Country Name` == "Viet Nam") %>%
  ggplot(aes(Agri.Index, CO2.Index, color = `Country Name`)) +
  geom_point() +
  geom_path() +
  scale_y_log10() +
  ggrepel::geom_text_repel(aes(label = Year), direction = "both", force = 0.4, color = "darkgrey") +
  theme_light() +
  scale_color_manual(values = colo)
