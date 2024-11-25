library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(ggrepel)
library(viridis)
library(scales)


## Is there a relationship between the percentage of agricultural land and CO2
## emissions per capita across countries?

data_full <- readr::read_rds("Data/cleaned/Worldbank.RDS")
color_assigns <- readr::read_rds("Data/cleaned/Country_Colors.RDS")

data_full$'Carbon dioxide (CO2) emissions (total) excluding LULUCF (Mt CO2e)' <- 
  as.numeric(gsub("\\.", "", data_full$'Carbon dioxide (CO2) emissions (total) excluding LULUCF (Mt CO2e)'))

data_clean_q5 <- na.omit(data_full[ , c("Country Name",
                                          "Year",
                                          "Agricultural land (% of land area)",
                                          "Surface area (sq. km)",
                                          "Carbon dioxide (CO2) emissions (total) excluding LULUCF (Mt CO2e)",
                                          "Population, total")])
data_clean_q5$'CO2e per capita in tons' <- data_clean_q5$'Carbon dioxide (CO2) emissions (total) excluding LULUCF (Mt CO2e)' * 1000000/
  data_clean_q5$'Population, total'

data_clean_q5 <- data_clean_q5 %>%
  left_join(color_assigns, by = c("Country Name" = "Country"))

data_clean_q5_summary <- data_clean_q5 %>%
  group_by(`Country Name`) %>%
  summarise(
    mean_agricultural_land = mean(`Agricultural land (% of land area)`, na.rm = TRUE),
    mean_surface_area = mean(`Surface area (sq. km)`, na.rm = TRUE),
    mean_co2_total = mean(`Carbon dioxide (CO2) emissions (total) excluding LULUCF (Mt CO2e)`, na.rm = TRUE),
    mean_population = mean(`Population, total`, na.rm = TRUE),
    mean_co2e_per_capita = mean(`CO2e per capita in tons`, na.rm = TRUE),
    ColorHex = first(ColorHex)  # Assuming ColorHex is the same for each country
  )


ggplot(data_clean_q5, aes(x = `Agricultural land (% of land area)`, 
                          y = `CO2e per capita in tons`,
                          color = ColorHex)) +
  geom_point(size = 2) +
  scale_y_log10(labels = comma) +
  geom_smooth(method = "lm", color = "grey", se = FALSE, linewidth = 0.75) +
  scale_color_identity(guide = "none") +
  geom_text_repel(data = data_clean_q5 %>% distinct(`Country Name`, ColorHex, .keep_all = TRUE),
                  aes(label = `Country Name`, color = ColorHex),
                  size = 3,
                  max.overlaps = 10) +  
  labs(title = "Agricultural land vs. CO2 emissions per Capita",
       x = "Agricultural land (% of land area)",
       y = "CO2 emissions per Capita (Tons)") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data_clean_q5_summary, aes(x = mean_agricultural_land, 
                          y = mean_co2e_per_capita,
                          color = ColorHex)) +
  geom_point(size = 2) +
  scale_y_log10(labels = comma) +
  geom_smooth(method = "lm", color = "grey", se = FALSE, linewidth = 0.75) +
  scale_color_identity(guide = "none") +
  geom_text_repel(data = data_clean_q5_summary %>% distinct(`Country Name`, ColorHex, .keep_all = TRUE),
                  aes(label = `Country Name`, color = ColorHex),
                  size = 3,
                  max.overlaps = 10) +  
  labs(title = "Agricultural land vs. CO2 emissions per Capita",
       x = "Agricultural land (% of land area)",
       y = "CO2 emissions per Capita (Tons)") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

exclude_index <- 2 #Remove Aruba because Outlier

ggplot(data_clean_q5_summary[-exclude_index, ], aes(x = mean_surface_area, 
                                  y = mean_co2e_per_capita,
                                  color = ColorHex)) +
  geom_point(size = 2) +
  scale_y_log10(labels = comma) +
  scale_x_log10(labels = comma) +
  geom_smooth(method = "lm", color = "grey", se = FALSE, linewidth = 0.75) +
  scale_color_identity(guide = "none") +
  geom_text_repel(data = data_clean_q5_summary[-exclude_index, ] %>% distinct(`Country Name`, ColorHex, .keep_all = TRUE),
                  aes(label = `Country Name`, color = ColorHex),
                  size = 3,
                  max.overlaps = 10) +  
  labs(title = "Surface Area vs. CO2 emissions per Capita",
       x = "Surface Area",
       y = "CO2 emissions per Capita (Tons)") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data_clean_q5_summary[-exclude_index, ], aes(x = mean_surface_area, 
                                                    y = mean_agricultural_land,
                                                    color = ColorHex)) +
  geom_point(size = 2) +
  scale_x_log10(labels = comma) +
  geom_smooth(method = "lm", color = "grey", se = FALSE, linewidth = 0.75) +
  scale_color_identity(guide = "none") +
  geom_text_repel(data = data_clean_q5_summary[-exclude_index, ] %>% distinct(`Country Name`, ColorHex, .keep_all = TRUE),
                  aes(label = `Country Name`, color = ColorHex),
                  size = 3,
                  max.overlaps = 10) +  
  labs(title = "Surface Area vs. Agricultural land",
       x = "Surface Area",
       y = "Agricultural land (% of land area)") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))



