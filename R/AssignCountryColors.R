library(dplyr)
library(viridis)
library(ggplot2)
library(forcats)

data_full <- readr::read_rds("Data/cleaned/Worldbank.RDS")

unique_countries <- sort(unique(data_full$`Country Name`))
num_countries <- length(unique_countries)
color_palette <- viridis(num_countries, option = "D")
country_colors <- data.frame(
  Country = unique_countries,
  ColorHex = color_palette,
  stringsAsFactors = FALSE
)
saveRDS(country_colors, file = "Data/cleaned/Country_Colors.rds")

#Showcasing all country & color combinations:
ggplot(country_colors, aes(x = fct_rev(reorder(Country, Country)), y = 1, fill = ColorHex)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Unique Colors Assigned to Each Country",
       x = "Country",
       y = "",
       fill = "Color")

