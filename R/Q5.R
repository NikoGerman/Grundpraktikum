library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(ggrepel)
library(viridis)
library(scales)

source("C:/Users/thomas/Desktop/git/Stat_GrundPraktikum/Grundpraktikum/R/ReadingData.R")

## Is there a relationship between the percentage of agricultural land and CO2
## emissions per capita across countries?

data_full <- Worldbank
data_clean_q5 <- na.omit(data_full[ , c("Country Name",
                                        "Year",
                                        "Agricultural land (% of land area)",
                                        "Surface area (sq. km)",
                                        "CO2 emissions (metric tons per capita)")])

ggplot(data_clean_q5, aes(x = `Agricultural land (% of land area)`, 
                          y = `CO2 emissions (metric tons per capita)`,
                          color = `Country Name`)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "grey", se = FALSE, linewidth = 0.75) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Agricultural land vs. CO2 emissions",
       x = "Agricultural land (% of land area)",
       y = " CO2 emissions (metric tons per capita)",
       color = "Country") +
  scale_x_continuous(limits = c(0, 85)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  # Add text labels for country names
  geom_text_repel(data = data_clean_q5 %>% 
                    group_by(`Country Name`) %>% 
                    slice(1), # Choose one point per country
                  aes(label = `Country Name`),
                  size = 3, 
                  show.legend = FALSE)

##Without Qatar:

ggplot(data_clean_q5[data_clean_q5$`Country Name` != "Qatar", ], 
       aes(x = `Agricultural land (% of land area)`, 
           y = `CO2 emissions (metric tons per capita)`,
           color = `Country Name`)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "grey", se = FALSE, linewidth = 0.75) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Agricultural land vs. CO2 emissions\n(Without Qatar)",
       x = "Agricultural land (% of land area)",
       y = " CO2 emissions (metric tons per capita)",
       color = "Country") +
  scale_x_continuous(limits = c(0, 85)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  # Add text labels for country names
  geom_text_repel(data = data_clean_q5[data_clean_q5$`Country Name` != "Qatar", ] %>% 
                    group_by(`Country Name`) %>% 
                    slice(1), # Choose one point per country
                  aes(label = `Country Name`),
                  size = 3, 
                  show.legend = FALSE)

##Does the size of the surface area of
## the country play a role?
ggplot(data_clean_q5, aes(x = `Surface area (sq. km)`, 
                          y = `CO2 emissions (metric tons per capita)`,
                          color = `Country Name`)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "grey", se = FALSE, linewidth = 0.75) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Surface area vs. CO2 emissions",
       x = "Surface area (sq. km)",
       y = " CO2 emissions (metric tons per capita)",
       color = "Country") +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  # Add text labels for country names
  geom_text_repel(data = data_clean_q5 %>% 
                    group_by(`Country Name`) %>% 
                    slice(1), # Choose one point per country
                  aes(label = `Country Name`),
                  size = 3, 
                  show.legend = FALSE)

#Without Qatar and Kazakhstan:
##Does the size of the surface area of
## the country play a role?
# Filter out Qatar and Kazakhstan
data_filtered <- data_clean_q5 %>%
  filter(`Country Name` != "Qatar", `Country Name` != "Kazakhstan")

# Create the ggplot
ggplot(data_filtered, aes(x = `Surface area (sq. km)`, 
                          y = `CO2 emissions (metric tons per capita)`,
                          color = `Country Name`)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "grey", se = FALSE, linewidth = 0.75) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Surface area vs. CO2 emissions\n(Without Qatar and Kazakhstan",
       x = "Surface area (sq. km)",
       y = "CO2 emissions (metric tons per capita)",
       color = "Country") +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  # Add text labels for country names
  geom_text_repel(data = data_filtered %>% 
                    group_by(`Country Name`) %>% 
                    slice(1), # Choose one point per country
                  aes(label = `Country Name`),
                  size = 3, 
                  show.legend = FALSE)


