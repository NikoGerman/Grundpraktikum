data <- readRDS("~/Grundpraktikum/Data/cleaned/Worldbank.RDS")

library(dplyr)
library(ggplot2)
library(patchwork)

filtered_data <- data %>%
  select(`Country Name`, Year, 
         `Prevalence of current tobacco use (% of adults)`,
         `GDP per capita, PPP (constant 2021 international $)`) %>%
  rename(Country = `Country Name`,
         Tobacco = `Prevalence of current tobacco use (% of adults)`,
         GDP = `GDP per capita, PPP (constant 2021 international $)`)

filtered_data <- filtered_data %>%
  filter(!is.na(Tobacco) & !is.na(GDP))

plot1 <- ggplot(filtered_data, aes(x = Tobacco, y = GDP, color = Country)) +
  geom_point(size = 1) +
  labs(x = "Prevalence of current tobacco use (% of adults)",
       y = "GDP per capita, PPP (constant 2021 international $)",
       title = "GDP vs Adult tobacco use rates") +
  theme_minimal() +
  theme(legend.title = element_text()) 
  

print(plot1)
