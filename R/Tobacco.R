data <- readRDS("Data/cleaned/Worldbank.RDS")

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
  filter(!is.na(Tobacco) & !is.na(GDP) &  Country != "Qatar")

filtered_data <- filtered_data %>%
  mutate(GDP_group = ifelse(GDP > 20000, "GDP Above 20000", "GDP Below 20000"))

plot1 <- ggplot(filtered_data, aes(x = Tobacco, y = GDP, color = Country)) +
  geom_point(size = 2) +
  geom_smooth(aes(group = Country, color = Country), method = "lm", se = FALSE, linewidth = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.8) +
  labs(x = "Prevalence of current tobacco use (% of adults)",
       y = "GDP per capita, PPP (constant 2021 international $)",
       title = "GDP vs Adult tobacco use rates") +
  theme_minimal() +
  theme(legend.title = element_text()) +
  facet_wrap(~ GDP_group)
  

print(plot1)


