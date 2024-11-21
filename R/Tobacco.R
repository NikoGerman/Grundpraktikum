library(dplyr)
library(ggplot2)
library(patchwork)

# Reading Data
data <- readRDS("Data/cleaned/Worldbank.RDS")
colors <- readRDS("Data/cleaned/Country_Colors.RDS")
colors_named <- setNames(colors$ColorHex, colors$Country)

# Selecting the target data
filtered_data <- data %>%
  select(`Country Name`, Year, 
         `Prevalence of current tobacco use (% of adults)`,
         `GDP per capita, PPP (constant 2021 international $)`) %>%
  rename(Country = `Country Name`,
         Tobacco = `Prevalence of current tobacco use (% of adults)`,
         GDP = `GDP per capita, PPP (constant 2021 international $)`)

# Cleaning the data and removing Qatar.
filtered_data <- filtered_data %>%
  filter(!is.na(Tobacco) & !is.na(GDP) &  Country != "Qatar")

# Grouping by whether the GDP per capita exceeds 20,000
filtered_data <- filtered_data %>%
  mutate(GDP_group = ifelse(GDP > 20000, "GDP per capita Over 20000", "GDP per capita Below 20000"))

# Plot over 20000
plot_high_gdp <- ggplot(filtered_data %>% filter(GDP_group == "GDP per capita Over 20000"),
                        aes(x = Tobacco, y = GDP, color = Country)) +
  geom_point(size = 1) +
  geom_smooth(aes(group = Country, color = Country), method = "lm", se = FALSE, linewidth = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.8) +
  scale_color_manual(values = colors_named) +
  labs(x = "Prevalence of current tobacco use (% of adults)",
       y = "GDP per capita, PPP (constant 2021 international $)",
       title = "GDP per capita Over 20000") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    legend.key.size = unit(0.3, "cm"),  
    legend.spacing.x = unit(0.1, "cm"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray85")
  ) +
  xlim(0, 60)

# Plot under 20000
plot_low_gdp <- ggplot(filtered_data %>% filter(GDP_group == "GDP per capita Below 20000"),
                       aes(x = Tobacco, y = GDP, color = Country)) +
  geom_point(size = 1) +
  geom_smooth(aes(group = Country, color = Country), method = "lm", se = FALSE, linewidth = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.8) +
  scale_color_manual(values = colors_named) +
  labs(x = "Prevalence of current tobacco use (% of adults)",
       y = "GDP per capita, PPP (constant 2021 international $)",
       title = "GDP per capita Below 20000") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    legend.key.size = unit(0.3, "cm"),  
    legend.spacing.x = unit(0.1, "cm"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray85")
  ) +
  xlim(0, 60)

final_plot <- (plot_high_gdp | plot_low_gdp) +
  plot_annotation(title = "Relationship Between GDP Per Capita and Tobacco Usage Prevalence") 

print(final_plot)



