library(dplyr)
library(ggplot2)
library(ggrepel)
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

# Cleaning the data and removing Qatar and China
filtered_data <- filtered_data %>%
  filter(!is.na(Tobacco) & !is.na(GDP) & Country != "Qatar" & Country != "China")

# Grouping by whether the GDP over 20,000
filtered_data <- filtered_data %>%
  mutate(GDP_group = ifelse(GDP > 20000, "GDP per capita Over 20000", "GDP per capita Below 20000"))

# Select one point per country for labeling
labels <- filtered_data %>%
  group_by(Country) %>%
  slice_max(order_by = Year)  

# Plot under 20000
plot_low_gdp <- ggplot(filtered_data %>% filter(GDP_group == "GDP per capita Below 20000"),
                       aes(x = GDP, y = Tobacco, color = Country)) +
  geom_point(size = 1) +
  geom_smooth(aes(group = Country, color = Country), method = "lm", se = FALSE, linewidth = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.8) +
  geom_text_repel(data = labels %>% filter(GDP_group == "GDP per capita Below 20000"),
                  aes(label = Country),
                  size = 3, max.overlaps = 10) +  
  scale_color_manual(values = colors_named) +
  labs(x = "GDP per capita, PPP (constant 2021 international $)",
       y = "Prevalence of current tobacco use (% of adults)",
       title = "GDP per capita Under 20000") +
  theme_minimal() +
  theme(
    legend.position = "none",  
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray85")
  ) +
  ylim(0, 60)

# Plot over 20000
plot_high_gdp <- ggplot(filtered_data %>% filter(GDP_group == "GDP per capita Over 20000"),
                        aes(x = GDP, y = Tobacco, color = Country)) +
  geom_point(size = 1) +
  geom_smooth(aes(group = Country, color = Country), method = "lm", se = FALSE, linewidth = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.8) +
  geom_text_repel(data = labels %>% filter(GDP_group == "GDP per capita Over 20000"),
                  aes(label = Country),
                  size = 3, max.overlaps = 10) +  
  scale_color_manual(values = colors_named) +
  labs(x = "GDP per capita, PPP (constant 2021 international $)",
       y = NULL,
       title = "GDP per capita Over 20000") +
  theme_minimal() +
  theme(
    legend.position = "none",  
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray85")
  ) +
  ylim(0, 60)


final_plot <- (plot_low_gdp | plot_high_gdp) +
  plot_annotation(title = "Relationship Between GDP Per Capita and Tobacco Usage Prevalence") 

print(final_plot)