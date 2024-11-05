data <- read.csv("Data\\Worldbank.csv", header = TRUE)

library(dplyr)
library(ggplot2)
library(patchwork)

filtered_data <- data %>%
  select(Country.Name, Year, 
         Prevalence.of.overweight..weight.for.height..female....of.children.under.5.,
         Prevalence.of.overweight..weight.for.height..male....of.children.under.5.,
         GDP.per.capita..PPP..constant.2021.international...) %>%
  rename(Country = Country.Name,
         Overweight_prevalence_female = Prevalence.of.overweight..weight.for.height..female....of.children.under.5.,
         Overweight_prevalence_male = Prevalence.of.overweight..weight.for.height..male....of.children.under.5.,
         GDP_per_capita = GDP.per.capita..PPP..constant.2021.international...)

filtered_data <- filtered_data %>%
  filter(!is.na(Overweight_prevalence_female) & !is.na(GDP_per_capita) & !is.na(Overweight_prevalence_male))

plot1 <- ggplot(filtered_data, aes(x = Overweight_prevalence_female, y = GDP_per_capita, color = Country)) +
  geom_point(size = 1) +
  geom_smooth(aes(group = Country), method = "lm", se = FALSE, color = "black", size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.8) +
  labs(x = "Overweight Rate Female Child",
       y = "GDP per Capita (PPP, constant 2021 international $)") +
  theme_minimal() +
  theme(legend.title = element_text()) +
  scale_color_brewer(palette = "Set1")

plot2 <- ggplot(filtered_data, aes(x = Overweight_prevalence_male, y = GDP_per_capita, color = Country)) +
  geom_point(size = 1) +
  geom_smooth(aes(group = Country), method = "lm", se = FALSE, color = "black", size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.8) +
  labs(x = "Overweight Rate Male Child",
       y = NULL) +
  theme_minimal() +
  theme(legend.title = element_text()) +
  scale_color_brewer(palette = "Set1")

combined_plot <- (plot1 | plot2) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")

combined_plot <- combined_plot + plot_annotation(title = "Overweight Rate Child vs. GDP per Capita by Country (Fitted)")& theme(plot.title = element_text(hjust = 0.5))

print(combined_plot)
