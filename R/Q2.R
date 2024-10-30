library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(ggrepel)
library(viridis)

source("C:/Users/thomas/Desktop/git/Stat_GrundPraktikum/Grundpraktikum/R/ReadingData.R")

### Question 2
### Education
### Do countries with higher central government debt as a percentage of GDP
### spend less on education relative to GDP?
#Answer: No. It's the other way around.

data_full <- Worldbank
data_clean_q2 <- na.omit(data_full[ , c("Country Name",
                            "Year",
                            "Central government debt, total (% of GDP)",
                            #"Current education expenditure, total (% of total expenditure in public institutions)",
                            "Government expenditure on education, total (% of GDP)")])



# Create the scatterplot with a trend line, uniform color points, and no legend
ggplot(data_clean_q2, aes(x = `Central government debt, total (% of GDP)`, 
                 y = `Government expenditure on education, total (% of GDP)`,
                 color = `Country Name`)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "grey", se = FALSE, linewidth = 0.75) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Central Government Debt vs. Education Expenditure, (% of GDP)",
       x = "Central Government Debt",
       y = "Government Expenditure on Education",
       color = "Country") +
  scale_x_continuous(limits = c(10, 75)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))




q1_reg <- lm(formula = `Central government debt, total (% of GDP)` ~ `Current education expenditure, total (% of total expenditure in public institutions)`,
                  data = data_full)

ggplot(data = data_full, aes(x = `Current education expenditure, total (% of total expenditure in public institutions)`, 
                             y = `Central government debt, total (% of GDP)`)) +
  geom_point(color = "blue", size = 2) +                  # Scatter plot of data points
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Linear regression line with confidence interval
  labs(title = "Linear Regression: Debt vs. Education Expenditure",
       x = "Current Education Expenditure (% of total in public institutions)",
       y = "Central Government Debt (% of GDP)") +
  theme_minimal()

### Are countries with higher education spending able to maintain lower pupil
### teacher ratios, and what impact might this have on education quality?
