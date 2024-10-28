### Question 2
### Education
### Do countries with higher central government debt as a percentage of GDP
### spend less on education relative to GDP?


library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)


data_full <- Worldbank
data_full$`Current education expenditure, total (% of total expenditure in public institutions)`
data_full$`Government expenditure on education, total (% of GDP)`

data_trim <- data_full[ , c("Country Name",
                            "Central government debt, total (% of GDP)",
                            "Current education expenditure, total (% of total expenditure in public institutions)",
                            "Government expenditure on education, total (% of GDP)")]

data_avg <- data_full %>%
  group_by(`Country Name`) %>%
  summarize(
    mean_debt = mean(`Central government debt, total (% of GDP)`, na.rm = TRUE),
    mean_education_expenditure = mean(`Current education expenditure, total (% of total expenditure in public institutions)`, na.rm = TRUE)
  )

summary(data_full$`Central government debt, total (% of GDP)`)
summary(data_full$`Current education expenditure, total (% of total expenditure in public institutions)`)
summary(data_full$`Government expenditure on education, total (% of GDP)`)

# Scatter plot of both variables
ggplot(data = data_full, aes(x = `Central government debt, total (% of GDP)`, 
                             y = `Current education expenditure, total (% of total expenditure in public institutions)`)) +
  geom_point(color = "blue", size = 2) +  # Scatter plot with blue points
  labs(title = "Debt (% of GDP) vs. Education Expenditure (% of total expenditure in public institutions)",
       x = "Central Government Debt",
       y = "Education expenditure") +
  theme_minimal()  # Clean theme for aesthetics



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
