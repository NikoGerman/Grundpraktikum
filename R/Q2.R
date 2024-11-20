library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(ggrepel)
library(viridis)

### Question 2
### Education
### Do countries with higher central government debt as a percentage of GDP
### spend less on education relative to GDP?
#Answer: No. It's the other way around.

data_full <- readr::read_rds("Data/cleaned/Worldbank.RDS")
color_assigns <- readr::read_rds("Data/cleaned/Country_Colors.RDS")

data_clean_q2 <- data_full %>%
  select("Country Name", "Year", "Central government debt, total (% of GDP)",
         "Labor force with basic education (% of total working-age population with basic education)") %>%
  drop_na()

data_clean_q2 <- data_clean_q2 %>%
  left_join(color_assigns, by = c("Country Name" = "Country"))


ggplot(data_clean_q2, aes(x = `Central government debt, total (% of GDP)`, 
                          y = `Labor force with basic education (% of total working-age population with basic education)`,
                          color = ColorHex)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "grey", se = FALSE, linewidth = 0.75) +
  scale_color_identity(guide = "none") +
  geom_text_repel(data = data_clean_q2 %>% distinct(`Country Name`, ColorHex, .keep_all = TRUE),
                  aes(label = `Country Name`, color = ColorHex),  # Set text color to match dot color
                  size = 3,
                  max.overlaps = 10) +  
  labs(title = "Central Government Debt vs. Labor force with basic education",
       x = "Central Government Debt (in %)",
       y = "Labor force with basic education (% of total working-age population)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



### Are countries with higher education spending able to maintain lower pupil
### teacher ratios, and what impact might this have on education quality?

data_clean_q2_1 <- na.omit(data_full[ , c("Country Name",
                                          "Year",
                                          "Pupil-teacher ratio, primary",
                                          "Government expenditure on education, total (% of GDP)")])


# Step 1: Calculate the average government expenditure per country
avg_expenditure_per_country <- aggregate(data_clean_q2_1$`Government expenditure on education, total (% of GDP)`,
                                         by = list(Country = data_clean_q2_1$`Country Name`),
                                         FUN = mean, na.rm = TRUE)

# Rename the column for clarity
colnames(avg_expenditure_per_country)[2] <- "Avg_Gov_Expenditure"

# Step 2: Determine the mean of average expenditures
mean_avg_expenditure <- mean(avg_expenditure_per_country$Avg_Gov_Expenditure, na.rm = TRUE)

# Step 3: Select the countries
top_countries <- avg_expenditure_per_country[avg_expenditure_per_country$Avg_Gov_Expenditure > mean_avg_expenditure, "Country"]

# Step 4: Filter the original dataset to include only the selected top countries
data_top50pc_edu_spend <- data_clean_q2_1[data_clean_q2_1$`Country Name` %in% top_countries, ]

# Create the ggplot with points and lines
ggplot(data_top50pc_edu_spend, aes(x = Year, y = `Pupil-teacher ratio, primary`, color = `Country Name`)) +
  geom_line() +  # Connect the points with lines
  geom_point() +  # Add points at each data point
  labs(title = "Pupil-Teacher Ratio Over Time \n for countries with higher than average education spending",
       x = "Year",
       y = "Pupil-Teacher Ratio",
       color = "Country") +
  scale_x_continuous(breaks = 2014:2018) +  # Ensure the x-axis only shows the years 2014 to 2018
  theme_minimal() +
  theme(legend.position = "right")


