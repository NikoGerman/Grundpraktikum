library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(ggrepel)
library(viridis)

data_full <- readr::read_rds("Data/cleaned/Worldbank.RDS")
color_assigns <- readr::read_rds("Data/cleaned/Country_Colors.RDS")

### Question 2
### Do countries with higher central government debt as a percentage of GDP
### have a lower percentage of labor force with basic education?
#Answer: No. It's the other way around.

data_clean_q2 <- na.omit(data_full[ , c("Country Name",
                                          "Year",
                                          "Central government debt, total (% of GDP)",
                                          "Labor force with basic education (% of total working-age population with basic education)")])

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
       x = "Central Government Debt (in % of GDP)",
       y = "Labor force with basic education (% of total working-age population)") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))



# Are countries with higher percentage of labor force with basic education
# able to maintain lower pupil-teacher ratios, and what impact might this have
# on education quality? 

data_clean_q2_1 <- na.omit(data_full[ , c("Country Name",
                                          "Year",
                                          "Pupil-teacher ratio, tertiary",
                                          "Labor force with basic education (% of total working-age population with basic education)")])
data_clean_q2_1 <- data_clean_q2_1 %>%
  left_join(color_assigns, by = c("Country Name" = "Country"))

# Step 1: Calculate the average % of labor force with basic education
avg_laborforce_w_basic_edu <- aggregate(data_clean_q2_1$`Labor force with basic education (% of total working-age population with basic education)`,
                                         by = list(Country = data_clean_q2_1$`Country Name`),
                                         FUN = mean, na.rm = TRUE)


colnames(avg_laborforce_w_basic_edu)[2] <- "Avg_LabFor_w_bEdu"
mean_avg_edu <- mean(avg_laborforce_w_basic_edu$Avg_LabFor_w_bEdu, na.rm = TRUE)
top_countries <- avg_laborforce_w_basic_edu[avg_laborforce_w_basic_edu$Avg_LabFor_w_bEdu > mean_avg_edu, "Country"]
data_top50pc_bEdu <- data_clean_q2_1[data_clean_q2_1$`Country Name` %in% top_countries, ]

drawQ2_1Plot <- function(df){
  
  first_year_data <- df %>%
    group_by(`Country Name`) %>%
    slice_min(order_by = Year)  # Select the earliest year for each country
  
  ggplot(df, aes(x = Year, y = `Pupil-teacher ratio, tertiary`, color = ColorHex, group = `Country Name`)) +
    geom_line(linewidth = 0.7) +  # Make lines slightly thicker
    geom_point(size = 1.2) +  # Increase point size
    geom_smooth(aes(group = 1), method = "lm", color = "grey", se = FALSE, linewidth = 0.75) +  # Add single regression line
    geom_text_repel(data = first_year_data,
                    aes(label = `Country Name`, color = ColorHex),  # Use ColorHex for text color
                    vjust = 1,
                    nudge_x = -0.2,  # Slight nudge to the left
                    size = 3,
                    max.overlaps = 10) +  
    labs(title = "Pupil-Teacher (Tertiary Edu.) Ratio Over Time \nfor countries with higher than average basic edu (as % of working age population)",
         x = "Year",
         y = "Pupil-Teacher Ratio",
         color = "Country") +
    scale_color_identity(guide = "none") +  # Use hex colors directly and hide legend
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5),  # Center title
          legend.position = "none")  # Remove legend if using text labels
}



data_top50pc_bEdu_filtered <- data_top50pc_bEdu %>%
  group_by(`Country Name`) %>%
  filter(n() > 3) %>%
  ungroup()

drawQ2_1Plot(data_top50pc_bEdu)
drawQ2_1Plot(data_top50pc_bEdu_filtered)

