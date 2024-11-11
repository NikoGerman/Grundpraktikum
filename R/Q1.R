library(dplyr)
library(tidyr)
library(ggplot2)
# load cleaned Worldbank Data, discard 2022 and 2023 observations, since
# the variables we are interested in, are incomplete in these two years.
Worldbank <- readr::read_rds("Data/cleaned/Worldbank.RDS")


p <- Worldbank %>%
  ggplot(aes(y = `Adjusted net national income per capita (current US$)`))+
  scale_y_log10() +
  labs(y = "Net national income per capita",
       color = "Country",
       x = "Access to electricity",
       title = "National income vs Access to Electricity",
       caption = "x: Access to Electricity in %\ny: Adjusted Net national income per capita in current USD \n width of box: proportional to squareroot of oberservations") +
  theme_light()

# bosxplots with 25% intervals
p + geom_boxplot(aes(x = cut_interval(`Access to electricity (% of population)`,
                                      n = 4, breaks = c(0,25,50,75,100),
                                      labels = c("0 - 25%", "25 - 50%", "50 - 75%", "75 - 100%"))),
                 varwidth = TRUE)


### boxplots with 20% intervals
p + geom_boxplot(aes(x = cut_interval(`Access to electricity (% of population)`,
                                      n = 5, 
                                      breaks = c(0, 20, 40, 60, 80, 100), 
                                      labels = c("0 - 20%", "20 - 40%", "40 - 60%", "60 - 80%", "80 - 100%"))),
                 varwidth = TRUE)

cor_p <- Worldbank %>%
  group_by(`Country Name`) %>%
  summarize(r_pearson = cor(x = `Access to electricity (% of population)`, 
                        y = `Adjusted net national income per capita (current US$)`,
                        method = "pearson",
                        use = "na.or.complete")) %>%
  filter(!is.na(r_pearson))

cor_p %>%
  ggplot(aes(x = r_pearson, y = forcats::fct_reorder(`Country Name`, r_pearson))) +
  geom_col(fill = "lightblue") +
  geom_vline(xintercept = 0, color = "red") +
  geom_vline(xintercept = 0.5, color = "red", linetype = "dashed") +
  guides(fill = "none") +
  labs(y = "",
       x = "Pearson correlation coefficient",
       title = "Correlation: Electricity - Net National Income",
       caption = "Correlation between Access to Electricity \nand Net National income per capita") +
  theme_light()

########### AREA
Worldbank %>%
  mutate(Surface_binned = cut(`Surface area (sq. km)`/ 100000, breaks = c(0, 2.5, 7.5, 12.5, Inf),
                              labels = c("0 - 0.25", "0.25 - 0.75", "0.75 - 1.25", "1.25+"))) %>%
  ggplot(aes(x = Surface_binned, y = `Adjusted net national income per capita (current US$)`)) +
  geom_boxplot(varwidth = TRUE) +
  theme_light() +
  labs(x = "million square km",
       y = "Net National income per capita",
       color = "Country",
       title = "National income vs Area")+
  scale_x_discrete(guide = guide_axis(angle = 30))+
  scale_y_log10()

########### POPULATION
Worldbank %>%
  ggplot(aes(x = `Population, total` / 1000000, color = `Country Name`)) +
  geom_point(aes(y = `Adjusted net national income per capita (current US$)`)) +
  theme_light() +
  labs(x = "",
       y = "",
       color = "Country",
       title = "National income vs Population",
       caption = "x: Population in Million Residents\ny: Adjusted Net national income per capita in current USD") +
  scale_x_continuous(guide = guide_axis(angle = 45)) +
  facet_wrap(vars(`Country Name`), scales = "free") +
  guides(color = "none")
