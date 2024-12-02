library(dplyr)
library(tidyr)
library(ggplot2)

colo <- country_colors$ColorHex
names(colo) <- country_colors$Country

# load cleaned Worldbank Data, discard 2022 and 2023 observations, since
# the variables we are interested in, are incomplete in these two years.
Worldbank <- readr::read_rds("Data/cleaned/Worldbank.RDS") %>%
  mutate(elec_binned = cut_interval(`Access to electricity (% of population)`,
                                    n = 4, breaks = c(0,25,50,75,100),
                                    labels = c("0 - 25%", "25 - 50%", "50 - 75%", "75 - 100%")),
         elec_quartile = cut_interval(`Access to electricity (% of population)`,
                                    n = 4, 
                                    breaks = c(summary(Worldbank$`Access to electricity (% of population)`)[c(1, 2, 3, 5)], Inf),
                                    labels = c("Q1", "Q2", "Q3", "Q4")),
         surface_quartile = cut_interval(`Surface area (sq. km)`,
                                         n = 4,
                                         breaks = summary(Worldbank$`Surface area (sq. km)`)[c(1, 2, 3, 5, 6)],
                                         labels = c("Q1", "Q2", "Q3", "Q4")),
         population_quartile = cut_interval(`Population, total`,
                                            n = 4,
                                            breaks = summary(Worldbank$`Population, total`)[c(1, 2, 3, 5, 6)],
                                            labels = c("Q1", "Q2", "Q3", "Q4")))

p <- Worldbank %>% 
  ggplot(aes(y = `Adjusted net national income per capita (current US$)`))+
  scale_y_log10() +
  labs(y = "Net national income per capita",
       color = "Country",
       x = "Access to electricity",
       title = "National income vs Access to Electricity",
       caption = "x: Access to Electricity in %\ny: Adjusted Net national income per capita in current USD \n width of box: proportional to squareroot of oberservations") +
  theme_light()

E <- Worldbank %>%
  group_by(elec_binned) %>%
  summarize(count = n())

# bosxplots with 25% intervals
p + geom_boxplot(aes(x = elec_binned)) +
  ggrepel::geom_label_repel(data = E, aes(label = paste("n =", count),x = elec_binned, y = 120), direction = "y", force = 0)

# boxplots faceted for elec_binned (y) and surface_quartile (x)
p + geom_boxplot() +
  facet_grid(rows = vars(elec_binned), cols = vars(surface_quartile))

# boxplots faceted for elec_quartile (y) and surface_quartile (x)
p + geom_boxplot() +
  facet_grid(rows = vars(elec_quartile), cols = vars(surface_quartile))

# boxplots faceted for elec_binned (y) and population_quartile (x)
p + geom_boxplot() +
  facet_grid(rows = vars(elec_binned), cols = vars(population_quartile))

# scatterplot of access to electricity vs. net national income per capita
# faceted by surface_quartile (y), population_quartile (x)
p + geom_point(aes(x = `Access to electricity (% of population)`, color = `Country Name`)) +
  facet_grid(rows = vars(surface_quartile), cols = vars(population_quartile)) +
  scale_x_log10() +
  scale_color_manual(values = colo) +
  guides(color = "none")

# calculate correlation coefficients
cor_p <- Worldbank %>%
  group_by(`Country Name`) %>%
  summarize(r_pearson = cor(x = `Access to electricity (% of population)`, 
                        y = `Adjusted net national income per capita (current US$)`,
                        method = "pearson",
                        use = "na.or.complete")) %>%
  filter(!is.na(r_pearson))

Worldbank %>%
  select(`Country Name`, surface_quartile) %>%
  inner_join(cor_p) %>%
  ggplot(aes(x = r_pearson, y = forcats::fct_reorder(`Country Name`, r_pearson))) +
  geom_col(fill = "lightblue") +
  geom_vline(xintercept = 0, color = "red") +
  geom_vline(xintercept = 5, color = "red", linetype = "dashed") +
  guides(fill = "none") +
  labs(y = "",
       x = "Pearson correlation coefficient",
       title = "Correlation: Electricity - Net National Income",
       caption = "Correlation between Access to Electricity \nand Net National income per capita") +
  ggrepel::geom_text_repel(aes(label = round(r_pearson, 2), x = sign(r_pearson) * .15), force = 0) +
  theme_light() +
  facet_wrap(~surface_quartile)

p2 <- cor_p %>%
  ggplot(aes(x = r_pearson, y = forcats::fct_reorder(`Country Name`, r_pearson))) +
  geom_col(fill = "lightblue") +
  geom_vline(xintercept = 0, color = "red") +
  geom_vline(xintercept = 0.5, color = "red", linetype = "dashed") +
  guides(fill = "none") +
  labs(y = "",
       x = "Pearson correlation coefficient",
       title = "Correlation: Electricity - Net National Income",
       caption = "Correlation between Access to Electricity \nand Net National income per capita") +
  ggrepel::geom_text_repel(aes(label = round(r_pearson, 2), x = sign(r_pearson) * .15), force = 0) +
  theme_light()

p2

W_ <- Worldbank %>%
  group_by(`Country Name`) %>%
  summarize(avg_pop = mean(`Population, total`), surface = mean(`Surface area (sq. km)`)) %>%
  full_join(cor_p)

W_ %>%
  filter(`Country Name` !="Aruba") %>%
  filter(`Country Name` != "Afghanistan") %>%
  #filter(`Country Name` != "Russian Federation") %>%
  ggplot(aes(x = surface, y = r_pearson)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  #scale_x_sqrt() +
  scale_x_log10() +
  ggrepel::geom_text_repel(aes(label = `Country Name`), force = .5) +
  theme_minimal()

W_ %>%
  filter(`Country Name` !="Aruba") %>%
  filter(`Country Name` != "Afghanistan") %>%
  filter(`Country Name` !="China") %>%
  filter(`Country Name` != "India") %>%
  ggplot(aes(x = avg_pop, y = r_pearson)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  #scale_x_sqrt() +
  ggrepel::geom_text_repel(aes(label = `Country Name`), force = .5) +
  theme_minimal()

########### AREA
W <- Worldbank %>%
  mutate(Surface_binned = cut(`Surface area (sq. km)`/ 100000, breaks = c(0, 2.5, 7.5, 12.5, Inf),
                              labels = c("0 - 0.25", "0.25 - 0.75", "0.75 - 1.25", "1.25+"))) 
W. <- W %>%
  group_by(Surface_binned) %>%
  summarize(count = n(), `Adjusted net national income per capita (current US$)` = median(`Adjusted net national income per capita (current US$)`, na.rm = TRUE))

W %>%
  ggplot(aes(x = Surface_binned, y = `Adjusted net national income per capita (current US$)`)) +
  geom_boxplot() +
  theme_light() +
  labs(x = "million square km",
       y = "Net National income per capita",
       color = "Country",
       title = "National income vs Area") +
  scale_x_discrete(guide = guide_axis(angle = 30)) +
  ggrepel::geom_label_repel(data = W., aes(label = paste("n =", count), y = 150), direction = "y", force = 0) +
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


