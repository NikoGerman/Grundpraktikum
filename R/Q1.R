library(dplyr)
library(tidyr)
library(ggplot2)
# load cleaned Worldbank Data, discard 2022 and 2023 observations, since
# the variables we are interested in, are incomplete in these two years.
Worldbank_Q1 <- readr::read_rds("Data/cleaned/Worldbank.RDS") %>%
  filter(Year < 2022) %>%
  select("Country Name":"Adjusted net national income per capita (current US$)", 
         "Surface area (sq. km)", "Population, total", "Population density (people per sq. km of land area)", "Pop.Index2014")

Plot_Electricity1 <- Worldbank_Q1 %>%
  ggplot(aes(y = `Adjusted net national income per capita (current US$)`, x = cut_interval(`Access to electricity (% of population)`, n = 4, breaks = c(0,25,50,75,100), labels = c("0 - 25%", "25 - 50%", "50 - 75%", "75 - 100%"))))+
  geom_point(aes(colour = `Country Code` == 'AFG')) +
  geom_boxplot()+
  scale_y_log10() +
  labs(x = "Access to electricity",
       y = "Net national income per capita",
       color = "Country",
       title = "National income vs Access to Electricity",
       caption = "x: Access to Electricity in %s\ny: Adjusted Net national income per capita in current USD") +
  scale_color_manual(labels = c("Rest of World", "Afghanistan"), values = c("darkgrey", "red"))+
  theme_light()
ggsave("Plots/Q1-1.png", Plot_Electricity1, device = "png")

Worldbank_Q1 %>%
  ggplot(aes(y = `Adjusted net national income per capita (current US$)`,
             x = cut_interval(`Access to electricity (% of population)`, n = 4, breaks = c(0,25,50,75,100), labels = c("0 - 25%", "25 - 50%", "50 - 75%", "75 - 100%"))))+
  geom_boxplot() +
  geom_point(data = Worldbank_Q1 %>% filter(`Country Code` == "AFG"), color = "red") +
  scale_y_log10() +
  #scale_color_manual(labels = c("Rest of World", "Afghanistan"), values = c("darkgrey", "red"))+
  ylab("Adj. net national income p.c. (current US$)") +
  xlab("Access to electricity") +
  theme_light()

Plot_Electricity_wo_AFG <- Worldbank_Q1 %>%
  filter(`Country Code` != "AFG") %>%
  ggplot(aes(y = `Adjusted net national income per capita (current US$)`, x = cut_interval(`Access to electricity (% of population)`, n = 4, breaks = c(0,25,50,75,100), labels = c("0 - 25%", "25 - 50%", "50 - 75%", "75 - 100%"))))+
  geom_boxplot()+
  scale_y_log10()+
  ylab("Adj. net national income p.c. (current US$)")+
  xlab("Access to electricity")+
  theme_light()

Worldbank_Q1 %>%
  group_by(`Country Name`) %>%
  summarize(r_pearson = cor(x = `Access to electricity (% of population)`, 
                        y = `Adjusted net national income per capita (current US$)`,
                        method = "pearson",
                        use = "na.or.complete"))

low_elec <- Worldbank_Q1 %>%
  group_by(`Country Code`) %>%
  summarize(elec = mean(`Access to electricity (% of population)`)) %>%
  filter(elec < 100) %>%
  pull(`Country Code`)

Worldbank_Q1 %>%
  filter(`Country Code` %in% low_elec) %>%
  ggplot(aes(y = `Adjusted net national income per capita (current US$)`, x = `Access to electricity (% of population)`))+
  geom_point(aes(color = `Country Name`), size = 1.5)+
  geom_smooth(method = "lm", se = FALSE, color = "grey")+
  geom_smooth(aes(group = `Country Code`, color = `Country Name`), method = "lm", se = FALSE)+
  scale_y_log10()+
  scale_color_brewer(type = "qual")+
  labs(x = "Access to electricity",
       y = "Net national income per capita",
       color = "Country",
       title = "National income vs Access to Electricity",
       caption = "x: Acces to Electricity in %s\ny: Adjusted Net national income per capita in current USD") +
  theme_light()

########### AREA
Worldbank_Q1 %>%
  mutate(`Surface area (million sq. km)` = `Surface area (sq. km)` / 1000000) %>%
  ggplot(aes(x = `Surface area (million sq. km)`, y = `Adjusted net national income per capita (current US$)`)) +
  geom_point(aes(color = `Country Name`)) +
  theme_light() +
  labs(x = "Area",
       y = "Net National income per capita",
       color = "Country",
       title = "National income vs Area",
       caption = "x: Surface Area in Million square km\ny: Adjusted Net national income per capita in current USD") +
  scale_y_log10() +
  scale_x_sqrt()

########### POPULATION
Worldbank_Q1 %>%
  filter(`Country Code` != "HKG") %>%
  ggplot(aes(x = `Population, total`, color = `Country Name`)) +
  geom_point(aes(y = `Adjusted net national income per capita (current US$)`)) +
  theme_light() +
  labs(x = "",
           y = "",
           color = "Country",
           title = "National income vs Population",
           caption = "x: Population in Million Residents\ny: Adjusted Net national income per capita in current USD") +
  scale_y_log10()

Worldbank_Q1 %>%
  filter(`Country Code` != "HKG") %>%
  ggplot(aes(x = `Population, total`, color = `Country Name`)) +
  geom_point(aes(y = `Adjusted net national income per capita (current US$)`)) +
  theme_light() +
  labs(x = "",
       y = "",
       color = "Country",
       title = "National income vs Population",
       caption = "x: Population in Million Residents\ny: Adjusted Net national income per capita in current USD") +
  facet_wrap(vars(`Country Name`), scales = "free") +
  guides(color = "none")
########### Population Indexed by 2014
Worldbank_Q1 %>%
  filter(`Country Code` != "HKG") %>%
  ggplot(aes(x = Pop.Index2014, color = `Country Name`)) +
  geom_point(aes(y = `Adjusted net national income per capita (current US$)`)) +
  theme_light() +
  labs(x = "",
       y = "",
       color = "Country",
       title = "National income vs Population",
       caption = "x: 100 = Population in the Year 2014\ny: Adjusted Net national income per capita in current USD") +
  scale_y_log10()

Worldbank_Q1 %>%
  filter(`Country Code` != "HKG") %>%
  ggplot(aes(x = Pop.Index2014, color = `Country Name`)) +
  geom_point(aes(y = `Adjusted net national income per capita (current US$)`)) +
  theme_light() +
  labs(x = "",
       y = "",
       color = "Country",
       title = "National income vs Population growth",
       caption = "x: Population Indexed to 2014 levels\ny: Adjusted Net national income per capita in current USD") +
  facet_wrap(vars(`Country Name`), scales = "free_y") +
  guides(color = "none")
########### POPULATION-DENSITY
Worldbank_Q1 %>%
  filter(`Country Code` != "HKG")  %>%
  ggplot(aes(x = `Population density (people per sq. km of land area)`, y = `Adjusted net national income per capita (current US$)`, colour = `Country Name`))+
  geom_point() +
  labs(y = "Net national income per capita",
       x = "Population Density",
       color = "Country",
       title = "National income vs Population Density",
       caption = "x: Residents per square km\ny: Adjusted Net national income per capita in current USD") +
  scale_y_log10() + 
  scale_x_log10() +
  theme_light()
