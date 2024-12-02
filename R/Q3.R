install.packages("ggplot2")
library(ggplot2)
install.packages("readxl")
library(readxl)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)

color_assigns <- readr::read_rds("Data/cleaned/Country_Colors.RDS")
colo <- country_colors$ColorHex
names(colo) <- country_colors$Country
colo

#How does the HIV prevalence in the population aged 15-49 
#relate to the total alcohol consumption per capita?
colnames(Worldbank)
result1 <- Worldbank %>% 
  filter(!is.na(`Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`) 
         & !is.na(`Prevalence of HIV, total (% of population ages 15-49)`)) %>% 
  select(`Country Name`, `Year`, `Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`, 
         `Prevalence of HIV, total (% of population ages 15-49)`)
result1

#keine facettierung
ggplot(result1, aes(x = `Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`, 
                    y = `Prevalence of HIV, total (% of population ages 15-49)`, 
                    color = `Country Name`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "red") +
  labs(title = "Alcohol Consumption vs. HIV Prevalence",
       x = "Total alcohol consumption ($)",
       y = "Prevalence of HIV (% of population ages 15-49)") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +
  scale_color_manual(values = colo) +
  scale_y_log10()

#facettierung nach Contry name
ggplot(result1, aes(x = `Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`, 
                    y = `Prevalence of HIV, total (% of population ages 15-49)`, 
                    color = `Country Name`)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
  facet_wrap(~ `Country Name`, scales = "free") +  
  labs(title = "Alcohol Consumption vs. HIV Prevalence",
       x = "Total alcohol consumption ($)",
       y = "Prevalence of HIV (% of population ages 15-49)") +
  theme_light() +
  scale_color_manual(values = colo) +
  scale_x_continuous(guide = guide_axis(angle = 45)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 2)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

#nur die Länder, die Änderung gibt
result1.1 <- Worldbank %>% 
  filter(!is.na(`Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`) 
         & !is.na(`Prevalence of HIV, total (% of population ages 15-49)`)
         & !(`Country Name` %in% c("Afghanistan", "Bangladesh", "Czechia", "New Zealand", "Qatar", "United States"))) %>% 
  select(`Country Name`, `Year`, `Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`, 
         `Prevalence of HIV, total (% of population ages 15-49)`) 

result1.1

ggplot(result1.1, aes(x = `Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`, 
                    y = `Prevalence of HIV, total (% of population ages 15-49)`, 
                    color = `Country Name`)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
  facet_wrap(~ `Country Name`, scales = "free") +  
  labs(title = "Alcohol Consumption vs. HIV Prevalence",
       x = "Total alcohol consumption ($)",
       y = "Prevalence of HIV (% of population ages 15-49)") +
  theme_light() +
  scale_color_manual(values = colo) +
  scale_x_continuous(guide = guide_axis(angle = 45)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 2)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

#average over time
result1.2 <- Worldbank %>% 
  filter(!is.na(`Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`) & !is.na(`Prevalence of HIV, total (% of population ages 15-49)`)) %>% 
  select(`Country Name`, `Year`, `Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`, `Prevalence of HIV, total (% of population ages 15-49)`) %>% 
  group_by(`Country Name`) %>% 
  summarise(
    avg_alcohol_con = mean(`Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`, na.rm = TRUE),
    avg_hiv_prevalence = mean(`Prevalence of HIV, total (% of population ages 15-49)`, na.rm = TRUE)
  )
result1.2

ggplot(result1.2, aes(x = `avg_alcohol_con`, 
                      y = `avg_hiv_prevalence`, 
                      color = `Country Name`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "red") +
  labs(title = "Alcohol Consumption vs. HIV Prevalence(average over time)",
       x = "alcohol consumption ($)",
       y = "Prevalence of HIV (% of population ages 15-49)") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +
  scale_color_manual(values = colo) +
  scale_y_log10()


#Do countries with higher percentage of labor force with basic education 
#have lower HIV prevalence rates in the 15-49 population?
colnames(Worldbank)

result2 <- Worldbank %>% 
  filter(!is.na(`Labor force with basic education (% of total working-age population with basic education)`) 
         & !is.na(`Prevalence of HIV, total (% of population ages 15-49)`)) %>%
  select(`Country Name`, `Year`, `Labor force with basic education (% of total working-age population with basic education)`, `Prevalence of HIV, total (% of population ages 15-49)`)                                                                                                       
result2

#keine facettierung
ggplot(result2, aes(x = `Labor force with basic education (% of total working-age population with basic education)`, 
                    y = `Prevalence of HIV, total (% of population ages 15-49)`, 
                    color = `Country Name`)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1),color = "red") +
  labs(title = "Labor force with basic education vs. HIV prevalence",
       x = "Labor force with basic education (%)",
       y = "Prevalence of HIV (% of population ages 15-49)") +
  scale_color_manual(values = colo) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +
  scale_y_log10()

#facettierung nach Country name
ggplot(result2, aes(x = `Labor force with basic education (% of total working-age population with basic education)`, 
                    y = `Prevalence of HIV, total (% of population ages 15-49)`, 
                    color = `Country Name`)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
  facet_wrap(~ `Country Name`, scales = "free") + 
  labs(title = "Labor force with basic education vs. HIV prevalence",
       x = "Labor force with basic education (%)",
       y = "Prevalence of HIV (% of population ages 15-49)") +
  scale_color_manual(values = colo) +
  theme_light() +
  scale_x_continuous(guide = guide_axis(angle = 45)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 2)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) 

#nur die Länder, die Änderung gibt
result2.1 <- Worldbank %>% 
  filter(!is.na(`Labor force with basic education (% of total working-age population with basic education)`) 
         & !is.na(`Prevalence of HIV, total (% of population ages 15-49)`)
         & !(`Country Name` %in% c("Afghanistan", "Bangladesh", "Chad", "Czechia", "New Zealand", "Pakistan", "Qatar", "United States"))) %>% 
  select(`Country Name`, `Year`, `Labor force with basic education (% of total working-age population with basic education)`, 
         `Prevalence of HIV, total (% of population ages 15-49)`) 

result2.1

ggplot(result2.1, aes(x = `Labor force with basic education (% of total working-age population with basic education)`, 
                    y = `Prevalence of HIV, total (% of population ages 15-49)`, 
                    color = `Country Name`)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
  facet_wrap(~ `Country Name`, scales = "free") + 
  labs(title = "Labor force with basic education vs. HIV prevalence",
       x = "Labor force with basic education (%)",
       y = "Prevalence of HIV (% of population ages 15-49)") +
  scale_color_manual(values = colo) +
  theme_light() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 2)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

#average over time
result2.2 <- Worldbank %>% 
  filter(!is.na(`Labor force with basic education (% of total working-age population with basic education)`) 
         & !is.na(`Prevalence of HIV, total (% of population ages 15-49)`)) %>% 
  select(`Country Name`, `Year`, `Labor force with basic education (% of total working-age population with basic education)`, 
         `Prevalence of HIV, total (% of population ages 15-49)`) %>% 
  group_by(`Country Name`) %>% 
  summarize(
    avg_laborforce = mean(`Labor force with basic education (% of total working-age population with basic education)`, na.rm = TRUE),
    avg_hiv_prevalence = mean(`Prevalence of HIV, total (% of population ages 15-49)`, na.rm = TRUE)
  )
result2.2

ggplot(result2.2, aes(x = `avg_laborforce`, 
                      y = `avg_hiv_prevalence`, 
                      color = `Country Name`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "red") +
  labs(title = "Labor force with basic education vs. HIV Prevalence(average over time)",
       x = "Labor force with basic education (%)",
       y = "Prevalence of HIV (% of population ages 15-49)") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 13)) +
  scale_color_manual(values = colo) +
  scale_y_log10()
  

