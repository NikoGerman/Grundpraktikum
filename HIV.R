install.packages("ggplot2")
library(ggplot2)
install.packages("readxl")
library(readxl)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)

color_assigns <- readr::read_rds("Data/cleaned/Country_Colors.RDS")
#How does the HIV prevalence in the population aged 15-49 
#relate to the total alcohol consumption per capita?
colnames(Worldbank)
HIV_R <- Worldbank %>%
  select(`Prevalence of HIV, total (% of population ages 15-49)`) %>%
  drop_na()
HIV_R

Alcohol_C <- Worldbank %>% 
  select(`Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`) %>% 
  drop_na()
Alcohol_C

result1 <- Worldbank %>% 
  filter(!is.na(`Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`) & !is.na(`Prevalence of HIV, total (% of population ages 15-49)`)) %>%
  select(`Country Name`, `Year`, `Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`, `Prevalence of HIV, total (% of population ages 15-49)`,
         )                                                                                                       
result1


#keine facet

ggplot(result1, aes(x = `Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`, 
                      y = `Prevalence of HIV, total (% of population ages 15-49)`, 
                      color = `Country Name`)) +
  geom_point() +

  labs(title = "Relationship between Alcohol Consumption and HIV Prevalence in Population Aged 15-49",
       x = "Total alcohol consumption ($)",
       y = "Prevalence of HIV (% of population ages 15-49)") +
  theme(plot.title = element_text(size = 10)) +
  scale_y_log10()

#facet nach Contry name
ggplot(result1, aes(x = `Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`, 
                    y = `Prevalence of HIV, total (% of population ages 15-49)`, 
                    color = `Country Name`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
  facet_wrap(~ `Country Name`, scales = "free") +

  labs(title = "Relationship between Alcohol Consumption and HIV Prevalence in Population Aged 15-49",
       x = "Total alcohol consumption ($)",
       y = "Prevalence of HIV (% of population ages 15-49)") +
  theme(plot.title = element_text(size = 10))
  

#facet nach year
ggplot(result1, aes(x = `Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`, 
                    y = `Prevalence of HIV, total (% of population ages 15-49)`, 
                    color = `Country Name`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
  facet_wrap(~ Year) +
  labs(title = "Relationship between Alcohol Consumption and HIV Prevalence in Population Aged 15-49",
       x = "Total alcohol consumption ($)",
       y = "Prevalence of HIV (% of population ages 15-49)") +
  theme(plot.title = element_text(size = 10))


#Do countries with higher percentage of labor force with basic education 
#have lower HIV prevalence rates in the 15-49 population?
colnames(Worldbank)

result2 <- Worldbank %>% 
  filter(!is.na(`Labor force with basic education (% of total working-age population with basic education)`) & !is.na(`Prevalence of HIV, total (% of population ages 15-49)`)) %>%
  select(`Country Name`, `Year`, `Labor force with basic education (% of total working-age population with basic education)`, `Prevalence of HIV, total (% of population ages 15-49)`,
  )                                                                                                       
result2


result2 <- result2 %>% 
  left_join(color_assigns, by = c("Country Name" = "Country"))
#facet nach year
ggplot(result2, aes(x = `Labor force with basic education (% of total working-age population with basic education)`, 
                    y = `Prevalence of HIV, total (% of population ages 15-49)`, 
                    color = `Country Name`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
  facet_wrap(~ Year) +
  labs(title = "Do ountries with higher percentage of labor force with basic education have lower HIV prevalence rates?",
       x = "Labor force with basic education (%)",
       y = "Prevalence of HIV (% of population ages 15-49)") +
  theme(plot.title = element_text(size = 8))


#facet nach country name
ggplot(result2, aes(x = `Labor force with basic education (% of total working-age population with basic education)`, 
                    y = `Prevalence of HIV, total (% of population ages 15-49)`, 
                    color = `Country Name`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
  facet_wrap(~`Country Name`, scales = "free") +
  labs(title = "Do countries with higher percentage of labor force with basic education have lower HIV prevalence rates?",
       x = "Labor force with basic education (%)",
       y = "Prevalence of HIV (% of population ages 15-49)") +
  theme(plot.title = element_text(size = 8))

#Has healthcare per capita spending increased over time for each country, 
#and has it resulted in lower HIV infection rates?
result3 <- Worldbank %>%
  filter(!is.na(`Current health expenditure per capita (current US$)`) & !is.na(`Prevalence of HIV, total (% of population ages 15-49)`)) %>%
  select(`Country Name`, `Year`, `Current health expenditure per capita (current US$)`, `Prevalence of HIV, total (% of population ages 15-49)`)                                                                                                       
result3


#facet nach year
ggplot(result3, aes(x = `Current health expenditure per capita (current US$)`, 
                    y = `Prevalence of HIV, total (% of population ages 15-49)`, 
                    color = `Country Name`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
  facet_wrap(~ Year) +
  labs(title = "Healthcare spending increased resulted in lower HIV infection rates?",
       x = "Current health expenditure(US$)",
       y = "Prevalence of HIV (% of population ages 15-49)") +
  theme(plot.title = element_text(size = 10))

#facet nach country name
ggplot(result3, aes(x = `Current health expenditure per capita (current US$)`, 
                    y = `Prevalence of HIV, total (% of population ages 15-49)`, 
                    color = `Country Name`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
  facet_wrap(~ `Country Name`, scales = "free") +
  labs(title = "Healthcare spending increased resulted in lower HIV infection rates?",
       x = "Current health expenditure(US$)",
       y = "Prevalence of HIV (% of population ages 15-49)") +
  theme(plot.title = element_text(size = 10))

#Adjusted net national income per capita (current US$）with Alcohol consumption
colnames(Worldbank)
result4 <- Worldbank %>%
  filter(!is.na(`Adjusted net national income per capita (current US$)`) & !is.na(`Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`)) %>%
  select(`Country Name`, `Year`, `Adjusted net national income per capita (current US$)`, `Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`)                                                                                                       
result4

#散点图
ggplot(result4, aes(x = `Adjusted net national income per capita (current US$)`, 
                    y = `Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`, 
                    color = `Country Name`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
  facet_wrap(~ `Country Name`, scales = "free") +
  labs(title = "Relationship between National income and alcohol counsumption?",
       x = "National income per capita (current US$)(US$)",
       y = "Total alcohol consumption ($)") +
  theme(plot.title = element_text(size = 10)) +
  scale_x_continuous(guide = guide_axis(angle = 45))

#折线图
Alcohol <- Worldbank %>% 
  select()
Anteil <- 
ggplot(result4, aes(x = `Adjusted net national income per capita (current US$)`, 
                    y = `Alcohol consumption, 
                    color = `Country Name`)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
  facet_wrap(~ `Country Name`, scales = "free") +
  labs(title = "Relationship between National income and alcohol counsumption?",
       x = "National income per capita (current US$)(US$)",
       y = "Prevalence of HIV (% of population ages 15-49)") +
  theme(plot.title = element_text(size = 10)) +
  scale_x_continuous(guide = guide_axis(angle = 45))