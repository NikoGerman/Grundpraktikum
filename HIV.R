install.packages("ggplot2")
library(ggplot2)
install.packages("readxl")
library(readxl)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)

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

result1$Year <- as.factor(result1$Year)

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
  

plot(`Prevalence of HIV, total (% of population ages 15-49)`, `Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`, 
     main="Relationship between Alcohol Consumption and HIV Prevalence in Population Aged 15-49", 
     xlab="HIV Prevalence (%)", 
     ylab="Alcohol Consumption per Capita",
     pch=19, col=rgb(0, 0, 1, 0.5))

#Do countries with higher percentage of labor force with basic education 
#have lower HIV prevalence rates in the 15-49 population?