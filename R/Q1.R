##### Question 1 ######
##### Electricity and national income #####
# How does access to electricity correlate with adjusted net national income
# per capita across different countries? Does this also depend on the country
# size or the inhabitants?
#########################
library(dplyr)
library(ggplot2)

Worldbank <- readr::read_rds("Data/Worldbank.RDS")

summary(Worldbank)
str(Worldbank)

Worldbank %>%
  filter(is.na(`Access to electricity (% of population)`))

Worldbank_Q1 <- Worldbank %>%
  filter(Year < 2023) %>%
  select("Country Name":"Adjusted net national income per capita (current US$)", "Surface area (sq. km)", "Population, total")
    

Worldbank_Q1 %>%
  filter(Year < 2022) %>%
  ggplot(aes(x = `Access to electricity (% of population)`, y = `Adjusted net national income per capita (current US$)`)) +
  geom_point(color = "blue", alpha = .5, shape = "diamond", size = 2)+
  scale_y_log10()+
  facet_grid(rows = vars(Year))+
  theme_light()

Worldbank_Q1 %>%
  filter(Year < 2022) %>%
  ggplot(aes(x = `Access to electricity (% of population)`, y = `Adjusted net national income per capita (current US$)`)) +
  geom_point(color = "blue", alpha = .5, shape = "diamond", size = 2)+
  geom_line()+
  scale_y_log10()+
  facet_grid(cols = vars(`Country Code`))+
  theme_light()

Worldbank_Q1 %>%
  filter(Year == 2021) %>%
  ggplot(aes(x = `Access to electricity (% of population)`, y = `Adjusted net national income per capita (current US$)`)) +
  geom_point(color = "blue", alpha = .5, shape = "diamond", size = 2)+
  scale_y_log10()+
  theme_light()

Worldbank_Q1 %>%
  mutate(Electricity_75 = `Access to electricity (% of population)`>= 75) %>%
  ggplot(aes(y = `Adjusted net national income per capita (current US$)`, x = Electricity_75))+
  geom_boxplot() +
  scale_y_log10()

Plot_Electricity1 <- Worldbank_Q1 %>%
  ggplot(aes(y = `Adjusted net national income per capita (current US$)`, x = cut_interval(`Access to electricity (% of population)`, n = 4, breaks = c(0,25,50,75,100), labels = c("0 - 25%", "25 - 50%", "50 - 75%", "75 - 100%"))))+
  geom_point(aes(colour = `Country Code` == 'AFG')) +
  geom_boxplot()+
  scale_y_log10() +
  labs(color = "Country")+
  scale_color_manual(labels = c("Rest of World", "Afghanistan"), values = c("darkgrey", "red"))+
  ylab("Adj. net national income p.c. (current US$)")+
  xlab("Access to electricity")+
  theme_light()
ggsave("Plots/Q1-1.png", Plot_Electricity1, device = "png")

Plot_Electricity_wo_AFG <- Worldbank_Q1 %>%
  filter(`Country Code` != "AFG") %>%
  ggplot(aes(y = `Adjusted net national income per capita (current US$)`, x = cut_interval(`Access to electricity (% of population)`, n = 4, breaks = c(0,25,50,75,100), labels = c("0 - 25%", "25 - 50%", "50 - 75%", "75 - 100%"))))+
  geom_boxplot()+
  scale_y_log10()+
  ylab("Adj. net national income p.c. (current US$)")+
  xlab("Access to electricity")+
  theme_light()

#cut_interval(Worldbank_Q1$`Access to electricity (% of population)`,n = 4, breaks = c(0,25,50,75,100), labels = c("(0,25]", "(25,50]", "(50,75]", "(75,100]"))

Worldbank_Q1 %>%
  group_by(`Country Name`) %>%
  summarize(tau = cor(x = `Access to electricity (% of population)`, 
                        y = `Adjusted net national income per capita (current US$)`,
                        method = "pearson",
                        use = "na.or.complete"))

Worldbank_Q1 %>%
  filter(`Country Code` == "CZE") %>%
  ggplot(aes(y = `Adjusted net national income per capita (current US$)`, x = `Access to electricity (% of population)`))+
  geom_point()

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
  scale_color_brewer(type = "qual")
  ylab("Adj. net national income p.c. (current US$)")+
  xlab("Access to electricity")+
  theme_light()
