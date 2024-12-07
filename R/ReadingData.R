library(dplyr)
library(tidyr)
Worldbank1_raw <- readxl::read_excel("Data/raw/Worldbank1.xlsx")
Worldbank2_raw <- readr::read_csv("Data/raw/Worldbank2.csv")
CO2_raw <- read.table("Data/raw/Co2_emi_WB.csv", header = TRUE, sep = ";")

# no usefull data after row 350
Worldbank1 <- Worldbank1_raw[1:350,] %>%
  mutate(across(`2000 [YR2000]`:average, as.numeric)) %>%
  select(-average, -`Series Code`) %>%
  pivot_longer(`2000 [YR2000]`:`2021 [YR2021]`, names_to = "Year", values_to = "data") %>%
  mutate(Year = as.numeric(substr(Year, start = 1, stop = 4))) %>%
  pivot_wider(names_from = "Series Name", values_from = "data")

# no usefull data after row 75
Worldbank2 <- Worldbank2_raw[1:75,] %>%
  select(-average, -`Series Code`) %>%
  pivot_longer(`2000 [YR2000]`:`2021 [YR2021]`, names_to = "Year", values_to = "data") %>%
  mutate(Year = as.numeric(substr(Year, start = 1, stop = 4))) %>%
  pivot_wider(names_from = "Series Name", values_from = "data")

CO2 <- CO2_raw[1:25, ] %>%
  select(-average, - Series.Code) %>%
  pivot_longer(`X2000..YR2000.`:`X2021..YR2021.`, names_to = "Year", values_to = "data") %>%
  mutate(Year = as.numeric(substr(Year, start = 2, stop = 5))) 

CO2$data <- gsub(pattern = "\\.", replacement = "", CO2$data)
CO2$data <- gsub(pattern = "^0", replacement = "0.", CO2$data)
CO2$data <- round(as.numeric(CO2$data), 2)

CO2 <- CO2 %>%
  pivot_wider(names_from = "Series.Name", values_from = "data")
  
Worldbank <- Worldbank1 %>%
  full_join(Worldbank2, by = c("Country Name" = "Country Name", "Country Code" = "Country Code", "Year" = "Year")) %>%
  full_join(CO2, by = c("Country Name" = "Country.Name", "Country Code" = "Country.Code", "Year" = "Year")) %>%
  mutate(across(c(`Country Name`, `Country Code`), ~as.factor(.x))) %>%
  mutate(Year = as.ordered(Year))

readr::write_rds(Worldbank, "Data/cleaned/Worldbank.RDS")
