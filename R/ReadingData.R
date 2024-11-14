library(dplyr)
library(tidyr)
Worldbank1_raw <- readxl::read_excel("Data/raw/Worldbank1.xlsx")
Worldbank2_raw <- readr::read_csv("Data/raw/Worldbank2.csv")

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

Worldbank <- Worldbank1 %>%
  full_join(Worldbank2, by = c("Country Name" = "Country Name", "Country Code" = "Country Code", "Year" = "Year")) %>%
  mutate(across(c(`Country Name`, `Country Code`), ~as.factor(.x))) %>%
  mutate(Year = as.ordered(Year))

readr::write_rds(Worldbank, "Data/cleaned/Worldbank.RDS")
