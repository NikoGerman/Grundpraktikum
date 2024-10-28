library(dplyr)
library(tidyr)
Worldbank1_raw <- readxl::read_excel("Data/raw/Worldbank1.xlsx")
Worldbank2_raw <- readxl::read_excel("Data/raw/Worldbank2.xlsx")

Worldbank1 <- Worldbank1_raw[1:208,] %>%
  mutate(across(`2023 [YR2023]`:average, as.numeric)) %>%
  select(-average, -`Series Code`) %>%
  pivot_longer(`2023 [YR2023]`:`2014 [YR2014]`, names_to = "Year", values_to = "data") %>%
  mutate(Year = as.numeric(substr(Year, start = 1, stop = 4))) %>%
  pivot_wider(names_from = "Series Name", values_from = "data")
  
Worldbank2 <- Worldbank2_raw[1:39,] %>%
  mutate(`2022 [YR2022]` = as.numeric(`2022 [YR2022]`),
         `2023 [YR2023]` = as.numeric(`2023 [YR2023]`)) %>%
  pivot_longer(`2014 [YR2014]`:`2023 [YR2023]`, names_to = "Year", values_to = "data") %>%
  mutate(Year = as.numeric(substr(Year, start = 1, stop = 4))) %>%
  select(-`Series Code`) %>%
  pivot_wider(names_from = "Series Name", values_from = "data")

Worldbank <- Worldbank1 %>%
  full_join(Worldbank2, by = c("Country Name" = "Country Name", "Country Code" = "Country Code", "Year" = "Year")) %>%
  mutate(across(c(`Country Name`, `Country Code`), ~as.factor(.x))) %>%
  mutate(Year = as.ordered(Year))

readr::write_rds(Worldbank, "Data/cleaned/Worldbank.RDS")