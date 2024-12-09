ReadData <- function() {
  Worldbank1_raw <- readxl::read_excel(path_Worldbank1)
  Worldbank2_raw <- readr::read_csv(path_Worldbank2)
  CO2_raw <- read.table(path_CO2, header = TRUE, sep = ";")
  Continents_raw <- readRDS(path_Continents)
  AdvancedEconomies <- readRDS(path_AdvancedEconomies)
  ### clean datasets ###
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
  # no usefull data after row 25
  CO2 <- CO2_raw[1:25, ] %>%
    select(-average, - Series.Code) %>%
    pivot_longer(`X2000..YR2000.`:`X2021..YR2021.`, names_to = "Year", values_to = "data") %>%
    mutate(Year = as.numeric(substr(Year, start = 2, stop = 5)))
  CO2$data <- gsub(pattern = "\\.", replacement = "", CO2$data)
  CO2$data <- gsub(pattern = "^0", replacement = "0.", CO2$data)
  CO2$data <- round(as.numeric(CO2$data), 2)
  CO2 <- CO2 %>%
    pivot_wider(names_from = "Series.Name", values_from = "data")
  Continents <- Continents_raw %>%
    select(Continent_Name, Three_Letter_Country_Code) %>%
    mutate(Continent = Continent_Name,
           Country_Code = Three_Letter_Country_Code,
           .keep = "none")
  # join dataframes
  Worldbank <- Worldbank1 %>%
    full_join(Worldbank2, by = c("Country Name" = "Country Name", "Country Code" = "Country Code", "Year" = "Year")) %>%
    full_join(CO2, by = c("Country Name" = "Country.Name", "Country Code" = "Country.Code", "Year" = "Year")) %>%
    mutate(across(c(`Country Name`, `Country Code`), ~as.factor(.x))) %>%
    mutate(Year = as.ordered(Year),
           Development_status = ifelse(`Country Name` %in% AdvancedEconomies, "developed", "emerging"))
  # clean names
  names(Worldbank) <- gsub(" ", "_", names(Worldbank))
  names(Worldbank) <- gsub("[,\\.]", "", names(Worldbank))
  #
  Worldbank <- Worldbank %>% 
    left_join(Continents, by = "Country_Code", relationship = "many-to-many")
  #
  return(Worldbank)
}
