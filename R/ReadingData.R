ReadData <- function() {
  # ----------------------
  # read datasets
  #   a) Worldbank 1
  #   b) Worldbank 2
  #   c) CO2
  #   d) Continents
  #   e) Advanced Economies
  # ---------------------- 
  Worldbank1_raw <- readxl::read_excel(path_Worldbank1)
  Worldbank2_raw <- readr::read_csv(path_Worldbank2, show_col_types = FALSE)
  CO2_raw <- read.table(path_CO2, header = TRUE, sep = ";")
  Continents_raw <- readRDS(path_Continents)
  AdvancedEconomies <- readRDS(path_AdvancedEconomies)
  
  # ----------------------
  # clean Worldbank1:
  #   - data ends after row 350
  #   - drop "Series Code" and "average"
  #   - cast Year columns into long format
  #   - cast Year and data into numeric
  #   - cast Series into wide format
  #   - suppress messages and warnings caused by 
  #     introduction of NAs by coercion to numeric
  # ----------------------
  Worldbank1 <- Worldbank1_raw[1:350,] %>%
    select(-average, -`Series Code`) %>%
    pivot_longer(`2000 [YR2000]`:`2021 [YR2021]`, names_to = "Year", values_to = "data") %>%
    mutate(Year = as.numeric(substr(Year, start = 1, stop = 4)), 
           data = as.numeric(data)) %>%
    pivot_wider(names_from = "Series Name", values_from = "data") %>%
    suppress_mw()

  # ----------------------
  # clean Worldbank2:
  #   - data ends after row 75
  #   - drop "Series Code" and "average"
  #   - cast Year columns into long format
  #   - cast Year into numeric
  #   - cast Series into wide format
  # ----------------------
  Worldbank2 <- Worldbank2_raw[1:75,] %>%
    select(-average, -`Series Code`) %>%
    pivot_longer(`2000 [YR2000]`:`2021 [YR2021]`, names_to = "Year", values_to = "data") %>%
    mutate(Year = as.numeric(substr(Year, start = 1, stop = 4))) %>%
    pivot_wider(names_from = "Series Name", values_from = "data")
  
  # ----------------------
  # clean CO2 data:
  #   - data ends after row 25
  #   - drop "Series Code" and "average"
  #   - cast Year columns into long format
  #   - clean reported data: lose thousand separators and leading zeros
  #   - cast data into numeric
  #   - cast Series into wide format
  # ----------------------
  CO2 <- CO2_raw[1:25, ] %>%
    select(-average, - Series.Code) %>%
    pivot_longer(`X2000..YR2000.`:`X2021..YR2021.`, names_to = "Year", values_to = "data") %>%
    mutate(Year = as.numeric(substr(Year, start = 2, stop = 5))) %>%
    mutate(data = gsub(pattern = "\\.", replacement = "", data)) %>%
    mutate(data = gsub(pattern = "^0", replacement = "0.", data)) %>%
    mutate(data = round(as.numeric(data), 2)) %>%
    pivot_wider(names_from = "Series.Name", values_from = "data")
  
  # CO2$data <- gsub(pattern = "\\.", replacement = "", CO2$data)
  # CO2$data <- gsub(pattern = "^0", replacement = "0.", CO2$data)
  # CO2$data <- round(as.numeric(CO2$data), 2)
  # CO2 <- CO2 %>%
  #   pivot_wider(names_from = "Series.Name", values_from = "data")
  
  # ----------------------
  # clean CO2 data:
  #   - keep Continent Name and Country Codes
  #   - Russia and Kazakhstan are in Europe and Asia,
  #   for ease of joining, they are only kept on one continent
  # ----------------------
  Continents <- Continents_raw %>%
    select(Continent_Name, Three_Letter_Country_Code) %>%
    mutate(Continent = Continent_Name,
           `Country Code` = Three_Letter_Country_Code,
           .keep = "none") %>%
    filter(!(Continent == "Europe" & `Country Code` %in% c("RUS", "KAZ")))
  
  # ----------------------
  # Join Data:
  #   - Worldbank1 and Worldbank2 gets joined on Country and year
  #   - CO2 data gets joined also on Country and year
  #   - Country Name and -Code get cast into factor variable
  #   - Year gets cast into ordered factor
  #   - Advanced Economies is a vector, containing developed countries
  #     - data of IMF: https://www.imf.org/en/Publications/WEO/weo-database/2023/April/groups-and-aggregates#ea
  #     - if a country is in that list, it gets the tag "Industrieland"
  #     - else "Schwellen-/Entwicklungsland"
  #   - Continent
  # ----------------------
  Worldbank <- Worldbank1 %>%
    full_join(Worldbank2, by = c("Country Name" = "Country Name", "Country Code" = "Country Code", "Year" = "Year")) %>%
    full_join(CO2, by = c("Country Name" = "Country.Name", "Country Code" = "Country.Code", "Year" = "Year")) %>%
    mutate(across(c(`Country Name`, `Country Code`), ~as.factor(.x))) %>%
    mutate(Year = as.ordered(Year),
           Development_status = ifelse(`Country Name` %in% AdvancedEconomies, "Industrieland", "Schwellen-/Entwicklungsland")) %>% 
    left_join(Continents, by = "Country Code", relationship = "many-to-many") %>%
    mutate(Continent = as.factor(Continent))
  
  # ----------------------
  # clean columnnames:
  #   - replace spaces with underscores ("_")
  #   - delete punctuation (".", ",")
  # ----------------------
  names(Worldbank) <- gsub(" ", "_", names(Worldbank))
  names(Worldbank) <- gsub("[,\\.]", "", names(Worldbank))
  
  # ----------------------
  # set Country Names to German
  # ----------------------
  levels(Worldbank$Country_Name) <- c(
    "Afghanistan", "Aruba", "Bangladesch", "Bolivien", "Brasilien",
    "Kambodscha", "Tschad", "VR China", "Tschechien", "Finnland",
    "Ghana", "Indien", "Kasachstan", "Mali", "Neuseeland",
    "Nigeria", "Pakistan", "Peru", "Katar", "Russland",
    "Tansania", "Thailand", "Vereinigtes Königreich", "Vereinigte Staaten", "Vietnam"
    )
  
  # ----------------------
  # set Continent to German
  # ----------------------
  levels(Worldbank$Continent) <- c(
    "Afrika", "Asien", "Europa", "Nordamerika", "Ozeanien", "Südamerika"
  )
  
  # ----------------------
  # save Data as RDS
  # ----------------------
  saveRDS(Worldbank, "Data/intermediate/Worldbank_intermediate.RDS")
}
