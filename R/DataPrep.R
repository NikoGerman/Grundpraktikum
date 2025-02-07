DataPrep <- function(method = "save", colors = FALSE) {
  checkmate::assertCharacter(method, len = 1, any.missing = FALSE)
  checkmate::assertSubset(method, choices = c("save", "return"))
  checkmate::assertFlag(colors)
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
  CountryClassifications_raw <- read.csv(path_CountryClassifications, sep = ";")
  
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
  # clean Country Classification data:
  #   - keep Country Codes and Income Group
  #   - set labels to german
  #   - rename to fit within naming scheme
  # ----------------------
  CountryClassifications <- CountryClassifications_raw %>%
    select(Code, Income.group) %>%
    filter(Income.group != "") %>%
    mutate(Income.group = factor(Income.group, levels = c("High income",
                                                          "Upper middle income",
                                                          "Lower middle income",
                                                          "Low income"),
                                 labels = c("Hohes Volkseinkommen",
                                            "Volkseinkommen oberes Mittelfeld",
                                            "Volkseinkommen unteres Mittelfeld",
                                            "Niedriges Volkseinkommen"))) %>%
    rename("Country Code" = "Code",
           "Income Group" = "Income.group")
  
  # ----------------------
  # Join Data:
  #   - Worldbank1 and Worldbank2 gets joined on Country and year
  #   - CO2 data gets joined also on Country and year
  #   - Continent Classification is joined
  #   - Country Classifications gets joined
  #   - Country Name and -Code, as well as Continent get cast into factor variable
  #   - Year gets cast into ordered factor
  # ----------------------
  Worldbank <- Worldbank1 %>%
    full_join(Worldbank2, by = c("Country Name" = "Country Name", "Country Code" = "Country Code", "Year" = "Year")) %>%
    full_join(CO2, by = c("Country Name" = "Country.Name", "Country Code" = "Country.Code", "Year" = "Year")) %>%
    left_join(Continents, by = "Country Code", relationship = "many-to-many") %>%
    left_join(CountryClassifications, by = "Country Code") %>%
    mutate(across(c(`Country Name`, `Country Code`, Continent), ~as.factor(.x)),
           Year = as.ordered(Year))
  
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
  # add features:
  #   a) access to electricity binned in 25% steps
  #   b) CO2 emissions per capita
  # ----------------------
  Worldbank <- Worldbank %>%
    mutate(
      electricity_binned = cut_interval(`Access_to_electricity_(%_of_population)`,
                                        n = 4, breaks = c(0, 25, 50, 75, 100),
                                        labels = c("0 - 25%", "25 - 50%", "50 - 75%", "75 - 100%")
      ),
      CO2_t_per_capita = `Carbon_dioxide_(CO2)_emissions_(total)_excluding_LULUCF_(Mt_CO2e)` * 1e6 / Population_total
    )
  
  # ----------------------
  # set and save country colors if colors is TRUE
  # ---------------------- 
  if (colors) {
    country_colors <- assignCountryColors(Worldbank)
    # ----------------------
    # if directory does not yet exist, create it
    # ---------------------- 
    if (!dir.exists("Data/cleaned")) {
      dir.create("Data/cleaned")
    }
    saveRDS(country_colors, "Data/cleaned/Country_Colors.RDS")
    cat("Country Colors saved to <Data/cleaned/Country_Colors.RDS>\n")
  }
  
  # ----------------------
  # if "save" is chosen, Worldbank gets saved, else it gets returned
  # ---------------------- 
  if(method == "save") {
    # ----------------------
    # if directory does not yet exist, create it
    # ---------------------- 
    if (!dir.exists("Data/cleaned")) {
      dir.create("Data/cleaned")
    }
    # ----------------------
    # save cleaned Dataset and Country Colors
    # ---------------------- 
    saveRDS(Worldbank, "Data/cleaned/Worldbank.RDS")
    cat("Worldbank data saved to <Data/cleaned/Worldbank.RDS>\n")
  } else {
    return(Worldbank)
  }
}
