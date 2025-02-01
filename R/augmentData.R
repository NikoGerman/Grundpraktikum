augmentData <- function() {
  # ----------------------
  # load data
  # ----------------------
  data <- readRDS("Data/intermediate/Worldbank_intermediate.RDS")
  
  # ----------------------
  # assert properties of data
  # ----------------------
  checkmate::assertDataFrame(data, col.names = "named")
  checkmate::assertSubset(c("Access_to_electricity_(%_of_population)",
                 "Surface_area_(sq_km)",
                 "Population_total"),
               names(data)
               )
  
  # ----------------------
  # add features:
  #   a) access to electricity binned in 25% steps
  #   b) CO2 emissions per capita
  # ----------------------
  data <- data %>%
    mutate(
      electricity_binned = cut_interval(`Access_to_electricity_(%_of_population)`,
                                        n = 4, breaks = c(0, 25, 50, 75, 100),
                                        labels = c("0 - 25%", "25 - 50%", "50 - 75%", "75 - 100%")
                                        ),
      CO2_t_per_capita = `Carbon_dioxide_(CO2)_emissions_(total)_excluding_LULUCF_(Mt_CO2e)` * 1e6 / Population_total
    )
  
  # ----------------------
  # set country colors
  # ---------------------- 
  country_colors <- assignCountryColors(data)
  
  # ----------------------
  # save cleaned Dataset and Country Colors
  # ---------------------- 
  saveRDS(data, "Data/cleaned/Worldbank.RDS")
  saveRDS(country_colors, "Data/cleaned/Country_Colors.RDS")
}
