augmentData <- function() {
  data <- readRDS("Data/raw/Worldbank.RDS")
  assertDataFrame(data, col.names = "named")
  assertSubset(c("Access_to_electricity_(%_of_population)",
                 "Surface_area_(sq_km)",
                 "Population_total"),
               names(data)
               )
  ### add features
  data <- data %>%
    mutate(
      electricity_binned = cut_interval(`Access_to_electricity_(%_of_population)`,
                                        n = 4, breaks = c(0, 25, 50, 75, 100),
                                        labels = c("0 - 25%", "25 - 50%", "50 - 75%", "75 - 100%")
                                        ),
      electricity_quartiles = cut_interval(`Access_to_electricity_(%_of_population)`,
                                           n = 4, 
                                           breaks = c(summary(data$`Access_to_electricity_(%_of_population)`)[c(1, 2, 3, 5)], Inf),
                                           labels = c("1st_Q", "2nd_Q", "3rd_Q", "4th_Q")
                                           ),
      surface_quartiles = cut_interval(`Surface_area_(sq_km)`,
                                       n = 4, 
                                       breaks = c(summary(data$`Surface_area_(sq_km)`)[c(1, 2, 3, 5)], Inf),
                                       labels = c("1st_Q", "2nd_Q", "3rd_Q", "4th_Q")
                                       ),
      population_quartiles = cut_interval(`Population_total`,
                                          n = 4, 
                                          breaks = c(summary(data$`Population_total`)[c(1, 2, 3, 5)], Inf),
                                          labels = c("1st_Q", "2nd_Q", "3rd_Q", "4th_Q")
      ),
      CO2_t_per_capita = `Carbon_dioxide_(CO2)_emissions_(total)_excluding_LULUCF_(Mt_CO2e)` * 1e6 / Population_total
    )
  saveRDS(data, "Data/cleaned/Worldbank.RDS")
  country_colors <- assignCountryColors(data)
  saveRDS(country_colors, "Data/cleaned/Country_Colors.RDS")
}
