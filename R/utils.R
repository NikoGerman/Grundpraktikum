# ----------------------
# assign unambigious colors to countries
# ----------------------
assignCountryColors <- function(data) {
  # ----------------------
  # check properties of data
  # ----------------------
  checkmate::assertDataFrame(data, col.names = "named")
  checkmate::assertFactor(data$Country_Name)
  # ----------------------
  #   - extract the unique country_names
  #   - get a unique color of the viridis palette for each country
  # ----------------------
  unique_countries = levels(data$Country_Name)
  colors <- viridis::viridis(length(unique_countries), option = "D")
  names(colors) <- unique_countries
  return(colors)
}

# ----------------------
# function, which sppresses Messages and Warnings simultaniously
# ----------------------
suppress_mw <- function(expr) {
  suppressMessages(suppressWarnings(expr))
}
