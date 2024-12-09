assignCountryColors <- function(data) {
  assertDataFrame(data, col.names = "named")
  #
  unique_countries = sort(unique(data$Country_Name))
  colors <- viridis(length(unique_countries), option = "D")
  names(colors) <- unique_countries
  # 
  return(colors)
}

suppress_mw <- function(expr) {
  suppressMessages(suppressWarnings(expr))
}
