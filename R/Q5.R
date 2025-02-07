Q5 <- function(data, country_colors = NULL, save = FALSE) {
  # ----------------------
  # check properties of input
  # ----------------------
  checkmate::assertDataFrame(data, col.names = "named")
  checkmate::assertCharacter(country_colors, null.ok = TRUE)
  checkmate::assertFlag(save)
  
  # ----------------------
  # Question:
  #   - Is there a relationship between the percentage of agricultural land and
  #     CO2 emissions per capita across countries? 
  #   - Does the size of the surface area of the country play a role?
  # ----------------------
  
  # ----------------------
  # if not provided, generate country_colors
  # ----------------------
  if (is.null(country_colors)) {
    country_colors <- assignCountryColors(data)
  }
  
  # ----------------------
  #   data_summarized
  #     aggregate data:
  #       - mean aggricultural land
  #       - mean surface area
  #       - mean population
  #       - mean CO2 emissions per capita
  #       - r_spearman: correlation betwwen Agricuktural land and CO2 emissions per capita
  # ----------------------
  data_summarized <- data %>%
    group_by(Country_Name) %>%
    summarise(mean_agricultural_land = mean(`Agricultural_land_(%_of_land_area)`, na.rm = TRUE),
              mean_surface_area = mean(`Surface_area_(sq_km)`, na.rm = TRUE),
              mean_population = mean(Population_total, na.rm = TRUE),
              mean_co2_per_capita = mean(CO2_t_per_capita, na.rm = TRUE),
              r_spearman = cor(`Agricultural_land_(%_of_land_area)`, CO2_t_per_capita, method = "spearman")) %>%
    ungroup()
  
  # ----------------------
  #   plot1:
  #     show CO2 emission per capita over the years
  #       x: year
  #       y: CO2 emissions per capita
  #   faceted by country,
  #   y-axis is set to logarithmic scale
  # ----------------------
  plot1 <- data %>%
    filter(CO2_t_per_capita != 0) %>%
    ggplot(aes(y = CO2_t_per_capita, 
               x = Year,
               group = Country_Name)) +
    geom_path(color = "lightblue") +
    facet_wrap(~Country_Name) +
    scale_y_log10(label = scales::label_number(suffix = "t")) +
    scale_x_discrete(breaks = seq(2000, 2021, by = 5), labels = seq(2000, 2021, by = 5), guide = guide_axis(angle = 45)) +
    labs(x = "Jahr", y = "CO2 Emissionen pro Kopf")
  
  # ----------------------
  #   plot2: scatterplot
  #     x: % of agricultural land
  #     y: CO2 emissions per capita
  #   points are colored by country
  #   instead of a color guide, labels are getting used
  # ----------------------
  plot2 <- data %>%
    ggplot(aes(x = `Agricultural_land_(%_of_land_area)`,
               y = `CO2_t_per_capita`,
               color = Country_Name)) +
    geom_point(size = 1) +
    scale_color_manual(values = country_colors, guide = "none") +
    scale_x_continuous(labels = scales::label_number(suffix = "%")) +
    scale_y_continuous(labels = scales::label_number(suffix = "t")) +
    geom_label_repel(data = data %>% distinct(`Country_Name`, .keep_all = TRUE),
                    aes(label = `Country_Name`),
                    size = 3,
                    max.overlaps = 10) +
    labs(title = "Landwirtschaftliche Nutzfläche und CO2 Emissionen pro Kopf",
         x = "Landwirtschaftliche Nutzfläche",
         y = "CO2 Emissionen pro Kopf")
  
  # ----------------------
  #   plot3:
  #   - plot correlation of
  #     + Agricultural land
  #     + CO2 per capita
  #     by country
  #   - only show countries where correlation can be computed
  #   - for corr.plot() see utils.R
  # ----------------------
  plot3 <- corr.plot(data,
                     x1 = "Agricultural_land_(%_of_land_area)",
                     x2 = "CO2_t_per_capita") +
    ggtitle("Korrelation von landwirtschaftlicher Fläche und CO2 Emissionen")
  
  # ----------------------
  #   plot4: scatterplot
  #     x: mean % agricuktural land
  #     y: mean CO2 emission per capita
  #   by country
  #   again labels instead of a color guide
  #   y-axis is set to logarithmic scale
  # ----------------------
  plot4 <- data_summarized %>%
    ggplot(aes(x = mean_agricultural_land,
               y = mean_co2_per_capita,
               color = Country_Name)) +
    geom_point(size = 1.5) +
    geom_label_repel(data = data_summarized %>% 
                       distinct(`Country_Name`, .keep_all = TRUE),
                     aes(label = `Country_Name`),
                     size = 3,
                     max.overlaps = 10) +
    scale_color_manual(values = country_colors, guide = "none") +
    scale_x_continuous(labels = scales::label_number(suffix = "%")) +
    scale_y_log10(labels = scales::label_number(suffix = "t")) +
    labs(title = "Landwirtschaft und CO2-Emissionen",
         x = "durschnittlicher Anteil landwirtschaftlich genutzter Fläche",
         y = "durchschnittliche CO2-Emissionen pro Kopf")
  
  # ----------------------
  #   plot5: scatterplot
  #     x: surface area
  #     y: spearman-correlation between % agricultural land and CO2 emissions p.c.
  #   for each country
  #   x-axis is logarithmic
  #   labels instead of color guide
  # ----------------------
  plot5 <- data_summarized %>%
    ggplot(aes(x = mean_surface_area,
               y = r_spearman,
               color = Country_Name)) +
    geom_point(size = 1, alpha = .7) +
    geom_label_repel(aes(label = Country_Name)) +
    scale_color_manual(values = country_colors, guide = "none") +
    scale_x_log10(label = scales::label_number(suffix = " qkm"),
                  limits = c(1e4, 3e7)) +
    labs(x = "Gesamtfläche",
         y = "Korrelationskoeffizient",
         caption = "verwendeter Korrelationskoeffizient: Spearman")
  
  # ----------------------
  # save plots as named list
  # ----------------------
  result <- list(
    plot1 = plot1,
    plot2 = plot2,
    plot3 = plot3,
    plot4 = plot4,
    plot5 = plot5
  )
  
  # ----------------------
  # save Figures if flag is set TRUE
  # for save.figures, see utils
  # WARNING
  # save needs to be FALSE here, 
  # otherways it would create an infinity loop
  # ----------------------
  if (save) {
    save.figures(Q5, list(data = data,
                          country_colors = country_colors,
                          save = FALSE))
  }
  
  # ----------------------
  # return result
  # ----------------------
  return(result)
}
