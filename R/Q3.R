Q3 <- function(data, country_colors = NULL, method = "save") {
  # ----------------------
  # check properties of input
  # ----------------------
  checkmate::assertDataFrame(data, col.names = "named")
  checkmate::assertCharacter(country_colors, null.ok = TRUE)
  checkmate::assertCharacter(method, len = 1, any.missing = FALSE)
  checkmate::assertSubset(method, choices = c("save", "return"))
  
  # ----------------------
  # Question:
  #   - How does the HIV prevalence in the population aged 15-49 
  #     relate to the total alcohol consumption per capita?
  #   - Do countries with higher percentage of labor force with basic education 
  #     have lower HIV prevalence rates in the 15-49 population?  
  # ----------------------
  
  # ----------------------
  # if not provided, generate country_colors
  # ----------------------
  if (is.null(country_colors)) {
    country_colors <- assignCountryColors(data)
  }
  
  # ----------------------
  # missingness:
  #   plot Missingness of 
  #     - HIV Prevalence
  #     - Total alcohol consumption
  #     by Country/Year.
  #   Label Missing if either one of the to features is missing
  #   - for plot.missing() see utils.R
  # ----------------------
  missingness <- data %>% plot.missing(x1 = "Prevalence_of_HIV_total_(%_of_population_ages_15-49)",
                             x2 = "Total_alcohol_consumption_per_capita_(liters_of_pure_alcohol_projected_estimates_15+_years_of_age)") +
    ggtitle("Beobachtungen zu HIV-Prävalenz und Alkoholkonsum pro Kopf")
  
  # ----------------------
  # plot1:
  #   - scatterplot of:
  #     x: mean Total alcohol consumption per Country
  #     y: mean HIV-prevalence per Country
  #   - y-axis is set to logarithmic scale
  #   - instead of a color guide, labels are used
  #   - a linear regression line is fitted
  # ----------------------
  plot1 <- data %>%
    group_by(Country_Name) %>%
    summarise(avg_alkohol = mean(`Total_alcohol_consumption_per_capita_(liters_of_pure_alcohol_projected_estimates_15+_years_of_age)`, na.rm = TRUE),
              avg_HIV = mean(`Prevalence_of_HIV_total_(%_of_population_ages_15-49)`, na.rm = TRUE)) %>%
    ggplot(aes(x = avg_alkohol, y = avg_HIV, color = Country_Name)) +
    geom_point() +
    geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "grey", alpha = .15) +
    geom_text_repel(aes(label = Country_Name)) +
    scale_y_log10(label = scales::label_number(suffix = "%")) +
    scale_x_continuous(label = scales::label_number(suffix = "l")) +
    scale_color_manual(values = country_colors, guide = "none") +
    labs(title = "durchschnittlicher Alkoholkonsum pro Kopf und HIV-Prevalenz pro Land",
         x = "Alkoholkonsum pro Kopf",
         y = "HIV-Prävalenz")  
  
  # ----------------------
  # plot2:
  #   - scatterplot of:
  #     x: Total alcohol consumption
  #     y: HIV-prevalence
  #   - y-axis is set to logarithmic scale
  #   - plot is faceted by continent
  #   - instead of a color guide, labels are used
  # ----------------------
  plot2 <- data %>%
    ggplot(aes(x = `Total_alcohol_consumption_per_capita_(liters_of_pure_alcohol_projected_estimates_15+_years_of_age)`,
               y = `Prevalence_of_HIV_total_(%_of_population_ages_15-49)`,
               color = Country_Name)) +
    geom_point() +
    geom_label_repel(data = data %>%
                       filter(!is.na(`Prevalence_of_HIV_total_(%_of_population_ages_15-49)`) & 
                                !is.na(`Total_alcohol_consumption_per_capita_(liters_of_pure_alcohol_projected_estimates_15+_years_of_age)`)) %>%
                       group_by(Country_Name) %>%
                       slice_max(order_by = Year),
                     aes(label = Country_Name), max.overlaps = 5, alpha = .65) +
    scale_color_manual(values = country_colors, guide = "none") +
    scale_y_log10(label = scales::label_number(suffix = "%")) +
    scale_x_continuous(label = scales::label_number(suffix = "l")) +
    facet_wrap(~Continent, ncol = 3) +
    labs(title = "Alkoholkonsum und HIV-Prävalenz",,
         x = "Alkoholkonsum pro Kopf",
         y = "HIV-Prävalenz",
         color = "Land")
  
  # ----------------------
  # plot3:
  #   - spearman correlation as columns:
  #     x: correlation between Total alcohol consumption and HIV prevalence
  #     y: Country
  #   - only Countries are shown, were correlation is computeable
  #   - for corr.plot() see utils.R
  # ----------------------
  plot3 <- data %>% corr.plot(x1 = "Prevalence_of_HIV_total_(%_of_population_ages_15-49)",
            x2 = "Total_alcohol_consumption_per_capita_(liters_of_pure_alcohol_projected_estimates_15+_years_of_age)")
  
  # ----------------------
  # plot4:
  #   - scatterplot of:
  #     x: mean education of labour force
  #     y: mean HIV prevalence
  #   - contrary to the plots before, y-axis is set NOT to logarithmic scale
  #     since the fitted regression line would not be clearly seen otherwise
  #   - instead of a color guide, labels are used
  #   - regression line is fitted
  # ----------------------
  plot4 <- data %>%
    group_by(Country_Name) %>%
    summarise(avg_HIV = mean(`Prevalence_of_HIV_total_(%_of_population_ages_15-49)`, na.rm = TRUE),
              avg_edu = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
    ggplot(aes(x = avg_edu, y = avg_HIV, color = Country_Name)) +
    geom_point() +
    geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "grey", linewidth = .75, alpha = .15) +
    scale_color_manual(values = country_colors, guide = "none") +
    scale_y_continuous(label = scales::label_number(suffix = "%")) +
    scale_x_continuous(label = scales::label_number(suffix = "%")) +
    geom_text_repel(aes(label = Country_Name)) +
    labs(title = "durchschnittliche Bildungsquote und durchschnittliche HIV-Prävalenz",
         x = "Bildungsquote",
         y = "HIV-Prävalenz")
  
  # ----------------------
  # save plots as named list
  # ----------------------
  result <- list(
    missingness = missingness,
    plot1 = plot1,
    plot2 = plot2,
    plot3 = plot3,
    plot4 = plot4
  )
  
  # ----------------------
  # save Figures if method is set to "save"
  # for save.figures, see utils
  # WARNING
  # method needs to be "return" here,
  # other ways it would create an infinity loop
  # ----------------------
  if (method == "save") {
    save.figures(Q3, list(data = data,
                          country_colors = country_colors,
                          method = "return"))
  } else {
    # ----------------------
    # return result
    # ----------------------
    return(result)
  }
}
