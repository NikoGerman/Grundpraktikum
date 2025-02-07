Q1 <- function(data, country_colors = NULL, save = FALSE) {
  # ----------------------
  # check properties of input
  # ----------------------
  checkmate::assertDataFrame(data, col.names = "named")
  checkmate::assertCharacter(country_colors, null.ok = TRUE)
  checkmate::assertFlag(save)
  
  # ----------------------
  # Question:
  #   - Welcher Zusammenhang besteht zwischen der Stromversorgung und 
  #     dem bereinigten Netto-Pro-Kopf-Einkommen in verschiedenen Ländern?
  #   - Hängt dies auch 
  #       - von der Fläche oder 
  #       - der Bevölkerung
  #     eines Landes ab?
  # ----------------------
  
  # ----------------------
  # if not provided, generate country_colors
  # ----------------------
  if (is.null(country_colors)) {
    country_colors <- assignCountryColors(data)
  }
  
  # ----------------------
  # calculate spearman correlation between
  #   - Access to Electricity
  #   - Net National income p.c.
  # by country, only keep the non-NA values
  # ----------------------
  correlation <- data %>%
    group_by(Country_Name) %>%
    summarize(r_spearman = cor(x = `Access_to_electricity_(%_of_population)`, 
                               y = `Adjusted_net_national_income_per_capita_(current_US$)`,
                               method = "spearman",
                               use = "na.or.complete")) %>%
    filter(!is.na(r_spearman))
  
  # ----------------------
  # plot Missingness of Net National Income p.c.
  #   - for plot.missing() see utils.R
  # ----------------------
  missingness <- data %>% plot.missing(x1 = "Adjusted_net_national_income_per_capita_(current_US$)") +
    ggtitle("Beobachtungen zu NNE pro Kopf")
  
  # ----------------------
  #   - plot Access to Electricity vs Net National Income p.c.
  #   - color countries by using countrycolors
  # ----------------------
  plot1 <- data %>%
    ggplot(aes(x = `Access_to_electricity_(%_of_population)`,
               y = `Adjusted_net_national_income_per_capita_(current_US$)`,
               color = Country_Name)) +
    geom_point() +
    scale_x_continuous(label = scales::label_number(suffix = "%")) +
    scale_y_log10(label = scales::label_number(suffix = "$")) +
    scale_color_manual(values = country_colors) +
    labs(x = "Zugang zu Elektrizität",
         y = "NNE pro Kopf",
         color = "Land")

  # ----------------------
  #   - create basic plot with Net National Income p.c on y-axis
  #   - y-axis is logarithmic
  # ----------------------
  plot2_basic <- ggplot(data,
              aes(y = `Adjusted_net_national_income_per_capita_(current_US$)`)
              )+
    scale_y_log10(labels = scales::label_number(suffix = "$")) +
    labs(y = "NNE pro Kopf")
  
  # ----------------------
  #   - use plot2_basic and add Access to Elektricity on x-Axis
  #   - scatterplot
  #   - facetting by continent
  #   - countries get colored by countryColors
  # ----------------------
  plot2_part1 <- plot2_basic + 
    geom_point(aes(x = `Access_to_electricity_(%_of_population)`), color = "grey", alpha = .7) +
    facet_wrap(~Continent, ncol = 2) +
    scale_x_continuous(label = scales::label_number(suffix = "%"),
                       guide = guide_axis(angle = 45)) +
    labs(x = "Zugang zu Elektrizität",
         color = "Land") +
    theme(panel.grid.minor = element_blank())
  
  # ----------------------
  #   - use plot2_basic and add electricity binned on x-Axis
  #   - histogram per bin
  #   - number of obervations per bin gets calculated and is added as label
  # ----------------------
  plot2_part2 <- plot2_basic + geom_boxplot(aes(x = electricity_binned, group = electricity_binned)) +
    geom_label_repel(data = data %>%
                       group_by(electricity_binned) %>%
                       summarize(count = n()),
                     aes(label = paste("n =", count),
                         x = electricity_binned, y = 100), 
                     direction = "y", force = 0
                     ) +
    labs(x = "Zugang zu Elektrizität")+
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
  
  # ----------------------
  #   - using patchwork, combine _part1 and _part2 to plot2
  #   - y axis (Net National Income p.c.) gets collected
  # ----------------------
  plot2 <- (plot2_part1 | plot2_part2) + plot_layout(axis = "collect")
  
  # ----------------------
  # plot 3
  #   - plot correlation by country
  #   - for corr.plot() see utils.R
  # ----------------------
  plot3 <- data %>% corr.plot(x1 = "Access_to_electricity_(%_of_population)",
            x2 = "Adjusted_net_national_income_per_capita_(current_US$)")
 
  # ----------------------
  #   - build data for plot 4
  #   - calculate mean Population and Surface Area per Country
  #   - join the correlation data from before
  # ----------------------
  data_plot4 <- data %>%
    group_by(`Country_Name`) %>%
    summarize(avg_pop = mean(`Population_total`), surface = mean(`Surface_area_(sq_km)`)) %>%
    full_join(correlation)
  
  # ----------------------
  # plot4_part1
  #   - plot surface vs correlation of Access to Electricity and Net National Income p.c
  #   - as scatterplot, each point represents a country
  #   - add regression line using simple linear regression ("lm")
  #   - label points with the name of the respective country
  #   - x has a logarithmic scale
  # ----------------------
  plot4_part1 <- data_plot4 %>%
    ggplot(aes(x = surface, y = r_spearman, color = Country_Name)) +
    scale_x_log10(limits = c(1e2, 1e8), labels = scales::label_number(suffix = " qkm")) +
    scale_color_manual(values = country_colors, guide = "none") +
    geom_point() +
    geom_smooth(method = "lm",
                se = TRUE, 
                color = "grey",
                linewidth = .75,
                alpha = .15) +
    geom_text_repel(aes(label = `Country_Name`), force = 1) +
    ylim(-.5, 1.25) +
    labs(y = "Korrellationskoeffizient")  +
    labs(x = "Landesfläche")
  
  # ----------------------
  # plot4_part2
  #   - same as part1, but without Aruba, since its small size has huge impact
  # ----------------------
  plot4_part2 <- plot4_part1 %+%
    (data_plot4 %>%
       filter(Country_Name != "Aruba"))
  
  # ----------------------
  # plot4
  #   - using patchwork, combine part1 and part2
  #   - collect the x and y axis
  # ----------------------
  plot4 <- (plot4_part1 / plot4_part2) +
    plot_layout(axes = "collect") +
    plot_annotation(caption = "Verwendeter Korrellationskoeffizient: Spearman")
  
  # ----------------------
  # plot5_part1
  #   - same as plot4_part1, but population instead of surface on x-axis
  # ----------------------
  plot5_part1 <- plot4_part1 %+% aes(x = avg_pop/1000000) %+%
    scale_x_log10(limits = c(5e-2, 3e3), labels = scales::label_number(suffix = " Mio")) +
    labs(x = "Bevölkerung")
  
  # ----------------------
  # plot5_part2
  #   - same as plot4_part2, but population instead of surface on x-axis
  # ----------------------
  plot5_part2 <- plot4_part2 %+% aes(x = avg_pop/1000000) %+%
    scale_x_log10(limits = c(5e-2, 3e3), labels = scales::label_number(suffix = " Mio")) +
    labs(x = "Bevölkerung")
  
  # ----------------------
  # plot5
  #   - using patchwork, combine part1 and part2
  #   - collect the x and y axis
  # ----------------------
  plot5 <- (plot5_part1 / plot5_part2) +
    plot_layout(axes = "collect") +
    plot_annotation(caption = "Verwendeter Korrellationskoeffizient: Spearman")
  
  # ----------------------
  # table containing the countries with mean Access to Electricity > 99%
  # ----------------------
  t1 <- data %>%
    group_by(Country_Name) %>%
    summarize("durchschnittliche Elektrifizierung" = mean(`Access_to_electricity_(%_of_population)`)) %>%
    arrange(desc(`durchschnittliche Elektrifizierung`)) %>%
    filter(`durchschnittliche Elektrifizierung` > 99) %>%
    rename(Land = Country_Name) %>%
    mutate(`durchschnittliche Elektrifizierung` = sprintf("%.1f%%", `durchschnittliche Elektrifizierung`))
  
  # ----------------------
  # save plots and table as named list
  # ----------------------
  result <- list(
    missingness = missingness, 
    plot1 = plot1, 
    plot2 = plot2, 
    plot3 = plot3,
    plot4 = plot4,
    plot5 = plot5,
    Table1 = t1
  )
  
  # ----------------------
  # save Figures if flag is set TRUE
  # for save.figures, see utils
  # WARNING
  # save needs to be FALSE here, 
  # otherways it would create an infinity loop
  # ----------------------
  if (save) {
    save.figures(Q1, list(data = data,
                          country_colors = country_colors,
                          save = FALSE))
  }
  
  # ----------------------
  # return result
  # ----------------------
  return(result)
}
