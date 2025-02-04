Q1 <- function() {
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
  # load Data
  # ----------------------
  Worldbank <- readRDS("Data/cleaned/Worldbank.RDS")
  country_colors <- readRDS("Data/cleaned/Country_Colors.rds")
  
  # ----------------------
  # calculate spearman correlation between
  #   - Access to Electricity
  #   - Net National income p.c.
  # by country, only keep the non-NA values
  # ----------------------
  correlation <- Worldbank %>%
    group_by(Country_Name) %>%
    summarize(r_spearman = cor(x = `Access_to_electricity_(%_of_population)`, 
                               y = `Adjusted_net_national_income_per_capita_(current_US$)`,
                               method = "spearman",
                               use = "na.or.complete")) %>%
    filter(!is.na(r_spearman))
  
  # ----------------------
  # plot Missingness of Net National Income p.c.
  # ----------------------
  missingness <- Worldbank %>%
    group_by(Year, Country_Name) %>%
    summarize(Missing = is.na(`Adjusted_net_national_income_per_capita_(current_US$)`)) %>%
    ungroup() %>%
    mutate(Missing = factor(Missing, levels = c(TRUE, FALSE), labels = c("fehlt", "vorhanden"))) %>%
    ggplot(aes(x = Year, y = Country_Name)) +
    geom_tile(aes(fill = Missing), alpha = .7) +
    scale_fill_manual(values = c("fehlt" = "orange", "vorhanden" = "lightblue")) +
    labs(title = "Beobachtungen zu NNE pro Kopf", x = "Jahr", y = "Land", fill = "Beobachtung") +
    scale_x_discrete(guide = guide_axis(angle = 45), 
                     breaks = seq(2000, 2021, by = 5),
                     labels = seq(2000, 2021, by = 5)) +
    theme(#panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = "grey", linewidth = .25))
  
  # ----------------------
  #   - plot Access to Electricity vs Net National Income p.c.
  #   - color countries by using countrycolors
  # ----------------------
  plot1 <- Worldbank %>%
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
  plot2_basic <- ggplot(Worldbank,
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
    # geom_point(aes(x = `Access_to_electricity_(%_of_population)`, color = Country_Name), alpha = .7) +
    # geom_label_repel(data = Worldbank %>%
    #                    filter(!is.na(`Access_to_electricity_(%_of_population)`) &
    #                             !is.na(`Adjusted_net_national_income_per_capita_(current_US$)`)) %>%
    #                    slice_min(order_by = Year, by = Country_Name),
    #                    #slice_sample(n = 1, by = Country_Name),
    #                  aes(x = `Access_to_electricity_(%_of_population)`,
    #                      y = `Adjusted_net_national_income_per_capita_(current_US$)`,
    #                      color = Country_Name,
    #                      label = Country_Name),
    #                  size = 3,
    #                  max.overlaps = 10, force = .8) +
    # scale_color_manual(values = country_colors, guide = "none") +
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
    geom_label_repel(data = Worldbank %>%
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
  # ----------------------
  plot3 <- correlation %>%
    ggplot(aes(x = r_spearman, y = forcats::fct_reorder(`Country_Name`, r_spearman))) +
    geom_col(fill = "lightblue") +
    geom_vline(xintercept = 0, color = "red") +
    geom_vline(xintercept = 0.5, color = "red", linetype = "dashed") +
    geom_vline(xintercept = -0.5, color = "red", linetype = "dashed") +
    guides(fill = "none") +
    labs(y = "",
         x = "Korrelationskoeffizient",
         caption = "lediglich Länder mit durschnittlichem Grad der Elektrifizierung < 100%
       Verwendeter Korrellationskoeffizient: Spearman") +
    geom_text_repel(aes(label = sprintf("%.2f", round(r_spearman, 2)), x = sign(r_spearman) * -.10), force = 0) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = "grey", linewidth = .25)) +
    scale_x_continuous(limits = c(-1, 1))
  
  # ----------------------
  #   - build data for plot 4
  #   - calculate mean Population and Surface Area per Country
  #   - join the correlation data from before
  # ----------------------
  data_plot4 <- Worldbank %>%
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
                se = FALSE, 
                color = "grey",
                linewidth = .75) +
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

  
  # return(list(p1.1, 
  #             p1.2, 
  #             p1.3, 
  #             p2.1, 
  #             p2.2, 
  #             p3.1, 
  #             p3.2, 
  #             t1,
  #             p1.0,
  #             p0)) 
  
  
  # ----------------------
  # table containing the countries with mean Access to Electricity > 99%
  # ----------------------
  t1 <- Worldbank %>%
    group_by(Country_Name) %>%
    summarize("durchschnittliche Elektrifizierung" = mean(`Access_to_electricity_(%_of_population)`)) %>%
    arrange(desc(`durchschnittliche Elektrifizierung`)) %>%
    filter(`durchschnittliche Elektrifizierung` > 99) %>%
    rename(Land = Country_Name) %>%
    mutate(`durchschnittliche Elektrifizierung` = sprintf("%.1f%%", `durchschnittliche Elektrifizierung`))
  
  # ----------------------
  # return plots and table as named list
  # ----------------------
  return(list(
    missingness = missingness, 
    plot1 = plot1, 
    plot2 = plot2, 
    plot3 = plot3,
    plot4 = plot4,
    plot5 = plot5,
    Table1 = t1
    )
  )
}
