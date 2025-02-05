Q2 <- function() {
  # ----------------------
  # Question:
  #   - Do countries with higher central government debt as a percentage of GDP
  #     have a lower percentage of labor force with basic education?
  #   - Are countries with higher percentage of labor force with basic education
  #     able to maintain lower pupil-teacher ratios
  #       - and what impact might this have on education quality? 
  # ----------------------
  
  # ----------------------
  # load Data
  # ----------------------
  Worldbank <- readRDS("Data/cleaned/Worldbank.RDS")
  country_colors <- readRDS("Data/cleaned/Country_Colors.rds")

  # ----------------------
  # missingness_1:
  # plot Missingness of 
  #   - government debt
  #   - percentage of Labour force w/ basic education
  # by Country/Year. Label Missing if either one of the to features is missing
  #   - for plot.missing() see utils.R
  # ----------------------
  missingness_1 <- Worldbank %>% plot.missing(x1 = "Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)",
                             x2 = "Central_government_debt_total_(%_of_GDP)") +
    ggtitle("Beobachtungen zu Staatsverschuldung und Bildungsquote")
  
  # ----------------------
  # missingness_2:
  # plot Missingness of 
  #   - pupil/teacher ratio
  #   - percentage of Labour force w/ basic education
  # by Country/Year. Label Missing if either one of the to features is missing
  #   - for plot.missing() see utils.R
  # ----------------------
  missingness_2 <- Worldbank %>% plot.missing(x1 = "Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)",
                             x2 = "Pupil-teacher_ratio_tertiary") +
    ggtitle("Beobachtungen zu Bildungsquote und Schüler-Lehrer-Verhältnis")
  
  # ----------------------
  # plot1:
  #   - scatterplot of:
  #     x: central government debt
  #     y: percentage of Labour force w/ basic education
  #     faceted by country
  # ----------------------
  plot1 <- Worldbank %>%
    filter((!is.na(`Central_government_debt_total_(%_of_GDP)`) & 
              !is.na(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`))) %>%
    ggplot(aes(x = `Central_government_debt_total_(%_of_GDP)`,
               y = `Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`,
               color = Country_Name)) +
    geom_point(size = 2, alpha = .6) +
    labs(title = "Staatsverschuldung und Bildungsquote",
         x = "Staatsverschuldung",
         y = "Bildungsquote") +
    scale_color_manual(values = country_colors, guide = "none") +
    scale_x_continuous(label = scales::label_number(suffix = "%"),
                       guide = guide_axis(angle = 45),
                       limits = c(0, 225)) +
    scale_y_continuous(label = scales::label_number(suffix = "%")) +
    facet_wrap(~Country_Name, nrow = 2)
  
  # ----------------------
  # plot2:
  #   - spearman correlation as columns:
  #     x: correlation between government debt and education of labour force
  #     y: Country
  #   - for corr.plot() see utils.R
  # ----------------------
  plot2 <- Worldbank %>% corr.plot(x1 = "Central_government_debt_total_(%_of_GDP)",
            x2 = "Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)")
  
  
  # ----------------------
  # plot3_part1:
  #   - since threre are few complete pobservation pairs,
  #     government debt and education of labour force
  #     get aggregated by Country using the mean value of both features
  #   - mean values are plotted:
  #       x: gov debt
  #       y: education of labour force
  #   - additionally a regression line is fitted on global level
  # ----------------------
  plot3_part1 <- Worldbank %>%
    group_by(Country_Name) %>%
    summarise(mean_debt = mean(`Central_government_debt_total_(%_of_GDP)`, na.rm = TRUE),
              mean_education = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
    ungroup() %>%
    ggplot(aes(x = mean_debt,
               y = mean_education,
               color = Country_Name)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "grey", alpha = .15) +
    geom_label_repel(aes(label = Country_Name)) +
    scale_color_manual(values = country_colors, guide = "none") +
    scale_x_continuous(label = scales::label_number(suffix = "%"), limits = c(20, 130)) +
    scale_y_continuous(label = scales::label_number(suffix = "%"), limits = c(0, 80)) +
    labs(x = "Staatsverschuldung",
         y = "Bildungsquote")
  
  # ----------------------
  # plot3_part2:
  #   same as part_1, but the United Kingdom gets excluded, since its an outlier
  # ----------------------
  plot3_part2 <- plot3_part1 %+% (Worldbank %>%
    group_by(Country_Name) %>%
    summarise(mean_debt = mean(`Central_government_debt_total_(%_of_GDP)`, na.rm = TRUE),
              mean_education = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(Country_Name != "Vereinigtes Königreich"))
  
  # ----------------------
  # plot3:
  #   using patchwork, combine _part1 and _part2 side by side
  # ----------------------
  plot3 <- (plot3_part1 | plot3_part2) + 
    plot_layout(axes = "collect", guides = "collect") +
    plot_annotation(title = "Staatsverschuldung und Bildungsquote im Durchschnitt")
  
  # ----------------------
  # plot4:
  #   as for the situation with plot3, we cope with the missingness of data
  #   by aggregating both pupil-teacher-ratio and education of labour force
  #   by Country. Then computing a scatterplot
  #     x: mean education of labour force
  #     y: mean pupil teacher ratio
  #   Additionally we fit a regression line on a gloabl level
  # ----------------------
  plot4 <- Worldbank %>%
    group_by(Country_Name) %>%
    summarize(mean_ptr = mean(`Pupil-teacher_ratio_tertiary`, na.rm = TRUE),
              mean_bildungsquote = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
    ggplot(aes(x = mean_bildungsquote,
               y = mean_ptr,
               color = Country_Name)) +
    geom_point() +
    geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "grey", alpha = .15) +
    geom_label_repel(aes(label = Country_Name)) +
    scale_color_manual(values = country_colors, guide = "none") +
    scale_x_continuous(label = scales::label_number(suffix = "%")) +
    labs(x = "Bildungsquote",
         y = "Schüler-Lehrer-Verhältnis",
         title = "Bildungsquote und Schüler-Lehrer-Verhältnis im Durchschnitt")
  
  # ----------------------
  # plot4:
  #   select the five Countries with the highest mean value for
  #   education of labour force
  #   for those five countries, plot:
  #     x: year
  #     y: pupil-teacher-ratio
  #   and connect the dots by country
  #   additionally fit a regression line on global level
  # ----------------------
  plot5 <- Worldbank %>%
    filter(Country_Name %in% (Worldbank %>% 
                                group_by(Country_Name) %>%
                                summarize(m = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
                                slice_max(m, n = 5) %>%
                                select(Country_Name) %>%
                                pull())
    ) %>%
    filter(!is.na(`Pupil-teacher_ratio_tertiary`)) %>%
    ggplot(aes(x = Year,
           y = `Pupil-teacher_ratio_tertiary`,
           color = Country_Name,
           group = Country_Name)) +
    geom_path(linewidth = .7, na.rm = TRUE) +
    geom_point() +
    geom_smooth(aes(group = 1), method = "lm", color = "grey", se = TRUE, linewidth = 0.75, alpha = .15) +
    geom_label_repel(data = Worldbank %>%
                       filter(Country_Name %in% (Worldbank %>% 
                                                   group_by(Country_Name) %>%
                                                   summarize(m = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
                                                   slice_max(m, n = 5) %>%
                                                   select(Country_Name) %>%
                                                   pull())
                       ) %>%
                       filter(!is.na(`Pupil-teacher_ratio_tertiary`)) %>%
                       slice_min(Year, by = `Country_Name`),
                     aes(label = `Country_Name`),
                     size = 3,
                     max.overlaps = 10) +
    scale_color_manual(values = country_colors, guide = "none") +
    labs(x = "Jahr",
         y = "Schüler-Lehrer-Verhältnis",
         color = "Land") +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    labs(title = "Schüler-Lehrer-Verhältnis der 5 Länder mit der höchsten Bildungsquote")
  
  # ----------------------
  # return plots as named list
  # ----------------------
  return(list(
    missingness_1 = missingness_1,
    missingness_2 = missingness_2,
    plot1 = plot1,
    plot2 = plot2,
    plot3 = plot3,
    plot4 = plot4,
    plot5 = plot5
  ))
}

