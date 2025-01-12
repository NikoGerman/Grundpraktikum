Q2 <- function() {
  ### Question 2
  ### Do countries with higher central government debt as a percentage of GDP
  ### have a lower percentage of labor force with basic education?
  #Answer: No. It's the other way around.
  Worldbank <- readRDS("Data/cleaned/Worldbank.RDS")
  country_colors <- readRDS("Data/cleaned/Country_Colors.rds")
  
  p0.1A <- Worldbank %>%
    group_by(Year, Country_Name) %>%
    summarize(Missing = is.na(`Central_government_debt_total_(%_of_GDP)`)) %>%
    ungroup() %>%
    mutate(Missing = factor(Missing, levels = c(TRUE, FALSE), labels = c("fehlt", "vorhanden"))) %>%
    ggplot(aes(x = Year, y = Country_Name)) +
    geom_tile(aes(fill = Missing), alpha = .7) +
    scale_fill_manual(values = c("fehlt" = "orange", "vorhanden" = "lightblue")) +
    labs(title = "Beobachtungen zur Staatsverschuldung", x = "Jahr", y = "Land", fill = "Beobachtung") +
    scale_x_discrete(guide = guide_axis(angle = 45), 
                     breaks = seq(2000, 2021, by = 5),
                     labels = seq(2000, 2021, by = 5)) +
    theme(#panel.grid.major = element_blank(),
      #panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "grey", linewidth = .25))
  
  p0.1B <- p0.1A %+% (Worldbank %>%
               group_by(Year, Country_Name) %>%
               summarize(Missing = is.na(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`)) %>%
               ungroup() %>%
               mutate(Missing = factor(Missing, levels = c(TRUE, FALSE), labels = c("fehlt", "vorhanden")))) +
    ggtitle("Beobachtungen zur Bildungsquote")
  
  p0.1 <- (p0.1A| p0.1B) + plot_layout(axes = "collect", guides = "collect")
  
  p0.2 <- p0.1A %+% (Worldbank %>%
               group_by(Year, Country_Name) %>%
               summarize(Missing = (is.na(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`) |
                                      is.na(`Central_government_debt_total_(%_of_GDP)`))) %>%
               ungroup() %>%
               mutate(Missing = factor(Missing, levels = c(TRUE, FALSE), labels = c("fehlt", "vorhanden")))) +
    ggtitle("Beobachtungen zu Staatsverschuldung und Bildungsquote")
  
  ###########
  #########
  ######
  
  p1.1 <- Worldbank %>%
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
  
  p1.2 <- Worldbank %>%
    group_by(Country_Name) %>%
    summarize(r_spearman = cor(x = `Central_government_debt_total_(%_of_GDP)`, 
                               y = `Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`,
                               method = "spearman",
                               use = "na.or.complete")) %>%
    filter(!is.na(r_spearman)) %>%
    ggplot(aes(x = r_spearman, y = forcats::fct_reorder(`Country_Name`, r_spearman))) +
    geom_col(fill = "lightblue") +
    geom_vline(xintercept = 0, color = "red") +
    geom_vline(xintercept = 0.5, color = "red", linetype = "dashed") +
    geom_vline(xintercept = -0.5, color = "red", linetype = "dashed") +
    guides(fill = "none") +
    labs(y = "",
         x = "Korrelationskoeffizient",
         caption = "Verwendeter Korrellationskoeffizient: Spearman") +
    geom_text_repel(aes(label = sprintf("%.2f", round(r_spearman, 2)), x = sign(r_spearman) * -.10), force = 0) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = "grey", linewidth = .25)) +
    scale_x_continuous(limits = c(-1, 1))
  
  p1.3A <- Worldbank %>%
    group_by(Country_Name) %>%
    summarise(mean_debt = mean(`Central_government_debt_total_(%_of_GDP)`, na.rm = TRUE),
              mean_education = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
    ungroup() %>%
    ggplot(aes(x = mean_debt,
               y = mean_education,
               color = Country_Name)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "grey") +
    geom_label_repel(aes(label = Country_Name)) +
    scale_color_manual(values = country_colors, guide = "none") +
    scale_x_continuous(label = scales::label_number(suffix = "%"), limits = c(20, 130)) +
    scale_y_continuous(label = scales::label_number(suffix = "%")) +
    labs(x = "Staatsverschuldung",
         y = "Bildungsquote")
  
  p1.3B <- p1.3A %+% (Worldbank %>%
    group_by(Country_Name) %>%
    summarise(mean_debt = mean(`Central_government_debt_total_(%_of_GDP)`, na.rm = TRUE),
              mean_education = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(Country_Name != "United Kingdom"))
  
  p1.3 <- (p1.3A | p1.3B) + 
    plot_layout(axes = "collect", guides = "collect") +
    plot_annotation(title = "Staatsverschuldung und Bildungsquote im Durchschnitt")
  
  # Are countries with higher percentage of labor force with basic education
  # able to maintain lower pupil-teacher ratios, and what impact might this have
  # on education quality?
  
  p2.1 <- Worldbank %>%
    group_by(Country_Name) %>%
    summarize(mean_ptr = mean(`Pupil-teacher_ratio_tertiary`, na.rm = TRUE),
              mean_bildungsquote = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
    ggplot(aes(x = mean_bildungsquote,
               y = mean_ptr,
               color = Country_Name)) +
    geom_point() +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "grey") +
    geom_label_repel(aes(label = Country_Name)) +
    scale_color_manual(values = country_colors, guide = "none") +
    scale_x_continuous(label = scales::label_number(suffix = "%")) +
    labs(x = "Bildungsquote",
         y = "Schüler-Lehrer-Verhältnis",
         title = "Bildungsquote und Schüler-Lehrer-Verhältnis im Durchschnitt")
  
  # Worldbank %>%
  #   ggplot(aes(y = `Pupil-teacher_ratio_tertiary`,
  #             x = `Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`,
  #             color = Country_Name)) +
  #   geom_point(alpha = .7) +
  #   geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "grey") +
  #   geom_label_repel(data = Worldbank %>%
  #                      filter(!is.na(`Pupil-teacher_ratio_tertiary`) &
  #                               !is.na(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`)) %>%
  #                      slice_max(Year, by = Country_Name),
  #                    aes(label = Country_Name)) +
  #   scale_color_manual(values = country_colors, guide = "none") +
  #   scale_x_continuous(label = scales::label_number(suffix = "%")) +
  #   labs(x = "Bildungsquote",
  #        y = "Schüler-Lehrer-Verhältnis")
  
  p2.2 <- Worldbank %>%
    filter(Country_Name %in% (Worldbank %>% 
                                group_by(Country_Name) %>%
                                summarize(m = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
                                slice_max(m, n = 5) %>%
                                select(Country_Name) %>%
                                pull())
    ) %>%
    ggplot(aes(x = Year,
           y = `Pupil-teacher_ratio_tertiary`,
           color = Country_Name,
           group = Country_Name)) +
    geom_path(linewidth = .7, na.rm = TRUE) +
    geom_point() +
    geom_smooth(aes(group = 1), method = "lm", color = "grey", se = FALSE, linewidth = 0.75) +
    geom_label_repel(data = Worldbank %>%
                       filter(Country_Name %in% (Worldbank %>% 
                                                   group_by(Country_Name) %>%
                                                   summarize(m = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
                                                   slice_max(m, n = 5) %>%
                                                   select(Country_Name) %>%
                                                   pull())
                       ) %>%
                       filter(!is.na(`Pupil-teacher_ratio_tertiary`)) %>%
                       slice_max(Year, by = `Country_Name`),
                     aes(label = `Country_Name`),
                     size = 3,
                     max.overlaps = 10) +
    scale_color_manual(values = country_colors, guide = "none") +
    labs(x = "Jahr",
         y = "Schüler-Lehrer-Verhältnis",
         color = "Land") +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    labs(title = "Schüler-Lehrer-Verhältnis der 5 Länder mit der höchsten Bildungsquote")
  
  # Worldbank %>%
  #   filter(Country_Name %in% (Worldbank %>% 
  #                               group_by(Country_Name) %>%
  #                               summarize(m = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
  #                               slice_max(m, n = 4) %>%
  #                               select(Country_Name) %>%
  #                               pull())
  #   ) %>%
  #   ggplot(aes(x = `Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`,
  #              y = `Pupil-teacher_ratio_tertiary`,
  #              color = Country_Name,
  #              group = Country_Name)) +
  #   geom_point() +
  #   geom_label_repel(aes(label = Year)) +
  #   scale_color_manual(values = country_colors, guide = "none") +
  #   labs(x = "Bildungsquote",
  #        y = "Schüler-Lehrer-Verhältnis",
  #        color = "Land") +
  #   scale_x_continuous(label = scales::label_number(suffix = "%")) +
  #   labs(title = "Schüler-Lehrer-Verhältnis der 5 Länder mit den bestgebildeten Erwerbspersonen") +
  #   facet_wrap(~Country_Name, scales = "free")
  
  return(list(p0.1,
              p0.2,
              p1.1,
              p1.2,
              p1.3,
              p2.1,
              p2.2))
}

