Q3 <- function() {
  #################
  #How does the HIV prevalence in the population aged 15-49 
  #relate to the total alcohol consumption per capita?
  #################
  Worldbank <- readRDS("Data/cleaned/Worldbank.RDS")
  country_colors <- readRDS("Data/cleaned/Country_Colors.rds")
  
  p0.1A <- Worldbank %>%
    group_by(Year, Country_Name) %>%
    summarize(Missing = is.na(`Total_alcohol_consumption_per_capita_(liters_of_pure_alcohol_projected_estimates_15+_years_of_age)`)) %>%
    ungroup() %>%
    mutate(Missing = factor(Missing, levels = c(TRUE, FALSE), labels = c("fehlt", "vorhanden"))) %>%
    ggplot(aes(x = Year, y = Country_Name)) +
    geom_tile(aes(fill = Missing), alpha = .7) +
    scale_fill_manual(values = c("fehlt" = "orange", "vorhanden" = "lightblue")) +
    labs(title = "Beobachtungen zum Alkoholkonsum", x = "Jahr", y = "Land", fill = "Beobachtung") +
    scale_x_discrete(guide = guide_axis(angle = 45), 
                     breaks = seq(2000, 2021, by = 5),
                     labels = seq(2000, 2021, by = 5)) +
    theme(#panel.grid.major = element_blank(),
      #panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "grey", linewidth = .25))
  
  p0.1B <- p0.1A %+%
    (Worldbank %>%
       group_by(Year, Country_Name) %>%
       summarize(Missing = is.na(`Prevalence_of_HIV_total_(%_of_population_ages_15-49)`)) %>%
       ungroup() %>%
       mutate(Missing = factor(Missing, levels = c(TRUE, FALSE), labels = c("fehlt", "vorhanden")))) +
    ggtitle("Beobachtungen zur HIV-Prävalenz")
  
  p0.1 <- (p0.1A | p0.1B) + plot_layout(axes = "collect", guides = "collect")
  
  p0.2 <- p0.1A %+%
    (Worldbank %>%
       group_by(Year, Country_Name) %>%
       summarize(Missing = is.na(`Prevalence_of_HIV_total_(%_of_population_ages_15-49)`) |
                   is.na(`Total_alcohol_consumption_per_capita_(liters_of_pure_alcohol_projected_estimates_15+_years_of_age)`)) %>%
       ungroup() %>%
       mutate(Missing = factor(Missing, levels = c(TRUE, FALSE), labels = c("fehlt", "vorhanden")))) +
    ggtitle("Beobachtungen zu HIV-Prävalenz und Alkoholkonsum pro Kopf")
  
  p1 <- Worldbank %>%
    ggplot(aes(x = `Total_alcohol_consumption_per_capita_(liters_of_pure_alcohol_projected_estimates_15+_years_of_age)`,
               y = `Prevalence_of_HIV_total_(%_of_population_ages_15-49)`,
               color = Country_Name)) +
    geom_point() +
    #geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "grey") +
    scale_color_manual(values = country_colors) +
    scale_y_log10(label = scales::label_number(suffix = "%")) +
    scale_x_continuous(label = scales::label_number(suffix = "l")) +
    labs(title = "Alkoholkonsum und HIV-Prävalenz",,
         x = "Alkoholkonsum pro Kopf",
         y = "HIV-Prävalenz",
         color = "Land")
  
  # view facetted per continent
  p1.1 <- p1 + facet_wrap(~Continent, ncol = 3) + 
    guides(color = "none") +
    geom_label_repel(data = Worldbank %>%
                       filter(!is.na(`Prevalence_of_HIV_total_(%_of_population_ages_15-49)`) & 
                                !is.na(`Total_alcohol_consumption_per_capita_(liters_of_pure_alcohol_projected_estimates_15+_years_of_age)`)) %>%
                       group_by(Country_Name) %>%
                       slice_max(order_by = Year),
                     aes(label = Country_Name), max.overlaps = 5, alpha = .65)
    #theme(legend.position='right')
  
  # view Asia
  p1.2 <- p1 %+% (Worldbank %>% 
            filter(Continent == "Asia") %>%
            group_by(Country_Name) %>%
            filter(mean(is.na(`Prevalence_of_HIV_total_(%_of_population_ages_15-49)`)) < 1) %>%
            ungroup()) +
    facet_wrap(~Country_Name) +
    guides(color = "none")
  
  # view Africa
  p1.3 <- p1 %+% (Worldbank %>% 
            filter(Continent == "Africa") %>%
            group_by(Country_Name) %>%
            filter(mean(is.na(`Prevalence_of_HIV_total_(%_of_population_ages_15-49)`)) < 1) %>%
            ungroup()) +
    facet_wrap(~Country_Name) +
    guides(color = "none")
  
  # view South America
  p1.4 <- p1 %+% (Worldbank %>% 
            filter(Continent == "South America") %>%
            group_by(Country_Name) %>%
            filter(mean(is.na(`Prevalence_of_HIV_total_(%_of_population_ages_15-49)`)) < 1) %>%
            ungroup()) +
    facet_wrap(~Country_Name) +
    guides(color = "none")
  
  # only countries without steady HIV-prevalence
  p1.5 <- p1 %+% (Worldbank %>%
            group_by(Country_Name) %>%
            filter(mean(is.na(`Prevalence_of_HIV_total_(%_of_population_ages_15-49)`)) < 1) %>%
            ungroup() %>%
            filter(!(Country_Name %in% c("Afghanistan", "Bangladesh", "Czechia", "New Zealand", "Qatar", "United States")))) +
    facet_wrap(~Country_Name) +
    guides(color = "none")
            
  # average over time
  p1.6 <- Worldbank %>%
    group_by(Country_Name) %>%
    summarise(avg_alkohol = mean(`Total_alcohol_consumption_per_capita_(liters_of_pure_alcohol_projected_estimates_15+_years_of_age)`, na.rm = TRUE),
              avg_HIV = mean(`Prevalence_of_HIV_total_(%_of_population_ages_15-49)`, na.rm = TRUE)) %>%
    ggplot(aes(x = avg_alkohol, y = avg_HIV, color = Country_Name)) +
    geom_point() +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "grey") +
    geom_text_repel(aes(label = Country_Name)) +
    scale_y_log10(label = scales::label_number(suffix = "%")) +
    scale_x_continuous(label = scales::label_number(suffix = "l")) +
    scale_color_manual(values = country_colors) +
    guides(color = "none") +
    labs(title = "durchschnittlicher Alkoholkonsum pro Kopf und HIV-Prevalenz pro Land",
         x = "Alkoholkonsum pro Kopf",
         y = "HIV-Prävalenz")
  
  p1.7 <- Worldbank %>%
    group_by(Country_Name) %>%
    filter(mean(is.na(`Prevalence_of_HIV_total_(%_of_population_ages_15-49)`)) < 1) %>%
    summarise(r_spearman = cor(`Total_alcohol_consumption_per_capita_(liters_of_pure_alcohol_projected_estimates_15+_years_of_age)`,
                               `Prevalence_of_HIV_total_(%_of_population_ages_15-49)`,
                               method = "spearman",
                               use = "complete.obs")) %>%
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
    geom_text_repel(aes(label = sprintf("%.2f", r_spearman), x = sign(r_spearman) * -.10), force = 0) +
    scale_x_continuous(limits = c(-1, 1)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = "grey", linewidth = .25))
  
  #################
  #Do countries with higher percentage of labor force with basic education 
  #have lower HIV prevalence rates in the 15-49 population?  
  #################
  
  # nur Länder mit vorhandenen HIV daten werden betrachtet
  p2 <- Worldbank %>%
    group_by(Country_Name) %>%
    filter(mean(`Prevalence_of_HIV_total_(%_of_population_ages_15-49)`, na.rm = TRUE) < 1) %>%
    ungroup() %>%
    ggplot(aes(x = `Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`,
               y = `Prevalence_of_HIV_total_(%_of_population_ages_15-49)`,
               color = Country_Name)) +
    geom_point() +
    #geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "grey") +
    scale_y_log10(label = scales::label_number(suffix = "%")) +
    scale_x_continuous(label = scales::label_number(suffix = "%")) +
    scale_color_manual(values = country_colors) +
    labs(title = "Bildungsquote und HIV-Prävalenz",
         x = "Bildungsquote",
         y = "HIV-Prävalenz",
         color = "Land")
  
  # faceted by Development status
  p2.1 <- p2 + facet_wrap(~Development_status)
  
  # faceted by Continent
  p2.2 <- p2 + facet_wrap(~Continent)
  
  # faceted by Country
  p2.3 <- p2 + facet_wrap(~Country_Name) +
    guides(color = "none")
  
  # faceted by COuntry, furthermore drop countries with constant prevalence
  p2.4 <- p2 %+% (Worldbank %>%
            filter(!(`Country_Name` %in% c("Afghanistan",
                                    "Bangladesh",
                                    "Czechia",
                                    "New Zealand",
                                    "Qatar",
                                    "United States"))) %>%
            group_by(Country_Name) %>%
            filter(mean(`Prevalence_of_HIV_total_(%_of_population_ages_15-49)`, na.rm = TRUE) < 1) %>%
            ungroup()) + 
    facet_wrap(~Country_Name) +
    guides(color = "none")
  
  p2.5 <- Worldbank %>%
    group_by(Country_Name) %>%
    summarise(avg_HIV = mean(`Prevalence_of_HIV_total_(%_of_population_ages_15-49)`, na.rm = TRUE),
              avg_edu = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
    ggplot(aes(x = avg_edu, y = avg_HIV, color = Country_Name)) +
    geom_point() +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "grey", linewidth = .75) +
    scale_color_manual(values = country_colors) +
    scale_y_continuous(label = scales::label_number(suffix = "%")) +
    scale_x_continuous(label = scales::label_number(suffix = "%")) +
    geom_text_repel(aes(label = Country_Name)) +
    guides(color = "none") +
    labs(title = "durchschnittliche Bildungsquote und durchschnittliche HIV-Prävalenz",
         x = "Bildungsquote",
         y = "HIV-Prävalenz")
  
  p2.6 <- Worldbank %>%
    group_by(Country_Name) %>%
    filter(mean(is.na(`Prevalence_of_HIV_total_(%_of_population_ages_15-49)`)) < 1) %>%
    summarise(r_spearman = cor(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`,
                               `Prevalence_of_HIV_total_(%_of_population_ages_15-49)`,
                               method = "spearman",
                               use = "complete.obs")) %>%
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
    geom_text_repel(aes(label = sprintf("%.2f", r_spearman), x = sign(r_spearman) * -.10), force = 0) +
    scale_x_continuous(limits = c(-1, 1)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = "grey", linewidth = .25)) %>%
    suppress_mw()
  
  # return(list(p1.1,
  #             p1.2,
  #             p1.3,
  #             p1.4,
  #             p1.5,
  #             p1.6,
  #             p1.7,
  #             p2.1,
  #             p2.2,
  #             p2.3,
  #             p2.4,
  #             p2.5,
  #             p2.6))
  
  return(list(p0.2,
              p1.1,
              p1.6,
              p1.7,
              p2.5))
}
