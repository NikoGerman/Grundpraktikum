Q3 <- function() {
  #################
  #How does the HIV prevalence in the population aged 15-49 
  #relate to the total alcohol consumption per capita?
  #################
  Worldbank <- readRDS("Data/cleaned/Worldbank.RDS")
  country_colors <- readRDS("Data/cleaned/Country_Colors.rds")
  
  
  p1 <- Worldbank %>%
    ggplot(aes(x = `Total_alcohol_consumption_per_capita_(liters_of_pure_alcohol_projected_estimates_15+_years_of_age)`,
               y = `Prevalence_of_HIV_total_(%_of_population_ages_15-49)`,
               color = Country_Name)) +
    geom_point() +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "grey") +
    scale_color_manual(values = country_colors) +
    scale_y_log10() +
    labs(title = "Alkoholkonsum und HIV-Prävalenz",,
         x = "Gesamtkonsum reiner Alkohol pro Kopf",
         y = "HIV-Prävalenz (in %)",
         color = "Land")
  
  # view facetted per continent
  p1.1 <- p1 + facet_wrap(~Continent, ncol = 3) + 
    guides(color = "none")
  
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
    scale_y_log10() +
    scale_color_manual(values = country_colors) +
    guides(color = "none") +
    labs(title = "durchscnittlicher Alkoholkonsum und HIV-Prevalenz pro Land",
         x = "Gesamtkonsum reiner Alkohol pro Kopf",
         y = "HIV-Prävalenz (in %)")
  
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
          axis.line = element_line(color = "grey", linewidth = .25)) %>%
    suppress_mw()
  
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
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "grey") +
    scale_y_log10() +
    scale_color_manual(values = country_colors) +
    labs(title = "Bildungsquote der Arbeiterschicht und HIV-Prävalenz",
         x = "Arbeiterschicht mit grundlegender Bildung (in %)",
         y = "HIV-Prävalenz (in %)",
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
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "grey") +
    scale_color_manual(values = country_colors) +
    scale_y_log10() +
    geom_text_repel(aes(label = Country_Name)) +
    guides(color = "none") +
    labs(title = "durchschnittliche Bildungsquote und durchschnittliche HIV-Prävalenz",
         x = "Bildungsquote (in %)",
         y = "HIV-Prävalenz (in %)")
  
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
  
  return(list(p1.1,
              p1.2,
              p1.3,
              p1.4,
              p1.5,
              p1.6,
              p1.7,
              p2.1,
              p2.2,
              p2.3,
              p2.4,
              p2.5,
              p2.6))
}
