Q2 <- function() {
  ### Question 2
  ### Do countries with higher central government debt as a percentage of GDP
  ### have a lower percentage of labor force with basic education?
  #Answer: No. It's the other way around.
  Worldbank <- readRDS("Data/cleaned/Worldbank.RDS")
  country_colors <- readRDS("Data/cleaned/Country_Colors.rds")
  
  p1 <- Worldbank %>%
    ggplot(aes(x = `Central_government_debt_total_(%_of_GDP)`,
               y = `Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`,
               color = Country_Name)) +
    geom_point(size = 2, alpha = .6) +
    geom_smooth(method = "lm", color = "grey", se = FALSE, linewidth = 0.75) +
    geom_label_repel(data = Worldbank %>% group_by(Country_Name) %>% slice_sample(n = 1) %>% ungroup(),
                    aes(label = Country_Name),
                    size = 3.5,
                    max.overlaps = 20,
                    force = .7,
                    alpha = 1) +
    labs(title = "Staatsverschuldung und Bildungsgrad der Arbeiterschicht",
         x = "Staatsverschuldung (in % des BIP)",
         y = "Anteil der Arbeiterschicht mit grundlegender Bildung (in %)") +
    scale_color_manual(values = country_colors) +
    guides(color = "none") +
    facet_wrap(~Development_status)
    
  
  # Are countries with higher percentage of labor force with basic education
  # able to maintain lower pupil-teacher ratios, and what impact might this have
  # on education quality?

  p2 <- Worldbank %>%
    ggplot(aes(x = Year,
               y = `Pupil-teacher_ratio_tertiary`,
               color = Country_Name,
               group = Country_Name)) +
    geom_line(linewidth = .7, na.rm = TRUE) +
    geom_point(size = 1.2) +
    geom_smooth(aes(group = 1), method = "lm", color = "grey", se = FALSE, linewidth = 0.75) +
    scale_color_manual(values = country_colors) +
    labs(x = "Jahr",
         y = "Schüler-Lehrer-Verhältnis",
         color = "Land") +
    scale_x_discrete(guide = guide_axis(angle = 45))
  
  p2.1 <- p2 %+% (Worldbank %>% 
    group_by(Country_Name) %>%
    filter(mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE) >=
             mean(Worldbank$`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
    ungroup()) +
    labs(title = "Schüler-Lehrer-Verhältnis der Länder überdurchschnittlich gebildeter Arbeiterschicht")
  
  top_5_countries <- Worldbank %>% group_by(Country_Name) %>%
    summarise(avg = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
    slice_max(order_by = avg, n = 5) %>%
    select(Country_Name) %>%
    pull()
  
  p2.2 <- p2 %+% (Worldbank %>% 
            filter(Country_Name %in% top_5_countries)
          ) + labs(title = "Schüler-Lehrer-Verhältnis der 5 Länder mit der durchschnittlich bestgebildesten Arbeiterschicht")

  return(list(p1, p2.1, p2.2))
}

