Q2 <- function() {
  ### Question 2
  ### Do countries with higher central government debt as a percentage of GDP
  ### have a lower percentage of labor force with basic education?
  #Answer: No. It's the other way around.
  
  # Load the datasets
  Worldbank <- readRDS("Data/cleaned/Worldbank.RDS")
  country_colors <- readRDS("Data/cleaned/Country_Colors.rds")
  
  # Filter out rows with NA in key variables
  Worldbank_q2 <- Worldbank %>%
    filter(
      !is.na(`Central_government_debt_total_(%_of_GDP)`) &
        !is.na(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`)
    )
  
  # Calculate mean debt per country and categorize Debt_Level
  Worldbank_q2 <- Worldbank_q2 %>%
    group_by(Country_Name) %>%
    mutate(
      mean_Debt = mean(`Central_government_debt_total_(%_of_GDP)`, na.rm = TRUE),
      Debt_Level = ifelse(mean_Debt < 75, "Durchschnittliche Verschuldung < 75%", "Durchschnittliche Verschuldung >= 75%")
    ) %>%
    ungroup()
  
  # Summarize data by country for the linear model and mean points
  country_means <- Worldbank_q2 %>%
    group_by(Country_Name) %>%
    summarize(
      Mean_Debt = mean(`Central_government_debt_total_(%_of_GDP)`, na.rm = TRUE),
      Mean_Education = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE),
      Debt_Level = first(Debt_Level)
    ) %>%
    ungroup()
  
  p1 <- ggplot(Worldbank_q2, 
               aes(x = `Central_government_debt_total_(%_of_GDP)`,
                   y = `Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`,
                   color = Country_Name)) +
    geom_point(size = 1.5, alpha = 0.4) +
    geom_point(data = country_means, 
               aes(x = Mean_Debt, y = Mean_Education, color = Country_Name),
               size = 2.5, 
               alpha = 1) +
    geom_smooth(
      data = country_means,  # Use summarized data for the linear model
      aes(x = Mean_Debt, y = Mean_Education),  # Map aesthetics to mean values
      method = "lm", 
      color = "grey", 
      se = FALSE, 
      linewidth = 0.75, 
      na.rm = TRUE
    ) +
    geom_label_repel(
      data = country_means,  # Use country_means for labels to align with mean points
      aes(x = Mean_Debt, y = Mean_Education, label = Country_Name),  # Specify x and y
      size = 3.5,
      max.overlaps = 20,
      force = 0.7,
      alpha = 1,
      show.legend = FALSE
    ) +
    labs(
      title = "Staatsverschuldung und Bildungsgrad der Arbeiterschicht",
      x = "Staatsverschuldung (in % des BIP)",
      y = "Anteil der Arbeiterschicht mit grundlegender Bildung (in %)"
    ) +
    scale_color_manual(values = country_colors) +
    guides(color = "none") +
    facet_wrap(~Debt_Level)
  return(p1)
}

Q2()