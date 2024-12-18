Q2 <- function() {
  ##############################################################################
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
      mean_Edu = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE),
      Debt_Level = ifelse(mean_Debt < 75, "Durchschnittliche Verschuldung < 75%", "Durchschnittliche Verschuldung >= 75%"),
      Education_Level = ifelse(mean_Edu < 60, "Durchschnittliche Bildung < 60%", "Durchschnittliche Bildung >= 60%")
    ) %>%
    ungroup()
  
  # Summarize data by country for the linear model and mean points
  country_means <- Worldbank_q2 %>%
    group_by(Country_Name) %>%
    summarize(
      Mean_Debt = mean(`Central_government_debt_total_(%_of_GDP)`, na.rm = TRUE),
      Mean_Education = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE),
      Debt_Level = first(Debt_Level),
      Education_Level = first(Education_Level)
    ) %>%
    ungroup()
  
  group_means_q1 <- country_means %>%
    group_by(Debt_Level) %>%
    summarize(Mean_Education = mean(Mean_Education, na.rm = TRUE)) %>%
    pivot_wider(
      names_from = Debt_Level,
      values_from = Mean_Education
    )
  
  hline_means <- data.frame(
    Debt_Level = c("Durchschnittliche Verschuldung < 75%", "Durchschnittliche Verschuldung >= 75%"),
    yintercept = c(group_means_q1$`Durchschnittliche Verschuldung < 75%`,group_means_q1$`Durchschnittliche Verschuldung >= 75%`)
  )
  
  p1 <- ggplot(Worldbank_q2, 
               aes(x = `Central_government_debt_total_(%_of_GDP)`,
                   y = `Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`,
                   color = Country_Name)) +
    geom_point(size = 1.5, alpha = 0.4) +
    geom_point(data = country_means, 
               aes(x = Mean_Debt, y = Mean_Education, color = Country_Name),
               size = 2.5, 
               alpha = 1) +
    geom_label_repel(
      data = country_means,
      aes(x = Mean_Debt, y = Mean_Education, label = Country_Name),  # Specify x and y
      size = 3.5,
      max.overlaps = 20,
      force = 0.7,
      alpha = 1,
      show.legend = FALSE
    ) +
    geom_hline(
      data = hline_means, 
      aes(yintercept = yintercept), 
      color = "red", 
      linetype = "dashed", 
      linewidth = 1
    ) +
    labs(
      title = "Staatsverschuldung und Bildungsgrad der Arbeiterschicht",
      x = "Staatsverschuldung (in % des BIP)",
      y = "Anteil der Arbeiterschicht mit grundlegender Bildung (in %)"
    ) +
    scale_color_manual(values = country_colors) +
    guides(color = "none") +
    facet_wrap(~Debt_Level)
  
  # Are countries with higher percentage of labor force with basic education
  # able to maintain lower pupil-teacher ratios, and what impact might this have
  # on education quality?
  # ANSWER: Yes they can. It means the education quality is consistent.
  
  # Calculate mean education per country
  country_means_2 <- Worldbank %>%
    group_by(Country_Name) %>%
    summarize(
      Mean_Education = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)
    ) %>%
    na.omit() %>%
    arrange(desc(Mean_Education))
  
  # Select top 10 countries based on mean education
  top10_countries <- country_means_2 %>%
    slice_head(n = 10) %>%
    pull(Country_Name)
  
  # Filter Worldbank data for top 10 countries and select relevant columns
  Worldbank_q2_1 <- Worldbank %>%
    filter(Country_Name %in% top10_countries) %>%
    select(
      Country_Name,
      Country_Code,
      Year,
      `Pupil-teacher_ratio_tertiary`,
      `Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`
    ) %>%
    # Convert Year to numeric
    mutate(Year = as.numeric(as.character(Year))) %>%
    # Remove rows with NA in Pupil-teacher_ratio_tertiary
    filter(!is.na(`Pupil-teacher_ratio_tertiary`))
  
  # Optionally, inspect the data structure to confirm variable types
  str(Worldbank_q2_1)
  
  # Ensure 'label_data' is correctly defined and contains necessary variables
  # For example, if you want to label the last year for each country:
  label_data <- Worldbank_q2_1 %>%
    group_by(Country_Name) %>%
    filter(Year == max(Year)) %>%
    ungroup()
  
  # Create the plot
  p2 <- ggplot(Worldbank_q2_1, 
               aes(
                 x = Year,
                 y = `Pupil-teacher_ratio_tertiary`,  # Enclosed in backticks
                 color = Country_Name,
                 group = Country_Name
               )) +
    geom_line(linewidth = 0.7, na.rm = TRUE) +
    geom_point(size = 1.2, na.rm = TRUE) +
    geom_smooth(aes(group = 1), method = "lm", color = "grey", se = FALSE, linewidth = 0.75, na.rm = TRUE) +
    scale_color_manual(values = country_colors) +
    labs(
      title = "Schüler-Lehrer-Verhältnis über die Jahre\n(Top 10 Länder nach gebildetster Arbeiterschicht)",
      x = "Jahr",
      y = "Schüler-Lehrer-Verhältnis"
    ) +
    scale_x_continuous(breaks = unique(Worldbank_q2_1$Year)) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    guides(color = "none") +
    geom_label_repel(
      data = label_data,  # Ensure label_data is correctly defined and contains Year as numeric
      aes(label = Country_Name),
      size = 3.5,
      max.overlaps = 20,
      force = 0.7,
      alpha = 1,
      show.legend = FALSE
    )
  
  
  
  return(list(p1, p2))
}

Q2()