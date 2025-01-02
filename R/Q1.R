Q1 <- function() {
  #####
  # Welcher Zusammenhang besteht zwischen der Stromversorgung und 
  # dem bereinigten Netto-Pro-Kopf-Einkommen in verschiedenen Ländern?
  # Hängt dies auch von der Fläche oder der Bevölkerung eines Landes ab?
  #####
  # calculate spearman correlation coefficients
  Worldbank <- readRDS("Data/cleaned/Worldbank.RDS")
  country_colors <- readRDS("Data/cleaned/Country_Colors.rds")
  
  p0 <- Worldbank %>%
    group_by(Year, Country_Name) %>%
    summarize(Missing = is.na(`Adjusted_net_national_income_per_capita_(current_US$)`)) %>%
    ungroup() %>%
    mutate(Missing = factor(Missing, levels = c(TRUE, FALSE), labels = c("fehlt", "vorhanden"))) %>%
    ggplot(aes(x = Year, y = Country_Name)) +
    geom_tile(aes(fill = Missing), alpha = .7) +
    scale_fill_manual(values = c("fehlt" = "orange", "vorhanden" = "lightblue")) +
    labs(title = "Beobachtungen zum Nettonationaleinkommen pro Kopf", x = "Jahr", y = "Land", fill = "Beobachtung") +
    scale_x_discrete(guide = guide_axis(angle = 45), 
                     breaks = seq(2000, 2021, by = 5),
                     labels = seq(2000, 2021, by = 5)) +
    theme(#panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = "grey", linewidth = .25))
  
  p1.0 <- Worldbank %>%
    ggplot(aes(x = `Access_to_electricity_(%_of_population)`,
               y = `Adjusted_net_national_income_per_capita_(current_US$)`,
               color = Country_Name)) +
    geom_point() +
    scale_x_continuous(label = scales::label_number(suffix = "%")) +
    scale_y_log10(label = scales::label_number(suffix = "$")) +
    scale_color_manual(values = country_colors) +
    labs(x = "Zugang zu Elektrizität\n(Anteil Gesamtbevölkerung)",
         y = "Netto-pro-Kopf-Einkommen (bereinigt)",
         color = "Land")
  
  correlation <- Worldbank %>%
    group_by(Country_Name) %>%
    summarize(r_spearman = cor(x = `Access_to_electricity_(%_of_population)`, 
                              y = `Adjusted_net_national_income_per_capita_(current_US$)`,
                              method = "spearman",
                              use = "na.or.complete")) %>%
    filter(!is.na(r_spearman))
  #
  p1.1 <- correlation %>%
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
  
  # basic plot
  p1.23 <- ggplot(Worldbank,
              aes(y = `Adjusted_net_national_income_per_capita_(current_US$)`)
              )+
    scale_y_log10(labels = scales::label_number(suffix = "$")) +
    labs(y = "Netto-pro-Kopf-Einkommen (bereinigt)")
  
  # for later use - number of observations per bin
  Electricity_obs_per_bin <- Worldbank %>%
    group_by(electricity_binned) %>%
    summarize(count = n())
  
  # scatterplot faceted by continent
  p1.2 <- p1.23 + 
    geom_point(aes(x = `Access_to_electricity_(%_of_population)`, color = `Country_Name`)) +
    facet_wrap(~Continent, ncol = 2) +
    scale_color_manual(values = country_colors, guide = "none") +
    scale_x_continuous(label = scales::label_number(suffix = "%")) +
    labs(x = "Zugang zu Elektrizität\n(Anteil Gesamtbevölkerung)",
         color = "Land") +
    theme(panel.grid.minor = element_blank())
  
  # bosxplots with 25% intervals
  p1.3 <- p1.23 + geom_boxplot(aes(x = electricity_binned, group = electricity_binned)) +
    geom_label_repel(data = Electricity_obs_per_bin,
                     aes(label = paste("n =", count),
                         x = electricity_binned, y = 100), 
                     direction = "y", force = 0
                     ) +
    labs(x = "Zugang zu Elektrizität\n(Anteil Gesamtbevölkerung)")+
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
  #
  data_p23 <- Worldbank %>%
    group_by(`Country_Name`) %>%
    summarize(avg_pop = mean(`Population_total`), surface = mean(`Surface_area_(sq_km)`)) %>%
    full_join(correlation)
  #
  p2.1 <- data_p23 %>%
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
  #
  p2.2 <- p2.1 %+%
    (data_p23 %>%
       filter(Country_Name != "Aruba")) +
    labs(x = "Landesfläche")
  #
  p3.1 <- p2.1 %+% aes(x = avg_pop/1000000) %+%
    scale_x_log10(limits = c(5e-2, 3e3), labels = scales::label_number(suffix = " Mio")) +
    labs(x = "Bevölkerung")
  #
  p3.2 <- p2.2 %+% aes(x = avg_pop/1000000) %+%
    scale_x_log10(limits = c(5e-2, 3e3), labels = scales::label_number(suffix = " Mio")) +
    labs(x = "Bevölkerung")
  
  t1 <- Worldbank %>%
    group_by(Country_Name) %>%
    summarize("durchschnittliche Elektrifizierung" = mean(`Access_to_electricity_(%_of_population)`)) %>%
    arrange(desc(`durchschnittliche Elektrifizierung`)) %>%
    filter(`durchschnittliche Elektrifizierung` > 99) %>%
    rename(Land = Country_Name) %>%
    mutate(`durchschnittliche Elektrifizierung` = sprintf("%.1f%%", `durchschnittliche Elektrifizierung`))
  
  return(list(p1.1, 
              p1.2, 
              p1.3, 
              p2.1, 
              p2.2, 
              p3.1, 
              p3.2, 
              t1,
              p1.0,
              p0)) 
}
