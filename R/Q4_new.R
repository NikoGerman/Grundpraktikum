Q4 <- function() {
  ##########
  # How does GDP per capita relate to the prevalence 
  # of current tobacco use (% of adults)?
  #########
  # Reading Data
  Worldbank <- readRDS("Data/cleaned/Worldbank.RDS")
  country_colors <- readRDS("Data/cleaned/Country_Colors.rds")
  
  
  p0.1 <- Worldbank %>%
    group_by(Year, Country_Name) %>%
    summarize(Missing = is.na(`Prevalence_of_current_tobacco_use_(%_of_adults)`)) %>%
    ungroup() %>%
    mutate(Missing = factor(Missing, levels = c(TRUE, FALSE), labels = c("fehlt", "vorhanden"))) %>%
    ggplot(aes(x = Year, y = Country_Name)) +
    geom_tile(aes(fill = Missing), alpha = .7) +
    scale_fill_manual(values = c("fehlt" = "orange", "vorhanden" = "lightblue")) +
    labs(title = "Beobachtungen zum Tabakkonsum", x = "Jahr", y = "Land", fill = "Beobachtung") +
    scale_x_discrete(guide = guide_axis(angle = 45), 
                     breaks = seq(2000, 2021, by = 5),
                     labels = seq(2000, 2021, by = 5)) +
    theme(#panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = "grey", linewidth = .25))
  
  p0.2 <- Worldbank %>%
    group_by(Year, Country_Name) %>%
    summarize(Missing = is.na(`GDP_per_capita_PPP_(constant_2021_international_$)`)) %>%
    ungroup() %>%
    mutate(Missing = factor(Missing, levels = c(TRUE, FALSE), labels = c("fehlt", "vorhanden"))) %>%
    ggplot(aes(x = Year, y = Country_Name)) +
    geom_tile(aes(fill = Missing), alpha = .7) +
    scale_fill_manual(values = c("fehlt" = "orange", "vorhanden" = "lightblue")) +
    labs(title = "Beobachtungen zum BIP", x = "Jahr", y = "Land", fill = "Beobachtung") +
    scale_x_discrete(guide = guide_axis(angle = 45), 
                     breaks = seq(2000, 2021, by = 5),
                     labels = seq(2000, 2021, by = 5)) +
    theme(#panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = "grey", linewidth = .25))
  
  p0 <- (p0.1 | p0.2 + guides(fill = "none")) + plot_layout(axes = "collect", guides = "collect")
  
  
  p1 <- Worldbank %>%
    ggplot(aes(x = `GDP_per_capita_PPP_(constant_2021_international_$)`,
               y = `Prevalence_of_current_tobacco_use_(%_of_adults)`,
               color = Country_Name)) +
    geom_point(size = 2, alpha = .4) +
    scale_color_manual(values = country_colors) +
    labs(x = "BIP pro Kopf (in 2021 $)",
         y = "Prävalenz des Tabakkonsums") +
    guides(color = "none") +
    scale_x_continuous(labels = scales::label_number(suffix = "$")) +
    scale_y_continuous(labels = scales::label_number(suffix = "%"))
  
  p1.0 <- p1 +
    geom_smooth(se = FALSE, color = "grey", method = "lm") +
    facet_wrap(~Continent) +
    scale_x_log10(labels = scales::label_number(suffix = "$"), limits = c(1000, 200000)) +
    geom_text_repel(data = (Worldbank %>% 
                              group_by(Country_Name) %>%
                              slice_min(order_by = Year) %>%
                              ungroup()),
                    aes(label = Country_Name),
                    size = 3, max.overlaps = 5, force = .7)

  
  p1.1 <- p1 %+% (Worldbank %>%
           filter(`GDP_per_capita_PPP_(constant_2021_international_$)` <= 20000)) +
    geom_text_repel(data = (Worldbank %>% 
                              group_by(Country_Name) %>%
                              slice_min(order_by = Year) %>%
                              ungroup() %>%
                              filter(`GDP_per_capita_PPP_(constant_2021_international_$)` <= 20000)),
                    aes(label = Country_Name),
                    size = 3, max.overlaps = 10)
  
  p1.11 <- p1.1 +
    geom_smooth(aes(group = Country_Name, color = Country_Name),se = FALSE, method = "lm")
  
  p1.12 <- p1.1 +
    geom_smooth(se = FALSE, color = "grey", method = "lm")
    
    
  p1.2 <- p1 %+% (Worldbank %>%
    filter(`GDP_per_capita_PPP_(constant_2021_international_$)` > 20000)) +
    geom_text_repel(data = (Worldbank %>% 
                      group_by(Country_Name) %>%
                      slice_min(order_by = Year) %>%
                      ungroup() %>%
                      filter(`GDP_per_capita_PPP_(constant_2021_international_$)` > 20000)),
                    aes(label = Country_Name),
                    size = 3, max.overlaps = 10)
  
  p1.21 <- p1.2 +
    geom_smooth(aes(group = Country_Name, color = Country_Name),se = FALSE, method = "lm")
  
  p1.22 <- p1.2 +
    geom_smooth(se = FALSE, color = "grey", method = "lm")
  
  return(list(p0.1, p1.0, p1.11, p1.12, p1.21, p1.22))
}
