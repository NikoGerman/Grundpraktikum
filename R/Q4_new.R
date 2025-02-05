Q4 <- function() {
  # ----------------------
  # Question:
  #   - How does GDP per capita relate to the prevalence 
  #     of current tobacco use (% of adults)?
  # ----------------------
  
  # ----------------------
  # load Data
  # ----------------------
  Worldbank <- readRDS("Data/cleaned/Worldbank.RDS")
  country_colors <- readRDS("Data/cleaned/Country_Colors.rds")
  
  # ----------------------
  # missingness:
  #   plot Missingness of 
  #     - Prevalence of tobacco use
  #     by Country/Year
  #   - for plot.missing() see utils.R
  # ----------------------
  missingness <- Worldbank %>% plot.missing(x1 = "Prevalence_of_current_tobacco_use_(%_of_adults)") +
    ggtitle("Beobachtungen zur Prävalenz des Tabakkonsums")
  

  plot_basic <- Worldbank %>%
    ggplot(aes(x = `GDP_per_capita_PPP_(constant_2021_international_$)`,
               y = `Prevalence_of_current_tobacco_use_(%_of_adults)`,
               color = Country_Name)) +
    geom_point(size = 2, alpha = .4) +
    scale_color_manual(values = country_colors, guide = "none") +
    labs(x = "BIP pro Kopf",
         y = "Prävalenz des Tabakkonsums") +
    scale_x_continuous(labels = scales::label_number(suffix = "$")) +
    scale_y_continuous(labels = scales::label_number(suffix = "%"))
  
  p1.0 <- plot_basic +
    geom_smooth(se = FALSE, color = "grey", method = "lm") +
    facet_wrap(~Continent) +
    scale_x_log10(labels = scales::label_number(suffix = "$"), limits = c(1000, 200000)) +
    geom_text_repel(data = (Worldbank %>% 
                              group_by(Country_Name) %>%
                              slice_min(order_by = Year) %>%
                              ungroup()),
                    aes(label = Country_Name),
                    size = 3, max.overlaps = 5, force = .7)

  
  p1.1 <- plot_basic %+% (Worldbank %>%
                    filter(Development_status == "Industrieland")
           #filter(`GDP_per_capita_PPP_(constant_2021_international_$)` <= 20000)
           ) +
    geom_text_repel(data = (Worldbank %>% 
                              group_by(Country_Name) %>%
                              slice_min(order_by = Year) %>%
                              ungroup() %>%
                              #filter(`GDP_per_capita_PPP_(constant_2021_international_$)` <= 20000)
                              filter(Development_status == "Industrieland")),
                    aes(label = Country_Name),
                    size = 3, max.overlaps = 10)
  
  p1.11 <- p1.1 +
    geom_smooth(aes(group = Country_Name, color = Country_Name),se = FALSE, method = "lm")
  
  p1.12 <- p1.1 +
    geom_smooth(se = FALSE, color = "grey", method = "lm")
    
    
  p1.2 <- plot_basic %+% (Worldbank %>%
    #filter(`GDP_per_capita_PPP_(constant_2021_international_$)` > 20000)
      filter(Development_status != "Industrieland")) +
    geom_text_repel(data = (Worldbank %>% 
                      group_by(Country_Name) %>%
                      slice_min(order_by = Year) %>%
                      ungroup() %>%
                      #filter(`GDP_per_capita_PPP_(constant_2021_international_$)` > 20000)
                        filter(Development_status != "Industrieland")),
                    aes(label = Country_Name),
                    size = 3, max.overlaps = 10)
  
  p1.21 <- p1.2 +
    geom_smooth(aes(group = Country_Name, color = Country_Name),se = FALSE, method = "lm")
  
  p1.22 <- p1.2 +
    geom_smooth(se = FALSE, color = "grey", method = "lm")
  
  return(list(p0.1, p1.0, p1.11, p1.12, p1.21, p1.22))
}
