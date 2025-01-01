Q5 <- function() {
  # Is there a relationship between the percentage of agricultural land and
  # CO2 emissions per capita across countries? 
  # Does the size of the surface area of the country play a role?
  # Reading Data
  Worldbank <- readRDS("Data/cleaned/Worldbank.RDS")
  country_colors <- readRDS("Data/cleaned/Country_Colors.rds")
  
  p0.1 <- Worldbank %>%
    filter(`Carbon_dioxide_(CO2)_emissions_(total)_excluding_LULUCF_(Mt_CO2e)` != 0) %>%
    ggplot(aes(y = `Carbon_dioxide_(CO2)_emissions_(total)_excluding_LULUCF_(Mt_CO2e)`, 
               x = Year,
               group = Country_Name)) +
    geom_path(color = "lightblue") +
    facet_wrap(~Country_Name, scales = "free_y") +
    scale_y_continuous(guide = "none") +
    scale_x_discrete(breaks = seq(2000, 2021, by = 5), labels = seq(2000, 2021, by = 5), guide = guide_axis(angle = 45)) +
    labs(x = "Jahr", y = "CO2 Emissionen", caption = "y-Skalen nicht einheitlich\n CO2-Emissionen sind alle > 0")

  p0.2 <- p0.1 +
    geom_label_repel(data = Worldbank %>%
                       group_by(Country_Name) %>%
                       summarize(Var = var(`Carbon_dioxide_(CO2)_emissions_(total)_excluding_LULUCF_(Mt_CO2e)`)),
                     aes(label = sprintf("Var = %.2e", Var),
                         x = 10,
                         y = 0), 
                     direction = "both", force = 1)
  
  p0.30 <-   Worldbank %>%
    ggplot(aes(x = `Carbon_dioxide_(CO2)_emissions_(total)_excluding_LULUCF_(Mt_CO2e)`,
               y = Country_Name)) +
    geom_boxplot() +
    labs(x = "CO2-Emissionen",
         y = "Land")
  
  p0.31 <- p0.30 +
    scale_x_log10(label = scales::label_number(suffix = "Mt"))
  
  p0.32 <- p0.30 +
    scale_x_continuous(label = scales::label_number(suffix = "Mt"))
  
  p0.3 <- (p0.31 | p0.32) + plot_layout(axes = "collect", guides = "collect")
  
  
  WB_summarized <- Worldbank %>%
    group_by(Country_Name) %>%
    summarise(mean_agricultural_land = mean(`Agricultural_land_(%_of_land_area)`, na.rm = TRUE),
              mean_surface_area = mean(`Surface_area_(sq_km)`, na.rm = TRUE),
              mean_co2_total = mean(`Carbon_dioxide_(CO2)_emissions_(total)_excluding_LULUCF_(Mt_CO2e)`, na.rm = TRUE),
              mean_population = mean(Population_total, na.rm = TRUE),
              mean_co2_per_capita = mean(CO2_t_per_capita, na.rm = TRUE),
              r_spearman = cor(`Agricultural_land_(%_of_land_area)`, CO2_t_per_capita, method = "spearman")) %>%
    ungroup()
  
  p1 <- WB_summarized %>%
    filter(!is.na(r_spearman)) %>%
    ggplot(aes(x = r_spearman, y = forcats::fct_reorder(`Country_Name`, r_spearman))) +
    geom_col(fill = "lightblue") +
    geom_vline(xintercept = 0, color = "red") +
    geom_vline(xintercept = 0.5, color = "red", linetype = "dashed") +
    geom_vline(xintercept = -0.5, color = "red", linetype = "dashed") +
    guides(fill = "none") +
    scale_x_continuous(limits = c(-1, 1)) +
    labs(title = "Korrelation von landwirtschaftlicher Fläche und CO2 Emissionen",
         y = "",
         x = "Korrelationskoeffizient",
         caption = "Verwendeter Korrellationskoeffizient: Spearman") +
    geom_text_repel(aes(label = sprintf("%.2f", round(r_spearman, 2)), x = sign(r_spearman) * -.10), force = 0) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = "grey", linewidth = .25)) %>%
    suppress_mw()
  
  p2 <- WB_summarized %>%
    ggplot(aes(x = mean_surface_area,
               y = r_spearman,
               color = Country_Name)) +
    geom_point(size = 1, alpha = .7) +
    geom_label_repel(aes(label = Country_Name)) +
    scale_color_manual(values = country_colors, guide = "none") +
    scale_x_log10(label = scales::label_number(suffix = " qkm"),
                  limits = c(1e4, 3e7)) +
    labs(x = "Geasmtfläche",
         y = "Korrelationskoeffizient",
         caption = "verwendeter Korrelationskoeffizient: Spearman")
  
  #cor(WB_summarized$r_spearman, WB_summarized$mean_surface_area, method = "spearman", use = "complete.obs")
  
  p3 <- Worldbank %>%
    ggplot(aes(x = `Agricultural_land_(%_of_land_area)`,
               y = `CO2_t_per_capita`,
               color = Country_Name)) +
    geom_point(size = 1, alpha = .7) +
    #geom_smooth(method = "lm", color = "grey", linewidth = .75, se = FALSE) +
    scale_color_manual(values = country_colors, guide = "none") +
    scale_x_continuous(labels = scales::label_number(suffix = "%")) +
    scale_y_continuous(labels = scales::label_number(suffix = "t")) +
    geom_label_repel(data = Worldbank %>% distinct(`Country_Name`, .keep_all = TRUE),
                    aes(label = `Country_Name`),
                    size = 3,
                    max.overlaps = 10) +
    labs(title = "Landwirtschaftliches Land und CO2 Emissionen",
         x = "Anteil landwirtschaftlich genutztes Land",
         y = "CO2 Emissionen pro Kopf")
  
  p4 <- WB_summarized %>%
    ggplot(aes(x = mean_agricultural_land,
               y = mean_co2_per_capita,
               color = Country_Name)) +
    geom_point(size = 1.5, alpha = .7) +
    #geom_smooth(method = "lm", se = TRUE, color = "grey", linewidth = .5) +
    geom_label_repel(data = WB_summarized %>% 
                       distinct(`Country_Name`, .keep_all = TRUE),
                     aes(label = `Country_Name`),
                     size = 3,
                     max.overlaps = 10) +
    annotate("label", x = 65, y = 30, 
             label = sprintf("r_sp = %.2f",  cor(Worldbank$`Agricultural_land_(%_of_land_area)`,
                                              Worldbank$CO2_t_per_capita,
                                              method = "spearman")
                             )
             ) +
    scale_color_manual(values = country_colors, guide = "none") +
    scale_x_continuous(labels = scales::label_number(suffix = "%")) +
    scale_y_log10(labels = scales::label_number(suffix = "t")) +
    labs(title = "Landwirtschaft und CO2-Emissionen",
         x = "durschnittlicher Anteil landwirtschaftlich genutzter Fläche",
         y = "durchschnittliche CO2-Emissionen pro Kopf",
         caption = "r_sp: Spearman-Korrelationskoeffizient")
  
  
  # p4 + aes(x = mean_surface_area,
  #          y = mean_co2_per_capita) +
  #   scale_x_continuous(labels = scales::label_number(suffix = " sq. km")) +
  #   labs(title = "Gesamtfläche und CO2-Emissionen",
  #        x = "Gesamtfläche",
  #        y = "durchschnittliche CO2-Emissionen pro Kopf")
  
  return(list(
    p0.1,
    p0.2,
    p0.3,
    p1,
    p2,
    p3,
    p4
    )
  )
}
