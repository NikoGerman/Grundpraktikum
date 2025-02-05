# ----------------------
# produce contents of the Appendix
# ----------------------

# ----------------------
# produce examples for spearman correlation
# ----------------------
spearman_examples <- function() {
  # ----------------------
  #   - set random seed
  #   - x_ : 50 values ranging from 0 to 10
  #   - y_noise : x_ plus noise sampled from N(0, 1.5)
  #   - y_rand : 50 points sampled from N(0, 1.5)
  # ----------------------
  set.seed(42)
  x_ <- seq(0, 10, length.out = 50)
  y_noise <- x_ + rnorm(length(x_), 0, 1.5)
  y_rand <- rnorm(length(x_), 0, 1.5)
  
  # ----------------------
  #   - initialize basic plot
  # ----------------------
  p <- ggplot(data = NULL, aes(x = x_)) +
    labs(x = "x", y = "y") +
    guides(x = "none", y = "none")
  
  # ----------------------
  #   - x vs y = exp(x)
  # ----------------------
  p_exp <- p + geom_point(aes(y = exp(x_))) +
    geom_label(aes(label = sprintf("r_sp = %.2f", cor(x_, exp(x_), method = "spearman")), x = 5, y = 15000))
  
  # ----------------------
  #   - x vs y = sin(c*x)
  # ----------------------
  p_sin <- p + geom_point(aes(y = sin(pi/3 * x_))) +
    geom_label(aes(label = sprintf("r_sp = %.2f", cor(x_, sin(pi/3 * x_), method = "spearman")), x = 5, y = 1.3)) +
    ylim(-1.5, 1.5)
  
  # ----------------------
  #   - x vs y = -x
  # ----------------------
  p_lin <- p + geom_point(aes(y = -1*x_)) +
    geom_label(aes(label = sprintf("r_sp = %.2f", cor(x_, -1*x_, method = "spearman")), x = 6.5, y = -1.5))
  
  # ----------------------
  #   - x vs y = 1
  # ----------------------
  p_const <- p + geom_point(aes(y = rep(1, length(x_)))) +
    geom_label(aes(label = sprintf("r_sp = %.2f", cor(x_, rep(1, length(x_)), method = "spearman")), x = 5, y = 1.5)) +
    scale_y_continuous(limits = c(0, 2))
  
  # ----------------------
  #   - x vs y_noise
  # ----------------------
  p_noise <- p + geom_point(aes(y = y_noise)) +
    geom_label(aes(label = sprintf("r_sp = %.2f", cor(x_, y_noise, method = "spearman")), x = 4, y = 12)) +
    ylim(-3, 13)
  
  # ----------------------
  #   - x vs y_rand
  # ----------------------
  p_rand <- p + geom_point(aes(y = y_rand)) +
    geom_label(aes(label = sprintf("r_sp = %.2f", cor(x_, y_rand, method = "spearman")), x = 5, y = 6)) +
    ylim(-8, 8)
  
  # ----------------------
  #   gather plots in 2x3
  #   add title
  # ----------------------
  p_out <- ((p_exp | p_lin | p_noise) / (p_sin | p_const | p_rand)) + 
    plot_annotation(title = "Spearman Korrelationskoeffizient")
  
  # ----------------------
  #   retrun final plot
  # ----------------------
  return(p_out)
}

# ----------------------
# produce graph explaining Volkswirtschaftliche Gesamtrechnung (VGR)
# returns raw format
# ----------------------

graph_VGR <- function() {
  DiagrammeR::grViz("
  digraph {
  layout = dot
    node [shape = diamond, color=lightblue, style=filled,fixedsize=False, fontname=Helvetica, labelType=\"html\"]
    edge[color=grey,arrowhead=vee,minlen = 1]
    
    BIP[label = <<b>Bruttoinlandsprodukt</b>>]
    BNE[label = <<b>Bruttonationaleinkommen</b>>]
    NNE[label = <<b>Nettonationaleinkommen</b>>]
  
    BIP -> BNE[label=<Saldo Primäreinkommen>, fontname=Helvetica]
    BNE -> NNE[label=<Abschreibungen>, fontname=Helvetica]
    
    edge [minlen = 2]
    
  }
   ") %>%
    DiagrammeRsvg::export_svg() %>%
    charToRaw()
}

# ----------------------
# produce aggregation examples using question 2
# aggregating education by Country:
#   - mean
#   - median
#   - first observation by Year
#   - last observation by Year
# returns a single plot
# ----------------------
aggr_examples <- function() {
  p_mean <- Worldbank %>%
    group_by(Country_Name) %>%
    summarise(mean_debt = mean(`Central_government_debt_total_(%_of_GDP)`, na.rm = TRUE),
              agg_education = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
    ungroup() %>%
    ggplot(aes(x = mean_debt,
               y = agg_education,
               color = Country_Name)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "grey") +
    geom_label_repel(aes(label = Country_Name)) +
    scale_color_manual(values = country_colors, guide = "none") +
    scale_x_continuous(label = scales::label_number(suffix = "%"), limits = c(20, 130)) +
    scale_y_continuous(label = scales::label_number(suffix = "%"), limits = c(0, 80)) +
    labs(x = "Staatsverschuldung",
         y = "Bildungsquote",
         title = "Durchschnitt")
  
  p_median <- p_mean %+% (Worldbank %>%
    group_by(Country_Name) %>%
    summarise(mean_debt = median(`Central_government_debt_total_(%_of_GDP)`, na.rm = TRUE),
              agg_education = median(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
    ungroup()) +
    ggtitle("Mittlere")
  
  p_first.obs <- p_mean %+% (Worldbank %>%
                               filter(!is.na(`Central_government_debt_total_(%_of_GDP)`) &
                                        !is.na(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`)) %>%
                               group_by(Country_Name) %>%
                               slice_min(Year) %>%
                               summarise(mean_debt = mean(`Central_government_debt_total_(%_of_GDP)`, na.rm = TRUE),
                                         agg_education = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
                               ungroup()) +
    ggtitle("Erste")
  
  p_last.obs <- p_mean %+% (Worldbank %>%
                               filter(!is.na(`Central_government_debt_total_(%_of_GDP)`) &
                                        !is.na(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`)) %>%
                               group_by(Country_Name) %>%
                               slice_max(Year) %>%
                               summarise(mean_debt = mean(`Central_government_debt_total_(%_of_GDP)`, na.rm = TRUE),
                                         agg_education = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
                               ungroup()) +
    ggtitle("Letzte")
  
  (p_first.obs | p_mean | p_median | p_last.obs) + 
    plot_layout(axes = "collect")
}
