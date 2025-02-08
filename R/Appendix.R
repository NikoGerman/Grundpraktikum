spearman_examples <- function(method = "save") {
  # create examples for spearman correlation
  
  # ----------------------
  # assert property of input
  # ----------------------
  checkmate::assertCharacter(method, len = 1, any.missing = FALSE)
  checkmate::assertSubset(method, choices = c("save", "return"))
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
  result <- ((p_exp | p_lin | p_noise) / (p_sin | p_const | p_rand)) + 
    plot_annotation(title = "Spearman Korrelationskoeffizient")
  
  # ----------------------
  #   if save is TRUE, check if directory exists, if not create it
  #   save result as .png
  # ----------------------
  if (method == "save") {
    if (!dir.exists("Figures/Appendix")) {
      dir.create(file.path("Figures/Appendix"))
    }
    ggsave(result, filename = "Figures/Appendix/spearman_examples.png", device = "png")
  } else {
    # ----------------------
    #   retrun result
    # ----------------------
    return(result)
  }
}

# ----------------------
# produce graph explaining Volkswirtschaftliche Gesamtrechnung (VGR)
# returns raw format
# ----------------------

graph_VGR <- function(method = "save") {
  # create graph explaining Volkswirtschaftliche Gesamtrechnung (VGR)
  
  # ----------------------
  # assert property of input
  # ----------------------
  checkmate::assertCharacter(method, len = 1, any.missing = FALSE)
  checkmate::assertSubset(method, choices = c("save", "return"))
  
  # ----------------------
  # create result: the graph
  # ----------------------
  result <- DiagrammeR::grViz("
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
  
  if (method == "save") {
    # ----------------------
    # check if directory exists, if not create it
    # save result as .png
    # ----------------------
    if (!dir.exists("Figures/Appendix")) {
      dir.create(file.path("Figures/Appendix"))
    }
    rsvg::rsvg_png(result, file = "Figures/Appendix/graphVGR.png", height = 1440)
  } else {
    return(result)
  }
  
}

aggr_examples <- function(data, country_colors = NULL, method = "save") {
  # create examples of different aggregation outputs
  # aggregating education by Country:
  #   - mean
  #   - median
  #   - first observation by Year
  #   - last observation by Year
  # returns a single plot
  
  # ----------------------
  # assert properties of inputs
  # ----------------------
  checkmate::assertDataFrame(data, col.names = "named")
  checkmate::assertCharacter(country_colors, null.ok = TRUE)
  checkmate::assertCharacter(method, len = 1, any.missing = FALSE)
  checkmate::assertSubset(method, choices = c("save", "return"))
  
  # ----------------------
  # if not provided, generate country_colors
  # ----------------------
  if (is.null(country_colors)) {
    country_colors <- assignCountryColors(data)
  }
  
  # ----------------------
  # p_mean
  #   + aggregate labor force w/ basic education by country
  #     using the mean
  #   + plot that aggregation vs mean central gov debt
  # ----------------------
  p_mean <- data %>%
    group_by(Country_Name) %>%
    summarise(mean_debt = mean(`Central_government_debt_total_(%_of_GDP)`, na.rm = TRUE),
              agg_education = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
    ungroup() %>%
    ggplot(aes(x = mean_debt,
               y = agg_education,
               color = Country_Name)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "grey", alpha = .15) +
    geom_label_repel(aes(label = Country_Name)) +
    scale_color_manual(values = country_colors, guide = "none") +
    scale_x_continuous(label = scales::label_number(suffix = "%"), limits = c(20, 130)) +
    scale_y_continuous(label = scales::label_number(suffix = "%"), limits = c(0, 80)) +
    labs(x = "Staatsverschuldung",
         y = "Bildungsquote",
         title = "Durchschnitt")
  
  # ----------------------
  # p_median
  #   + aggregate labor force w/ basic education by country
  #     using the median
  #   + plot that aggregation vs mean central gov debt
  # ----------------------
  p_median <- p_mean %+% (data %>%
    group_by(Country_Name) %>%
    summarise(mean_debt = median(`Central_government_debt_total_(%_of_GDP)`, na.rm = TRUE),
              agg_education = median(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
    ungroup()) +
    ggtitle("Mittlere")
  
  # ----------------------
  # p_first.obs
  #   + aggregate labor force w/ basic education by country
  #     using the first observation
  #   + plot that aggregation vs mean central gov debt
  # ----------------------
  p_first.obs <- p_mean %+% (data %>%
                               filter(!is.na(`Central_government_debt_total_(%_of_GDP)`) &
                                        !is.na(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`)) %>%
                               group_by(Country_Name) %>%
                               slice_min(Year) %>%
                               summarise(mean_debt = mean(`Central_government_debt_total_(%_of_GDP)`, na.rm = TRUE),
                                         agg_education = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
                               ungroup()) +
    ggtitle("Erste")
  
  # ----------------------
  # p_last.obs
  #   + aggregate labor force w/ basic education by country
  #     using the last observation
  #   + plot that aggregation vs mean central gov debt
  # ----------------------
  p_last.obs <- p_mean %+% (data %>%
                               filter(!is.na(`Central_government_debt_total_(%_of_GDP)`) &
                                        !is.na(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`)) %>%
                               group_by(Country_Name) %>%
                               slice_max(Year) %>%
                               summarise(mean_debt = mean(`Central_government_debt_total_(%_of_GDP)`, na.rm = TRUE),
                                         agg_education = mean(`Labor_force_with_basic_education_(%_of_total_working-age_population_with_basic_education)`, na.rm = TRUE)) %>%
                               ungroup()) +
    ggtitle("Letzte")
  
  # ----------------------
  # using patchwork, combine p_first.obs, p_mean, p_median, p_last.obs as result
  # ----------------------
  result <- (p_first.obs | p_mean | p_median | p_last.obs) + 
    plot_layout(axes = "collect") + plot_annotation(title = "Ein Beispiel aus Frage 2")
  
  # ----------------------
  # save result examples as .png
  # ----------------------
  if (method == "save") {
    if (!dir.exists("Figures/Appendix")) {
      dir.create(file.path("Figures/Appendix"))
    }
    ggsave(result, filename = "Figures/Appendix/aggregation_examples.png", device = "png")
  } else {
    # ----------------------
    # return result
    # ----------------------
    return(result)
  }
}

country_classification <-function(data, method = "save") {
  # ----------------------
  # assert properties of inputs
  # ----------------------
  checkmate::assertDataFrame(data, col.names = "named")
  checkmate::assertCharacter(method, len = 1, any.missing = FALSE)
  checkmate::assertSubset(method, choices = c("save", "return"))
  
  result <- data %>%
    group_by(Income_Group) %>%
    summarise(Countries = paste(unique(Country_Name), collapse = ", "), .groups = 'drop') %>%
    rename(Klassifizierung = Income_Group, Länder = Countries) %>%
    knitr::kable(format = "html", align = c("l", "r")) %>%
    kableExtra::kable_styling(font_size = 24)
  
  if (method == "save") {
     result %>%
      readr::write_file("Figures/Appendix/Country_Classification.html")
  } else {
    return(result)
  }
}
