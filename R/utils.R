assignCountryColors <- function(data) {
  # assigns unique color to each unique country
  # in data

  # ----------------------
  # check properties of data
  # ----------------------
  checkmate::assertDataFrame(data, col.names = "named")
  checkmate::assertFactor(data$Country_Name)
  # ----------------------
  #   - extract the unique country_names
  #   - get a unique color of the viridis palette for each country
  # ----------------------
  unique_countries = levels(data$Country_Name)
  colors <- viridis::viridis(length(unique_countries), option = "D")
  names(colors) <- unique_countries
  return(colors)
}

suppress_mw <- function(expr) {
  # suppresses messages AND warnings
  suppressMessages(suppressWarnings(expr))
}

corr.plot <- function(data, x1, x2) {
  # create correlation plot between feature x1 and x2
  # by Country. Only Countries w/ at least one complete observation 
  # are shown
  
  # ----------------------
  # check properties of input
  # ----------------------
  checkmate::assertDataFrame(data, col.names = "named")
  checkmate::assertSubset(c(x1, x2), choices = names(data))
  data %>%
    group_by(Country_Name) %>%
    summarise(r_spearman = cor(x = !!sym(x1),
                               y = !!sym(x2),
                               method = "spearman",
                               use = "na.or.complete")) %>%
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
          axis.line = element_line(color = "grey", linewidth = .25))
}

plot.missing <- function(data, x1, x2 = NULL) {
  # create tile plot showing missing data by Country and year
  # return ggplot2 object
  # accepts up to two variable names, x1 & x2
  # if both are supplied, the function handles "vorhanden" as complete observation
  # and "fehlt" as either or both observations are missing
  
  # ----------------------
  # check properties of input
  # ----------------------
  checkmate::assertDataFrame(data, col.names = "named")
  checkmate::assertSubset(c(x1, x2), choices = names(data))
  if (is.null(x2)) {x2 <- x1}
  data %>%
    group_by(Year, Country_Name) %>%
    summarize(Missing = is.na(!!sym(x1)) | is.na(!!sym(x2))) %>%
    ungroup() %>%
    mutate(Missing = factor(Missing, levels = c(TRUE, FALSE), labels = c("fehlt", "vorhanden"))) %>%
    ggplot(aes(x = Year, y = Country_Name)) +
    geom_tile(aes(fill = Missing), alpha = .7) +
    scale_fill_manual(values = c("fehlt" = "orange", "vorhanden" = "lightblue")) +
    labs(x = "Jahr",
         y = "Land",
         fill = "Beobachtung") +
    scale_x_discrete(guide = guide_axis(angle = 45), 
                     breaks = seq(2000, 2021, by = 5),
                     labels = seq(2000, 2021, by = 5)) +
    theme(panel.border = element_blank(),
          axis.line = element_line(color = "grey", linewidth = .25))
}

save.figures <- function(fun, args){
  # save figures returned by fun
  # saves figures to Figures/fun
  
  # ----------------------
  # assert properties of inputs
  # ----------------------
  checkmate::assertFunction(fun, null.ok = FALSE)
  fun.name <- as.character(substitute(fun))
  checkmate::assertSubset(fun.name, choices = paste0("Q", 1:5))
  checkmate::assertList(args)
  
  # ----------------------
  # call fun to create plots
  # ----------------------
  plots <- do.call(fun, args = args)
  
  # ----------------------
  # create directory if not existing
  # ----------------------
  if (!dir.exists(file.path("Figures/", fun.name))) {
    dir.create(file.path("Figures/", fun.name))
  }
  
  for (i in 1:length(plots)) {
    # ----------------------
    # save ggplot objects as .png
    # ----------------------
    if ("gg" %in% class(plots[[i]])) {
      filename <- paste0("Figures/", fun.name, "/", fun.name, "_plot_", i, ".png")
      ggsave(filename = filename, plot = plots[[i]], width = 8, height = 6)
    } 
    # ----------------------
    # save other objects as .html
    # ----------------------
    else {
      filename <- paste0("Figures/", fun.name, "/", fun.name, "_table_", i, ".html")
      knitr::kable(plots[[i]], format = "html", align = c("l", "r")) %>%
        readr::write_file(filename)
    }
  }
}
