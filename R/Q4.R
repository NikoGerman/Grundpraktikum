Q4 <- function(data, country_colors = NULL, method = "save") {
  # ----------------------
  # check properties of input
  # ----------------------
  checkmate::assertDataFrame(data, col.names = "named")
  checkmate::assertCharacter(country_colors, null.ok = TRUE)
  checkmate::assertCharacter(method, len = 1, any.missing = FALSE)
  checkmate::assertSubset(method, choices = c("save", "return"))
  
  # ----------------------
  # Question:
  #   - How does GDP per capita relate to the prevalence 
  #     of current tobacco use (% of adults)?
  # ----------------------
  
  # ----------------------
  # if not provided, generate country_colors
  # ----------------------
  if (is.null(country_colors)) {
    country_colors <- assignCountryColors(data)
  }
  
  # ----------------------
  # missingness:
  #   plot Missingness of 
  #     - Prevalence of tobacco use
  #     by Country/Year
  #   - for plot.missing() see utils.R
  # ----------------------
  missingness <- data %>% plot.missing(x1 = "Prevalence_of_current_tobacco_use_(%_of_adults)") +
    ggtitle("Beobachtungen zur Prävalenz des Tabakkonsums")
  
  # ----------------------
  # plot_basic:
  #   as the name says, basic scatterplot of
  #     x: GDP per capita
  #     y: Prevalence of tobacco use
  #     color: Country
  # ----------------------
  plot_basic <- data %>%
    ggplot(aes(x = `GDP_per_capita_PPP_(constant_2021_international_$)`,
               y = `Prevalence_of_current_tobacco_use_(%_of_adults)`,
               color = Country_Name)) +
    geom_point(size = 2) +
    scale_color_manual(values = country_colors, guide = "none") +
    labs(x = "BIP pro Kopf",
         y = "Prävalenz des Tabakkonsums") +
    scale_x_continuous(labels = scales::label_number(suffix = "$")) +
    scale_y_continuous(labels = scales::label_number(suffix = "%"))
  
  # ----------------------
  # plot1:
  #   takes plot_basic,
  #     - facetting by Continent
  #     - changing x to logarithmic scale, to account for Katar
  #     - puts a text label on the first observation of each country
  # ----------------------
  plot1 <- plot_basic +
    geom_smooth(se = TRUE, color = "grey", method = "lm", alpha = .15) +
    facet_wrap(~Continent) +
    scale_x_log10(labels = scales::label_number(suffix = "$"), limits = c(1000, 200000)) +
    geom_text_repel(data = (data %>% 
                              group_by(Country_Name) %>%
                              slice_min(order_by = Year) %>%
                              ungroup()),
                    aes(label = Country_Name),
                    size = 3, max.overlaps = 5, force = .7)

  # ----------------------
  # plot2_basic:
  #   takes plot_basic,
  #     - leaves only the countries which are NOT labelled as "High income"
  #     - puts a text label on the first observation of each country
  # ----------------------
  plot2_basic <- plot_basic %+% (data %>%
                    filter(Income_Group != "Hohes Volkseinkommen")
           ) +
    geom_text_repel(data = (data %>% 
                              group_by(Country_Name) %>%
                              slice_min(order_by = Year) %>%
                              ungroup() %>%
                              filter(Income_Group != "Hohes Volkseinkommen")
                            ),
                    aes(label = Country_Name),
                    size = 3, max.overlaps = 10)
 
  # ----------------------
  # plot2_part1:
  #   takes plot2_basic,
  #     - adds a linear regression line for each country
  #     - does NOT add confidence bands around the regression lines
  # ----------------------
  plot2_part1 <- plot2_basic +
    geom_smooth(aes(group = Country_Name, color = Country_Name),se = FALSE, method = "lm") +
    labs(caption = "Zur besseren Lesbarkeit wurde auf die Konfidenzintervalle
         der einzelnen Regressionsgeraden verzichtet")
  
  # ----------------------
  # plot2_part2:
  #   takes plot2_basic,
  #     - adds a linear regression line on global level
  # ----------------------
  plot2_part2 <- plot2_basic +
    geom_smooth(se = TRUE, color = "grey", method = "lm", alpha = .15)
  
  # ----------------------
  # plot2:
  #   using patchwork,
  #     - displays _part1 and _part2 side by side
  # ----------------------
  plot2 <- (plot2_part2 | plot2_part1) + plot_layout(axes = "collect") + 
    plot_annotation(title = "Länder mit niedrigen und mittleren Einkommen")
    
  # ----------------------
  # plot3_basic:
  #   takes plot_basic,
  #     - leaves only the countries which are labelled as "High income"
  #     - puts a text label on the first observation of each country
  # ----------------------
  plot3_basic <- plot_basic %+% (data %>%
                            filter(Income_Group == "Hohes Volkseinkommen")
    ) +
    geom_text_repel(data = (data %>% 
                      group_by(Country_Name) %>%
                      slice_min(order_by = Year) %>%
                      ungroup() %>%
                      filter(Income_Group == "Hohes Volkseinkommen")
                      ),
                    aes(label = Country_Name),
                    size = 3, max.overlaps = 10)
  
  # ----------------------
  # plot3_part1:
  #   takes plot3_basic,
  #     - adds a linear regression line for each country
  #     - does NOT add confidence bands around the regression lines
  # ----------------------
  plot3_part1 <- plot3_basic +
    geom_smooth(aes(group = Country_Name, color = Country_Name),se = FALSE, method = "lm") +
    labs(caption = "Zur besseren Lesbarkeit wurde auf die Konfidenzintervalle
         der einzelnen Regressionsgeraden verzichtet")
  
  # ----------------------
  # plot3_part2:
  #   takes plot3_basic,
  #     - adds a linear regression line on global level
  # ----------------------
  plot3_part2 <- plot3_basic +
    geom_smooth(se = TRUE, color = "grey", method = "lm", alpha = .15)
  
  # ----------------------
  # plot3:
  #   using patchwork,
  #     - displays _part1 and _part2 side by side
  # ----------------------
  plot3 <- (plot3_part2 | plot3_part1) + plot_layout(axes = "collect") + 
    plot_annotation(title = "Länder mit hohen Einkommen")
  
  # ----------------------
  # save plots as named list
  # ----------------------
  result <- list(
    missingness = missingness,
    plot1 = plot1,
    plot2 = plot2,
    plot3 = plot3
    )
  
  # ----------------------
  # save Figures if method is set to "save"
  # for save.figures, see utils
  # WARNING
  # method needs to be "return" here,
  # other ways it would create an infinity loop
  # ----------------------
  if (method == "save") {
    save.figures(Q4, list(data = data,
                          country_colors = country_colors,
                          method = "return"))
  } else {
    # ----------------------
    # return result
    # ----------------------
    return(result)
  }
}
