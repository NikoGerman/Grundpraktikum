
# ----------------------
# save VGR graph as .png
# ----------------------
graph_VGR() %>%
  rsvg::rsvg_png(file = "Figures/Appendix/graphVGR.png", height = 1440)

# ----------------------
# save spearman examples as .png
# ----------------------
spearman_examples() %>%
  ggsave(filename = "Figures/Appendix/spearman_examples.png", device = "png")

# ----------------------
# save plots Q1 - Q5
# Note: throws loads of warnings from every call of ggplot
# ----------------------

for(k in seq_len(5)) {
  fun <- paste0("Q", k)
  cat(sprintf("saving plots of Question %d ...\n", k))
  plots <- do.call(fun, args = list())
  for (i in 1:length(plots)) {
    if ("gg" %in% class(plots[[i]])) {
      filename <- paste0("Figures/", fun, "/", fun, "_plot_", i, ".png")
      ggsave(filename = filename, plot = plots[[i]], width = 8, height = 6)
    } else {
      filename <- paste0("Figures/", fun, "/", fun, "_table_", i, ".html")
      knitr::kable(plots[[i]], format = "html", align = c("l", "r")) %>%
        readr::write_file(filename)
    }
  }
}
