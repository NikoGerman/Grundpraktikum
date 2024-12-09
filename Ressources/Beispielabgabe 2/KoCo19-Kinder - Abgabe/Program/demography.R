
########### Altersverteilung
age.start <- kinder.full %>%
  group_by(BewohnerId) %>%
  slice(which.min(sample_days)) 
# Erstellen des Histogramms für die Altersverteilung bei der Rekrutierung
Altersverteilung <- ggplot(age.start, aes(x = Age_start, after_stat(density))) +
  geom_histogram(binwidth = 1, fill ="#377eb8", color = "black", alpha = 0.7) +
  scale_x_continuous(breaks = seq(min(age.start$Age_start, na.rm = TRUE), 
                                  max(age.start$Age_start, na.rm = TRUE), by = 1)) + 
  scale_y_continuous(labels = label_percent()) +
  labs(x = "Alter", y = "Anteil", 
       title = "Altersverteilung bei der Rekrutierung (n = 436)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        # remove vertical grid
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave("Altersverteilung.png", path = "Results/", width = 5, height = 5,
       device='png', dpi=300)