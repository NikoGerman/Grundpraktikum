kinder <- readRDS("Work.Data/kinder.rds")
#### Seropositivität über Runden
kinder.sero.r <- kinder %>%
  filter(!is.na(Result)) |> 
  group_by(Round) |> 
  mutate(Result = ifelse(Result == "Positive", 1, 0)) %>%
  group_by(BewohnerId) %>%
  mutate(first_positive = ifelse(Result == 1, cumsum(Result) == 1, FALSE)) %>%
  ungroup()

proportions.r <- kinder.sero.r |>
  group_by(Result, Round) |>
  summarize(count = n()) |>
  mutate(prop = count / sum(count))

n_per_round <- kinder.sero.r |>
  group_by(Result, Round) |>
  summarize(count = n()) |> 
  group_by(Round) |> 
  summarise(sum_count = sum(count))


### Plot
merged_data <- merge(proportions.r, n_per_round, by = "Round")


positive_data <- merged_data |> 
  filter(Result == 1) |>
  filter(!is.na(Round))


round_labels <- merged_data %>%
  arrange(Round) %>%
  distinct(Round, .keep_all = TRUE) %>%
  mutate(x_label = paste(Round, "\nn =", sum_count)) %>%
  pull(x_label)


seropos.1 <- ggplot(positive_data, aes(x = Round, y = prop)) +
  geom_col(fill = "#4daf4a", color = "black", alpha = 0.4) +
  labs(title = "Proportion der seropositiven Kinder über die Runden",
       x = "Runde",
       y = "Anteil") +
  scale_y_continuous(labels = label_percent()) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  scale_x_discrete(labels = round_labels) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# get bar stats
bar_stats <- positive_data |>
  group_by(Round) |>
  summarize(mean = round(mean(prop) * 100, 1)) |>
  mutate(label = paste0(mean, "%"))

#seropos.2 <- seropos.1 +
#  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels = label_percent()) +
#  geom_hline(yintercept = 0.071, color = "black", linetype = "dashed", size = 1)

seropos.3 <- seropos.1 +
  # show bar stats on top of each bars
  geom_text(data = bar_stats, aes(x = Round, y = mean/100, label = label),
            vjust = -0.5, color = "black") +
  ylim(0, 0.52)

ggsave("seropos.1.png", path = "Results/", width = 6, height = 5,
       device='png', dpi=300)

