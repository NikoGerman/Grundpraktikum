###setup
source("Program/sero_extremecases.R")
### it's fast to source sero_extremecases, and the easiest way to obtain some variables...
kinder <- readRDS("Work.Data/kinder.rds")
kinder <- kinder %>%
  mutate(Round = case_when(
    sample_days >= as.Date("2020-04-03") & sample_days < as.Date("2020-06-30") ~ "R1",
    sample_days > as.Date("2020-11-03") & sample_days < as.Date("2020-12-12") ~ "R2",
    sample_days > as.Date("2021-02-22") & sample_days < as.Date("2021-04-30") ~ "R3",
    sample_days > as.Date("2021-07-19") & sample_days < as.Date("2021-10-28") ~ "R4",
    sample_days > as.Date("2021-10-29") & sample_days < as.Date("2022-01-31") ~ "R5",
    sample_days > as.Date("2022-05-13") & sample_days < as.Date("2022-08-31") ~ "R6"
  ))
library(zoo)
round_dates <- as.Date(c("2020-04-03", "2020-06-30", "2020-11-03", "2020-12-12",
                         "2021-02-22", "2021-04-30", "2021-07-19", "2021-10-28","2021-10-29","2022-01-31", "2022-05-13", "2022-08-31"))
all_dates <- seq(as.Date("2020-04-03"), as.Date("2022-08-30"), by="day")
### dates to plot a bit different from round_dates
dates_to_plot <- as.Date(c("2020-04-03", "2020-06-30", "2020-11-03", "2020-12-12",
                           "2021-02-22", "2021-04-30", "2021-07-19", "2021-10-28","2022-01-31", "2022-05-13", "2022-08-31"))
all_data <- readRDS("Work.Data/all_data.rds")
sero.excases <- readRDS("Work.Data/sero.excases.rds")

round_data <- all_data %>%
  group_by(Round) %>%
  summarise(Start = min(sample_days), End = max(sample_days)) %>%
  filter(!is.na(Round))

colors <- # repeate grey 200 times and one red one #377eb8
  rep("black", 52)

# find max prop in all_data
max_prop_imp <- max(all_data$prop, na.rm = TRUE)
# find min prop in all data while Round == Round 6 and sample_days >= 2022-08
mean_prop_imp <- mean(all_data$prop[all_data$Round == "Round 6" & all_data$sample_days >= as.Date("2022-08-28")], na.rm = TRUE)


###plotting
seroprevimp1 <- ggplot() +
  geom_step(data = sero.excases, aes(x = sample_days, y = prop, color = Iteration), lwd = 1) +
  scale_color_manual(values = c("black", "black")) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  labs(title = "Seroprävalenz von Kindern über die Zeit",
       subtitle = "50 Iterationen    n = 436",
       x = "Zeitverlauf", y = "kumulativer Anteil") +
  scale_x_date(breaks = dates_to_plot, date_labels = "%Y-%m") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "none") +
  geom_rect(data = round_data, aes(xmin = Start, xmax = End, ymin = -Inf, ymax = Inf, fill = Round), alpha = 0.4) +
  scale_fill_manual(values = rep("#4daf4a", 6),
                    labels = c("Runde 1", "Runde 2", "Runde 3", "Runde 4", "Runde 5", "Runde 6"),
                    name = NULL,
                    na.translate = FALSE)
  

seroprevimp3 <- seroprevimp1 +
  geom_text(data = sero.excases, x = last_sample_day, y = last_prop, label = paste0(round(last_prop, 2) * 100, "%"),
            hjust = 0.8, vjust = 1.8, show.legend = FALSE, size = 4) +
  geom_text(data = sero.excases, x = last_sample_day_worst, y = last_prop_worst, label = paste0(round(last_prop_worst, 2) * 100, "%"),
            hjust = 0.8, vjust = 1.8, show.legend = FALSE, size = 4)

seroprevimp4 <- seroprevimp3 +
  geom_step(data = all_data, aes(x = sample_days, y = prop, color = Iteration),
            alpha = 0.2) +
  scale_color_manual(values = colors) +
  geom_text(data = all_data, x = last_sample_day, y = max_prop_imp + 0.1, label = paste0("Mittel: ",
                                                                                          round(mean_prop_imp, 2) * 100, "%"),
            hjust = 0.8, vjust = 1.8, show.legend = FALSE, size = 4, color = "black")

seroprevimp5 <- seroprevimp4 +
  geom_vline(xintercept = as.Date("2022-03-01"), color = "black") +
  annotate("text", x = as.Date("2022-01-01"), y = 0.5, label = "Omikron", size = 4, color = "black")


ggsave("seroprevimputated.png", path = "Results/", width = 9, height = 7,
       device='png', dpi=300)