###setup
library(zoo)
round_dates <- as.Date(c("2020-04-03", "2020-06-30", "2020-11-03", "2020-12-12",
                         "2021-02-22", "2021-04-30", "2021-07-19", "2021-10-28","2021-10-29","2022-01-31", "2022-05-13", "2022-08-31"))
all_dates <- seq(as.Date("2020-04-03"), as.Date("2022-08-30"), by="day")


#############
#############the meaningful(true) complete cases
dates_to_plot <- as.Date(c("2020-04-03", "2020-06-30", "2020-11-03", "2020-12-12",
                           "2021-02-22", "2021-04-30", "2021-07-19", "2021-10-28","2022-01-31", "2022-05-13", "2022-08-31"))
kinder <- readRDS("Work.data/kinder.rds")
kinder <- kinder %>%
  mutate(Round = case_when(
    sample_days >= as.Date("2020-04-03") & sample_days <= as.Date("2020-06-30") ~ "R1",
    sample_days >= as.Date("2020-11-03") & sample_days <= as.Date("2020-12-12") ~ "R2",
    sample_days >= as.Date("2021-02-22") & sample_days <= as.Date("2021-04-30") ~ "R3",
    sample_days >= as.Date("2021-07-19") & sample_days <= as.Date("2021-10-28") ~ "R4",
    sample_days >= as.Date("2021-10-29") & sample_days <= as.Date("2022-01-31") ~ "R5",
    sample_days >= as.Date("2022-05-13") & sample_days <= as.Date("2022-08-31") ~ "R6"
  ))
kinder.truecomplete <- kinder
kinder.truecomplete$Round <- as.numeric(sub("R", "", kinder.truecomplete$Round))
id.keep <- vector()
for(round in 1:5) {
  kinder.truecomplete.round <- kinder.truecomplete %>%
    filter(!BewohnerId %in% id.keep) %>%
    mutate(Result = ifelse(Result == "Positive", 1, 0)) %>%
    filter(Round == round) %>%
    filter(!is.na(Result))
  for(i in 1:nrow(kinder.truecomplete.round)) {
    if(kinder.truecomplete.round[i,]$Result == 1) {
      id.keep <- c(id.keep, kinder.truecomplete.round[i,]$BewohnerId)
    }
  }
}
######Only use five localized loops, because the sixth round needs to preserve all non-missing values
kinder.truecomplete.6 <- kinder.truecomplete %>%
  filter(!BewohnerId %in% id.keep) %>%
  mutate(Result = ifelse(Result == "Positive", 1, 0)) %>%
  filter(Round == 6) %>%
  filter(!is.na(Result)) %>%
  distinct(BewohnerId)
id.keep <- c(id.keep, kinder.truecomplete.6$BewohnerId)

true.complete.df <- kinder.truecomplete |>
  filter(BewohnerId %in% id.keep)

true.complete.df.sero <- true.complete.df %>%
  filter(!is.na(Result)) |> 
  mutate(Result = ifelse(Result == "Positive", 1, 0)) %>%
  group_by(BewohnerId) %>%
  mutate(first_positive = ifelse(Result == 1, cumsum(Result) == 1, FALSE)) %>%
  ungroup()

seropos.true.complete <- true.complete.df.sero %>%
  filter(first_positive) %>% 
  group_by(sample_days) %>%
  arrange(sample_days) %>%
  summarize(n_seropos.volle.teilnehmer = sum(Result, na.rm = TRUE))
seropos.true.complete$seroprev <- cumsum(seropos.true.complete$n_seropos.volle.teilnehmer)
seropos.true.complete$total <- length(unique(true.complete.df$BewohnerId))
seropos.true.complete$prop <- seropos.true.complete$seroprev / seropos.true.complete$total
seropos.true.complete.full <- merge(seropos.true.complete, data.frame(sample_days = all_dates), by="sample_days", all=TRUE)
first_non_na <- min(which(!is.na(seropos.true.complete.full$prop)))
if(!is.na(first_non_na) && first_non_na > 1) {
  seropos.true.complete.full$prop[1:(first_non_na-1)] <- 0
}
seropos.true.complete.full$prop <- na.locf(seropos.true.complete.full$prop, na.rm = FALSE)
seropos.true.complete.full <- seropos.true.complete.full |> 
  mutate(Round = case_when(
    sample_days >= as.Date("2020-04-03") & sample_days <= as.Date("2020-06-30") ~ "Round 1",
    sample_days >= as.Date("2020-11-03") & sample_days <= as.Date("2020-12-12") ~ "Round 2",
    sample_days >= as.Date("2021-02-22") & sample_days <= as.Date("2021-04-30") ~ "Round 3",
    sample_days >= as.Date("2021-07-19") & sample_days <= as.Date("2021-10-28") ~ "Round 4",
    sample_days >= as.Date("2021-10-29") & sample_days <= as.Date("2022-01-31") ~ "Round 5",
    sample_days >= as.Date("2022-05-13") & sample_days <= as.Date("2022-08-31") ~ "Round 6"
  ))

last_prop <- max(seropos.true.complete.full$prop)
last_sample_day <- max(seropos.true.complete.full$sample_days)
###plotting
#######we may see some of the kids have observation out of the roundtime. 
#######It's because of the abnormal observation time of the original dataframe.With my method of loop,these sample days are kept.
#######It might be better to replace every observation out of roundtime with NA,
#######But it won't affect the final seroprevalance. 
#######Because if a kid is positive in one pausetime, he will be seropositive in the next round....
seroprev.truecomplete.1 <- seropos.true.complete.full |>
  arrange(sample_days) |> 
  ggplot(aes(x = sample_days, y = prop)) +
  geom_step(lwd = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste0("Seroprävalenz der complete cases über die Zeit (n = ", seropos.true.complete$total, ")"), x = "Zeitverlauf", y = "kumulativer Anteil") +
  scale_x_date(breaks = dates_to_plot, date_labels = "%Y-%m") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "none") +
  scale_fill_manual(values = rep("#4daf4a", 6),
                    labels = c("Runde 1", "Runde 2", "Runde 3", "Runde 4", "Runde 5", "Runde 6"),
                    name = NULL,
                    na.translate = FALSE) +
  geom_rect(aes(xmin = sample_days, xmax = lead(sample_days), ymin = -Inf, ymax = Inf, fill = Round), alpha = 0.4) +
  geom_hline(yintercept = last_prop, linetype = "dashed", color = "grey") +
  geom_text(x = last_sample_day, y = last_prop, label = paste0(round(last_prop, 2) * 100, "%"),
            hjust = 0.8, vjust = 1.8, show.legend = FALSE, size = 5) 


############### complete-cases according to our defination (kids joining in all rounds)
##### filter for each BewohnerId in kinder, keep only BewohnerIds with all 6 Rounds
volle.teilnehmer <- kinder %>%
  filter(!is.na(Anti_N)) %>%
  select(BewohnerId, Round) %>%
  group_by(BewohnerId) %>%
  distinct() %>%
  filter(n() >= 6) %>%
  ungroup() %>%
  select(BewohnerId) %>%
  distinct(BewohnerId, .keep_all = TRUE)

potentiell.volle.teilnehmer.df <- kinder |> 
  filter(!is.na(Anti_N)) |> 
  group_by(BewohnerId, Round) |> 
  summarize(count = n()) |>
  summarize(total = sum(count)) |>
  filter(total >= 6) |> 
  ungroup()

potentiell.volle.teilnehmer <- potentiell.volle.teilnehmer.df$BewohnerId

####### manually filter the kids
volle.teilnehmer.IDs <- c("2157D03", "2712N03", "2735G04", "2775Q03", "3297A03", "3517P03",
                          "3517P04", "3534B04", "3534B05", "3547R04", "3547R05", "3551S02",
                          "3823Q02", "3823Q03", "3866Q03", "4472Z03", "4476S02", "4718N02",
                          "4804F02", "4820G03", "4823H03", "4899Z01", "4911W03", "4926P03",
                          "4965B03", "4994D02", "4998G02", "4999H03", "4999H04", "5017T02",
                          "5036F01", "5136M02", "5338R02", "5362R02", "5381N02", "5397P04",
                          "5398S04", "5407F03")

volle.teilnehmer.df <- kinder |>
  filter(BewohnerId %in% volle.teilnehmer.IDs)

volle.teilnehmer.anzahl <- length(volle.teilnehmer.IDs)



volle.teilnehmer.df.sero <- volle.teilnehmer.df %>%
  filter(!is.na(Result)) |> 
  mutate(Result = ifelse(Result == "Positive", 1, 0)) %>%
  group_by(BewohnerId) %>%
  mutate(first_positive = ifelse(Result == 1, cumsum(Result) == 1, FALSE)) %>%
  ungroup()

seropos.volle.teilnehmer <- volle.teilnehmer.df.sero %>%
  filter(first_positive) %>% 
  group_by(sample_days) %>%
  arrange(sample_days) %>%
  summarize(n_seropos.volle.teilnehmer = sum(Result, na.rm = TRUE))
seropos.volle.teilnehmer$seroprev <- cumsum(seropos.volle.teilnehmer$n_seropos.volle.teilnehmer)
seropos.volle.teilnehmer$total <- length(unique(volle.teilnehmer.df$BewohnerId))
seropos.volle.teilnehmer$prop <- seropos.volle.teilnehmer$seroprev / seropos.volle.teilnehmer$total


### complete "seropos.volle.teilnehmer" with all dates and add Round column.
seropos.volle.teilnehmer.full <- merge(seropos.volle.teilnehmer, data.frame(sample_days = all_dates), by="sample_days", all=TRUE)
first_non_na <- min(which(!is.na(seropos.volle.teilnehmer.full$prop)))
if(!is.na(first_non_na) && first_non_na > 1) {
  seropos.volle.teilnehmer.full$prop[1:(first_non_na-1)] <- 0
}
seropos.volle.teilnehmer.full$prop <- na.locf(seropos.volle.teilnehmer.full$prop, na.rm = FALSE)
seropos.volle.teilnehmer.full <- seropos.volle.teilnehmer.full |> 
  mutate(Round = case_when(
    sample_days >= as.Date("2020-04-03") & sample_days <= as.Date("2020-06-30") ~ "Round 1",
    sample_days >= as.Date("2020-11-03") & sample_days <= as.Date("2020-12-12") ~ "Round 2",
    sample_days >= as.Date("2021-02-22") & sample_days <= as.Date("2021-04-30") ~ "Round 3",
    sample_days >= as.Date("2021-07-19") & sample_days <= as.Date("2021-10-28") ~ "Round 4",
    sample_days >= as.Date("2021-10-29") & sample_days <= as.Date("2022-01-31") ~ "Round 5",
    sample_days >= as.Date("2022-05-13") & sample_days <= as.Date("2022-08-31") ~ "Round 6"
  ))

last_prop <- max(seropos.volle.teilnehmer.full$prop)
last_sample_day <- max(seropos.volle.teilnehmer.full$sample_days)

###plotting
seroprev.volle.1 <- seropos.volle.teilnehmer.full |>
  arrange(sample_days) |> 
  ggplot(aes(x = sample_days, y = prop)) +
  geom_step(lwd = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste0("Seroprävalenz der complete cases über die Zeit (n = ", volle.teilnehmer.anzahl, ")"), x = "Zeitverlauf", y = "kumulativer Anteil") +
  scale_x_date(breaks = dates_to_plot, date_labels = "%Y-%m") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "none") +
  scale_fill_manual(values = rep("#4daf4a", 6),
                    labels = c("Runde 1", "Runde 2", "Runde 3", "Runde 4", "Runde 5", "Runde 6"),
                    name = NULL,
                    na.translate = FALSE) +
  geom_rect(aes(xmin = sample_days, xmax = lead(sample_days), ymin = -Inf, ymax = Inf, fill = Round), alpha = 0.4)

seroprev.volle.2 <- seroprev.volle.1 +
  geom_hline(yintercept = last_prop, linetype = "dashed", color = "grey") +
  geom_text(x = last_sample_day, y = last_prop, label = paste0(round(last_prop, 2) * 100, "%"),
            hjust = 0.8, vjust = 1.8, show.legend = FALSE, size = 5) 

seroprev.volle.3 <- seroprev.volle.2 +
  geom_vline(xintercept = as.Date("2021-11-29"), color = "black") +
  annotate("text", x = as.Date("2022-02-10"), y = 0.25, label = "Omikron", size = 5, color = "black")
