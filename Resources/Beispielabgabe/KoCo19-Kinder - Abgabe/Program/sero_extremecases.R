### Setup
library(zoo)
round_dates <- as.Date(c("2020-04-03", "2020-06-30", "2020-11-03", "2020-12-12",
                         "2021-02-22", "2021-04-30", "2021-07-19", "2021-10-28",
                         "2021-10-29","2022-01-31", "2022-05-13", "2022-08-31"))
all_dates <- seq(as.Date("2020-04-03"), as.Date("2022-08-30"), by="day")

### Preparations for the imputation
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
n_participants <- length(unique(kinder$BewohnerId))
# Extract distinct participant IDs
all.study.participants <- kinder %>%
  distinct(BewohnerId)
# Sample participant IDs (replace this with your actual vector)
participant.ids <- all.study.participants$BewohnerId
# Create a data frame with all combinations of participant IDs and rounds
all_combinations <- crossing(BewohnerId = participant.ids, Round = unique(kinder$Round)) |> 
  filter(!is.na(Round))
# Perform a left join to identify missing entries
result_data <- left_join(all_combinations, kinder, by = c("BewohnerId", "Round"))
# create a numeric Result column in which Positive equals 1
kinder.worst <- result_data %>%
  mutate(Result = ifelse(is.na(Result), "Positive", Result))

# Generate the imputed data by iterating over every second entry in round_dates to create intervals
interval_list <- list()
for (i in seq(1, length(round_dates), by = 2)) {
  start_date <- round_dates[i]
  # Check if there's an end date available
  if (i + 1 <= length(round_dates)) {
    end_date <- round_dates[i + 1]
    # Check if end_date is NA
    if (!is.na(end_date)) {
      # Generate a sequence of dates from start_date to end_date
      current_interval <- seq(start_date, end_date, by = "day")
      # Add the current interval and its length to the list
      interval_list[[length(interval_list) + 1]] <- list(interval = current_interval, length = length(current_interval))
    }
  }
}

# Impute the missing sample_days
round_labels <- c("R1", "R2", "R3", "R4", "R5", "R6")
# Create a named list to store intervals
interval_list_named <- setNames(interval_list, paste0(round_labels, "_interval"))
# Initialize a new data frame to store the imputed values
imputed_data_result <- data.frame()
for (i in seq_along(round_labels)) {
  current_round <- round_labels[i]
  current_interval <- interval_list_named[[paste0(current_round, "_interval")]]
  # Draw a random sample of one day from the current interval
  random_date <- as.Date(sample(current_interval$interval, 1))
  # Impute missing values for the current round
  imputed_data_current_round <- kinder.worst %>%
    filter(Round == current_round) %>%
    group_by(BewohnerId) %>%
    mutate(sample_days = if_else(is.na(sample_days), random_date, sample_days))
  # Append the imputed data for the current round to the result data frame
  imputed_data_result <- bind_rows(imputed_data_result, imputed_data_current_round)
}
imputed_data <- imputed_data_result

kinder.worst.sero <- imputed_data %>%
  mutate(Result = ifelse(Result == "Positive", 1, 0)) %>%
  group_by(BewohnerId) %>%
  mutate(first_positive = ifelse(Result == 1, cumsum(Result) == 1, FALSE)) %>%
  ungroup()


seropos.worst <- kinder.worst.sero %>%
  filter(first_positive) %>% 
  group_by(sample_days) %>%
  arrange(sample_days) %>%
  summarize(n_seropos.worst = sum(Result, na.rm = TRUE))
seropos.worst$seroprev <- cumsum(seropos.worst$n_seropos.worst)
seropos.worst$total <- length(unique(kinder.worst.sero$BewohnerId))
seropos.worst$prop <- seropos.worst$seroprev / seropos.worst$total

### complete "seropos.worst" with all dates and add Round column.
seropos.worst.full <- merge(seropos.worst, data.frame(sample_days = all_dates), by="sample_days", all=TRUE)
first_non_na <- min(which(!is.na(seropos.worst.full$prop)))
if(!is.na(first_non_na) && first_non_na > 1) {
  seropos.worst.full$prop[1:(first_non_na-1)] <- 0
}
seropos.worst.full$prop <- na.locf(seropos.worst.full$prop, na.rm = FALSE)
seropos.worst.full <- seropos.worst.full |> 
  mutate(Round = case_when(
    sample_days >= as.Date("2020-04-03") & sample_days < as.Date("2020-06-30") ~ "Round 1",
    sample_days > as.Date("2020-11-03") & sample_days < as.Date("2020-12-12") ~ "Round 2",
    sample_days > as.Date("2021-02-22") & sample_days < as.Date("2021-04-30") ~ "Round 3",
    sample_days > as.Date("2021-07-19") & sample_days < as.Date("2021-10-28") ~ "Round 4",
    sample_days > as.Date("2021-10-29") & sample_days < as.Date("2022-01-31") ~ "Round 5",
    sample_days > as.Date("2022-05-13") & sample_days < as.Date("2022-08-31") ~ "Round 6"
  )) 

last_prop_worst <- max(seropos.worst.full$prop)
last_sample_day_worst <- max(seropos.worst.full$sample_days)

#####################
### add first_positive column for "kinder",create "seropos" with days seroprevalance raising.

kinder.sero <- kinder %>%
  filter(!is.na(Result)) |> 
  mutate(Result = ifelse(Result == "Positive", 1, 0)) %>%
  group_by(BewohnerId) %>%
  mutate(first_positive = ifelse(Result == 1, cumsum(Result) == 1, FALSE)) %>%
  ungroup()

seropos <- kinder.sero %>%
  filter(first_positive) %>% 
  group_by(sample_days) %>%
  arrange(sample_days) %>%
  summarize(n_seropos = sum(Result, na.rm = TRUE))
seropos$seroprev <- cumsum(seropos$n_seropos)
seropos$total <- length(unique(kinder$BewohnerId))
seropos$prop <- seropos$seroprev / seropos$total

### complete "seropos" with all dates and add Round column.
seropos.full <- merge(seropos, data.frame(sample_days = all_dates), by="sample_days", all=TRUE)
first_non_na <- min(which(!is.na(seropos.full$prop)))
if(!is.na(first_non_na) && first_non_na > 1) {
  seropos.full$prop[1:(first_non_na-1)] <- 0
}
seropos.full$prop <- na.locf(seropos.full$prop, na.rm = FALSE)
seropos.full <- seropos.full |> 
  mutate(Round = case_when(
    sample_days >= as.Date("2020-04-03") & sample_days < as.Date("2020-06-30") ~ "Round 1",
    sample_days > as.Date("2020-11-03") & sample_days < as.Date("2020-12-12") ~ "Round 2",
    sample_days > as.Date("2021-02-22") & sample_days < as.Date("2021-04-30") ~ "Round 3",
    sample_days > as.Date("2021-07-19") & sample_days < as.Date("2021-10-28") ~ "Round 4",
    sample_days > as.Date("2021-10-29") & sample_days < as.Date("2022-01-31") ~ "Round 5",
    sample_days > as.Date("2022-05-13") & sample_days < as.Date("2022-08-31") ~ "Round 6"
  )) 

last_prop <- max(seropos.full$prop)
last_sample_day <- max(seropos.full$sample_days)



###############
###############
###############
###############combined Plot with best and worst
# Add a new column to each dataset to differentiate them
seropos.full <- seropos.full %>% mutate(Dataset = "Best")
seropos.worst.full <- seropos.worst.full %>% mutate(Dataset = "Worst")

sero.excases <- bind_rows(seropos.full, seropos.worst.full)


sero.excases <- sero.excases %>% rename (Iteration = Dataset)

sero.excases <- sero.excases %>% 
  select(Iteration, everything()) %>%
  select(-n_seropos.worst)

saveRDS(sero.excases, "Work.Data/sero.excases.rds")