### setup
library(zoo)
kinder <- readRDS("Work.Data/kinder.rds")

# create round columns based on the official dates
kinder <- kinder %>%
  mutate(Round = case_when(
    sample_days >= as.Date("2020-04-03") & sample_days < as.Date("2020-06-30") ~ "R1",
    sample_days > as.Date("2020-11-03") & sample_days < as.Date("2020-12-12") ~ "R2",
    sample_days > as.Date("2021-02-22") & sample_days < as.Date("2021-04-30") ~ "R3",
    sample_days > as.Date("2021-07-19") & sample_days < as.Date("2021-10-28") ~ "R4",
    sample_days > as.Date("2021-10-29") & sample_days < as.Date("2022-01-31") ~ "R5",
    sample_days > as.Date("2022-05-13") & sample_days < as.Date("2022-08-31") ~ "R6"
  ))

round_dates <- as.Date(c("2020-04-03", "2020-06-30", "2020-11-03", "2020-12-12",
                         "2021-02-22", "2021-04-30", "2021-07-19", "2021-10-28",
                         "2021-10-29","2022-01-31", "2022-05-13", "2022-08-31"))
all_dates <- seq(as.Date("2020-04-03"), as.Date("2022-08-30"), by="day")

# dates to plot a bit different from round_dates
dates_to_plot <- as.Date(c("2020-04-03", "2020-06-30", "2020-11-03", "2020-12-12",
                           "2021-02-22", "2021-04-30", "2021-07-19", "2021-10-28",
                           "2022-01-31", "2022-05-13", "2022-08-31"))

### preparations for the imputation
kinder <- kinder %>%
  select(BewohnerId, Round, sample_date, sample_days, Result)
# Number of distinct study participants
n_participants <- length(unique(kinder$BewohnerId))
# Extract distinct participant IDs
all.study.participants <- kinder %>%
  distinct(BewohnerId)
# Sample participant IDs
participant.ids <- all.study.participants$BewohnerId
# Create a data frame with all combinations of participant IDs and rounds
all_combinations <- crossing(BewohnerId = participant.ids, Round = unique(kinder$Round)) |> 
  filter(!is.na(Round))
# Perform a left join to identify missing entries
result_data <- left_join(all_combinations, kinder, by = c("BewohnerId", "Round"))
# Create a numeric Result column in which Positive equals 1
result_data <- result_data |> 
  mutate(Result = ifelse(Result == "Positive", 1, 0))

# sanity check: There should be 6*436 entries but since some participants have multiple 
# entries for one round there are some more entries.


# Calculate proportions of seropositive patient over the rounds (serves as pi)
kinder_filtered  <- kinder %>%
  filter(!is.na(Result)) |> 
  group_by(Round) |> 
  mutate(Result = ifelse(Result == "Positive", 1, 0))

pos.proportions <- kinder_filtered   |>
  group_by(Result, Round) |>
  summarize(count = n()) |>
  mutate(prop = count / sum(count)) |> 
  filter(Result == 1) |> 
  filter(!is.na(Round)) |> 
  arrange(Round)

n_per_round <- kinder_filtered   |>
  group_by(Result, Round) |>
  summarize(count = n()) |> 
  group_by(Round) |> 
  summarise(sum_count = sum(count))

plot_data_list <- list()
for(j in 1: 50){
## Imputation process for the Result column
# Initialize an empty data frame to store the results
imputed_data <- data.frame()
# Iterate over each round
for (i in seq_along(pos.proportions$Round)) {
  current_round <- pos.proportions$Round[i]
  current_probability <- pos.proportions$prop[i]
  # Filter the data for the current round
  current_round_data <- result_data %>%
    filter(Round == current_round) %>%
    arrange(sample_days) |> 
    group_by(BewohnerId) %>%
    mutate(Result = ifelse(is.na(Result), rbinom(1, 1, current_probability), Result)) %>%
    ungroup()
  # Combine the imputed values for the current round into the overall results
  imputed_data <- bind_rows(imputed_data, current_round_data)
}

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
      interval_list[[length(interval_list) + 1]] <- list(interval = current_interval,
                                                         length = length(current_interval))
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
  imputed_data_current_round <- imputed_data %>%
    filter(Round == current_round) %>%
    group_by(BewohnerId) %>%
    mutate(sample_days = if_else(is.na(sample_days), random_date, sample_days))
  # Append the imputed data for the current round to the result data frame
  imputed_data_result <- bind_rows(imputed_data_result, imputed_data_current_round)
}
imputed_data <- imputed_data_result


### Preparition for plotting
imputed_data_sero <- imputed_data %>%
  arrange(sample_days) |> 
  group_by(BewohnerId) %>%
  mutate(first_positive = ifelse(Result == 1, cumsum(Result) == 1, FALSE)) %>%
  ungroup() 

imputed_seropos <- imputed_data_sero%>%
  filter(first_positive) %>% 
  group_by(sample_days) %>%
  arrange(sample_days) %>%
  summarize(n_seropos = sum(Result, na.rm = TRUE))

imputed_seropos$seroprev <- cumsum(imputed_seropos$n_seropos)
imputed_seropos$total <- length(unique(kinder$BewohnerId))
imputed_seropos$prop <- imputed_seropos$seroprev / imputed_seropos$total
imputed_seropos.full <- merge(imputed_seropos, data.frame(sample_days = all_dates), by="sample_days", all=TRUE)
first_non_na <- min(which(!is.na(imputed_seropos.full$prop)))

if(!is.na(first_non_na) && first_non_na > 1) {
  imputed_seropos.full$prop[1:(first_non_na-1)] <- 0
}

imputed_seropos.full$prop <- na.locf(imputed_seropos.full$prop, na.rm = FALSE)

imputed_seropos.full <- imputed_seropos.full |> 
  mutate(Round = case_when(
    sample_days >= as.Date("2020-04-03") & sample_days <= as.Date("2020-06-30") ~ "Round 1",
    sample_days >= as.Date("2020-11-03") & sample_days <= as.Date("2020-12-12") ~ "Round 2",
    sample_days >= as.Date("2021-02-22") & sample_days <= as.Date("2021-04-30") ~ "Round 3",
    sample_days >= as.Date("2021-07-19") & sample_days <= as.Date("2021-10-28") ~ "Round 4",
    sample_days >= as.Date("2021-10-29") & sample_days <= as.Date("2022-01-31") ~ "Round 5",
    sample_days >= as.Date("2022-05-13") & sample_days <= as.Date("2022-08-31") ~ "Round 6"
  ))
plot_data_list[[j]] <- imputed_seropos.full
}
all_data <- bind_rows(plot_data_list, .id = "Iteration")

saveRDS(all_data, "Work.Data/all_data.rds")