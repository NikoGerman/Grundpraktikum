## This program cleans the original data sets and saves modified data sets for
## future utilization.

## OUTLINE
# Data Preparation
  # Load Data
  # Initial NA Removal
  # Data Overview

# Data Modification
  # Renaming and Selecting Columns
  # Value Labeling
  # Addressing Data Inconsistencies

# Advanced Data Processing
  # Age Calculations
  # Date Formatting and Outlier Identification
  # Pivoting Adult Data for Longitudinal Analysis
  # Vaccine Date Integration

# Data Type Adjustments and Validation
  # Type Conversion
  # Date Validation

# Data Merging and Cleaning
  # Merging Datasets
  # Filtering and Adjusting for Specific Groups
  # Round Assignment

# Final Steps
  # Save Cleaned Datasets for Future Use

library(tidyr)
library(dplyr)
library(stringr)
library(checkmate)
library(lubridate)
source("Program/functions.R")

# Read the csv files from Data folder
kinder <- read.csv("Data/Data_kids_Practikum2324.csv", sep = ",", header = TRUE)
adult <- read.csv("Data/data_koco19_round6.csv", sep = ",", header = TRUE)

# delete columns or rows if they are all NAs.
# apply the remove_NAs function to all interested data sets
kinder <- remove_NAs(kinder)
adult <- remove_NAs(adult)
# Get the overview of the data
str(kinder)
summary(kinder)
str(adult)
summary(adult)

# Modification to data sets, simplifying their joining
adult <- adult %>%
  rename(HaushaltsId = hh_id, BewohnerId = ind_id,
         birthday = Q6_f1_Geburtsdatum_DDMONYYYY) # colnames aligning to kinder
kinder <- kinder %>%
  rename(Sex = Q6_d_Geschlecht, birthday = Q6_f1_Geburtsdatum_DDMONYYYY) %>%
  select(-X) # shortening long var.names and delete unnecessary index variable

# Assign value Labels (checked by comparing with adult)
kinder$Sex <- factor(kinder$Sex, levels = c(0, 1), labels = c("Female", "Male"))

# Examination of data shows further problems: in kinder,
# for each BewohnerId, only some rows of his/her Sex, birthday have values,
# others are NA. Obviously, for each individuals, these variables
# should have the same value. Here we need to assign the values accordingly.

# Apply the fill_na_with_mode function to kinder
kinder <- fill_na_with_mode(kinder, "BewohnerId", c("Sex", "birthday"))

# Give kinder Age variable for the start of research
# age categories (to be discussed)
age_breaks <- c(0, 10, 14, 16, 18, 20, 22, Inf)
age_labels <- c("<10", "10-14", "14-16", "16-18", "18-20", "20-22", ">22")

# Add an Age and Age group column to kinder
# kinder also need dynamic age variables for the development of rounds.
kinder <- kinder %>%
  mutate("Age" = interval(birthday, as.Date(sample_date)) %/% years(1)) %>%
  # mutate a new variable Age_start, which is the first Age for each BewohnerId
  group_by(BewohnerId) %>%
  mutate(Age_start = min(Age)) %>%
  mutate(AgeGroup = cut(Age, breaks = age_breaks, labels = age_labels,
                         include.lowest = TRUE, right = FALSE)) %>%
  select(contains("Id"), Sex, birthday, Age_start, sample_date, Age, AgeGroup, everything())

# Convert date in kinder and sample_date in adult to YYYY-MM (delete days and keep year and month)
kinder$sample_date <- as.Date(kinder$sample_date)
kinder$sample_days <- kinder$sample_date
kinder$sample_date <- format(kinder$sample_date, "%Y-%m")

# Mark DBS_value_1 < 0.105 as "Negative" or else "Positive", according to cut-off given by the partner
# In a new column result

kinder <- kinder %>%
  mutate(Result = ifelse(DBS_value_1 < 0.105, "Negative", "Positive")) %>%
# Rename DBS_value_1 to Anti_N, and Ro.RBD.ig.quant.DBS to Anti_S
  rename(Anti_N = DBS_value_1, Anti_S = Ro.RBD.Ig.quant.DBS) %>%
# Keeping only the necessary columns in kinder, and reorder them
  select(HaushaltsId, BewohnerId, LabId, Sex, birthday, Age_start, sample_date, sample_days,
         Age, AgeGroup, Result, Anti_N, Anti_S)

# Adult: longitudinal pivoting

### Motivation: connecting observations from both kinder and adult provides more
### information about the adult samples.
### Goal: each round of sampling should be a separate row, keeping all other
### socio-demographic and organizational information.
### However, not all dates in teens have matches in kinder.

# There are two variable name patterns that need to be pivoted:
### for example: date_R1, R1_result. Thus pivot separately.

# First pivot for columns with 'R*_' pattern
adult.results.long <- adult %>%
  select(BewohnerId, starts_with("R")) %>%
  pivot_longer(
    cols = -BewohnerId,
    names_to = c("Round", ".value"),
    names_pattern = "(R\\d+)_(.*)"
  )

# Second pivot for columns with 'date_' pattern
adult.dates.long <- adult %>%
  select(BewohnerId, matches("^date_R")) %>%
  pivot_longer(
    cols = -BewohnerId,
    names_to = "Round",
    names_pattern = "^date_(R\\d+)$",
    values_to = "sample_date"
  ) %>%
  # Ensure consistent naming for 'Round' after pivot
  mutate(Round = str_replace(Round, "^date_", ""))

# Now join the results and dates together
adult.long <- adult.results.long %>%
  left_join(adult.dates.long, by = c("BewohnerId", "Round"))

# Convert sample_date to YYYY-MM, delete days and keep only months and years
adult.long$sample_date <- as.Date(adult.long$sample_date)
adult.long$sample_days <- adult.long$sample_date
adult.long$sample_date <- format(adult.long$sample_date, "%Y-%m")

# obviously, some cols in adult were dropped. Reattach them here.
adult.dropped <- adult %>%
  select(-matches("^date_R\\d+|^R\\d+_"))
adult.long <- adult.long %>%
  left_join(adult.dropped, by = "BewohnerId") %>%
  select(HaushaltsId, BewohnerId, LabId, Sex, Birth_Country, birthday, Age,
         smokestatus, any_risk_emp,
         sample_date, Round, Result, quant, everything())

# after examination of the data, it has been proved
# (see proof_of_vaccine_info_integration.R)
# that all Vaccine_Date1 and Vaccine_Date2 from adult
# can be generally harmlessly integrated into vac_1_date and vac_2_date
# now, executing this integration to both teens and parents.

# apply the vac_integrate function
adult.long <- vac_integrate(adult.long)

# Some of Result_S1 in adult.long are incorrect, e.g. negativ
# change them to Negative
adult.long$Result_S1 <- str_replace(adult.long$Result_S1, "negativ", "Negative")
adult.long$Result_S1 <- str_replace(adult.long$Result_S1, "positiv", "Positive")
adult.long$Result_S1 <- factor(adult.long$Result_S1, levels = c("Negative", "Positive"),
                               labels = c("Negative", "Positive"))

# Now, change the class of columns to appropriate ones.
# Factors: Sex, Birth_Country, smokestatus, any_risk_emp,
# odkForm, Result, Round, Vac, Type_S1, Type_N, Result_S1
# Date: birthday, sample_date, vac_1_date, vac_2_date

# apply to_fac_date function to variables
kinder <- to_fac_date(kinder,
                      fac_cols = c("Sex"),
                      date_cols = c("birthday"))
adult.long <- to_fac_date(adult.long,
                            fac_cols = c("Sex", "Birth_Country", "smokestatus", "any_risk_emp",
                                         "Result", "Round", "Vac", "Type_S1", "Type_N",
                                         "Result_S1"),
                            date_cols = c("birthday", "vac_1_date", "vac_2_date"))

# check if vac_1_date and vac_2_date have abnormal values
# Apply the check_dates function to all adults
adult.long <- check_dates(adult.long)

# Next, assign Round to kinder
kinder <- kinder %>%
  mutate(Round = case_when(
    sample_days >= as.Date("2020-04-03") & sample_days <= as.Date("2020-06-30") ~ "R1",
    sample_days >= as.Date("2020-11-03") & sample_days <= as.Date("2020-12-12") ~ "R2",
    sample_days >= as.Date("2021-02-22") & sample_days <= as.Date("2021-04-30") ~ "R3",
    sample_days >= as.Date("2021-07-19") & sample_days <= as.Date("2021-10-28") ~ "R4",
    sample_days >= as.Date("2021-10-29") & sample_days <= as.Date("2022-01-31") ~ "R5",
    sample_days >= as.Date("2022-05-13") & sample_days <= as.Date("2022-08-31") ~ "R6"
  ))

kinder$LabId <- as.character(kinder$LabId)
adult.long$LabId <- as.character(adult.long$LabId)

kinder <- kinder |>
  mutate(Round.Lab = case_when(
    grepl("^10", LabId) ~ "R1",
    grepl("^11", LabId) ~"R2",
    grepl("^12", LabId) ~ "R3",
    grepl("^13", LabId) ~ "R4",
    grepl("^14", LabId) ~"R5",
    grepl("^15", LabId) ~"R6",
    TRUE ~ NA
  ))

kinder$Round <- as.factor(kinder$Round)
kinder <- kinder %>%
  select(HaushaltsId:AgeGroup, Round, Round.Lab, everything())

#### END OF WORKING ON ORIGINAL DATA ####

# Set intermediary vars
adult.int <- adult.long
kinder.int <- kinder

# concatenate kinder and adult to family, keeping all columns, setting columns
# without source data to NA.
cols.to.adult <- setdiff(names(kinder), names(adult.long))
cols.to.kinder <- setdiff(names(adult.long), names(kinder))
for (col in cols.to.adult) {
  adult.int[[col]] <- NA
}
for (col in cols.to.kinder) {
  kinder.int[[col]] <- NA
}
family <- bind_rows(adult.int, kinder.int) %>%
  arrange(HaushaltsId, BewohnerId)

# Based on our research purpose, delete adults in families without tested kids
ids.kinder <- unique(kinder$HaushaltsId)
family <- family %>%
  filter(HaushaltsId %in% ids.kinder)

# extract the parents
parents <- adult.long %>%
  filter(HaushaltsId %in% ids.kinder)

# examination of parents shows repetition of observation in parents and kinder
# which suggests teenagers were also included in adult.long dataset.

# extract the teenagers (present in both adult.long and kinder)
ids.teen <- adult.long$BewohnerId[adult.long$BewohnerId %in% kinder$BewohnerId]
teens <- adult.long[adult.long$BewohnerId %in% ids.teen, ]

# exclude teens from parents
parents <- parents[!parents$BewohnerId %in% teens$BewohnerId, ]

# delete columns or rows if they are all NAs.
# apply the remove_NAs function to all interested data sets
family <- remove_NAs(family)
parents <- remove_NAs(parents)
teens <- remove_NAs(teens)

### Teenagers: merge with kinder
### Motivation: connecting observations from both kinder and adult provides more
### information about the kinder samples.

# This merge includes everything from teens. Some sample dates originating only from
# adults is no-match for those in kinder.
# Regularly, each individual will have all rounds of sampling recorded here.

teens <- teens %>%
  left_join(kinder, by = c("BewohnerId", "sample_date"),
             suffix = c(".to_remove", ".original")) %>%
  select(-contains(".to_remove")) %>%
  rename_with(~ gsub("\\.original$", "", .), contains(".original")) %>%
  select(HaushaltsId, BewohnerId, LabId, Sex, Birth_Country, birthday, Age_start,
         smokestatus, any_risk_emp,
         sample_date, Age, AgeGroup, Round, Round.Lab, Result, quant, Anti_N, everything()) %>%
  mutate(Result = ifelse(quant < 0.105 & is.na(Result), "Negative",
                         ifelse(quant >= 0.105 & is.na(Result), "Positive", Result)))

# create another joined dataset where all kids are preserved

kinder.full <- teens %>%
  full_join(kinder, by = c("BewohnerId", "sample_date"),
            suffix = c(".to_remove", ".original"), relationship = "many-to-many") %>%
  select(-contains(".to_remove")) %>%
  rename_with(~ gsub("\\.original$", "", .), contains(".original")) %>%
  select(HaushaltsId, BewohnerId, LabId, Sex, Birth_Country, birthday, Age_start,
         smokestatus, any_risk_emp,
         sample_date, Age, AgeGroup, Round, Round.Lab, Result, quant, Anti_N, everything()) %>%
  mutate(Result = ifelse(quant < 0.105 & is.na(Result), "Negative",
                         ifelse(quant >= 0.105 & is.na(Result), "Positive", Result)))


# Examination of data shows further problems: in teens,
# for each BewohnerId, only some rows of his/her HaushaltsId, Birth_Country
# have values, others are NA. Obviously, for each individuals, all these variables
# should have the same value. Here we need to assign the values accordingly.

# apply the fill_na_with_mode function to teens
teens <- fill_na_with_mode(teens, "BewohnerId", c("HaushaltsId", "Birth_Country"))

kinder <- kinder %>%
  group_by(BewohnerId) %>%
  filter(any(!is.na(Result)))
kinder.full <- kinder.full %>%
  group_by(BewohnerId) %>%
  filter(any(!is.na(Result)))
family <- family %>%
  group_by(BewohnerId) %>%
  filter(any(!is.na(Result)))

# Now, we have 7 datasets: kinder, kinder.full, adult, adult.long, family, teens and parents.
# save each of them as RDS to Work.Data folder
 saveRDS(kinder, file = "Work.Data/kinder.RDS")
 saveRDS(kinder.full, file = "Work.Data/kinder.full.RDS")
 saveRDS(adult, file = "Work.Data/adult.RDS")
 saveRDS(adult.long, file = "Work.Data/adult.long.RDS")
 saveRDS(teens, file = "Work.Data/teens.RDS")
 saveRDS(parents, file = "Work.Data/parents.RDS")
 saveRDS(family, file = "Work.Data/family.RDS")
