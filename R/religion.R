religion_raw <- readr::read_csv("Data/raw/UNdata_Export_20241209_095747986.csv") 

religion <- religion_raw %>%
  filter(`Country or Area` %in% religion_raw$`Country or Area`[!grepl("\\d", religion_raw$`Country or Area`)]) %>%
  filter(`Country or Area` != "footnoteSeqID") %>%
  filter(Sex == "Both Sexes") %>%
  filter(Religion != "Total") %>%
  group_by(`Country or Area`) %>%
  slice_max(Value) %>%
  select(`Country or Area`, Religion)

Worldbank %>%
  left_join(religion, by = c("Country_Name" = "Country or Area"), relationship = "many-to-many") %>%
  select(all_of(c("Country_Name", "Religion")))
