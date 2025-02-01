# 	Variables von adult hat Muster nach Round? 

library("tidyverse")

# Results columns
result_cols <- adult.long %>%
  group_by(Round) %>%
  select(BewohnerId, Result, Result_S1) %>%
  filter(Round %in% c("R1", "R2")) %>%
  filter(!is.na(Result_S1))
# Result_S1 starts up from R3

# quant und quant_S1
adult.long %>%
  group_by(Round) %>%
  select(BewohnerId, quant, quant_S1) %>%
  filter(Round %in% c("R1", "R2")) %>%
  filter(!is.na(quant_S1)) %>%
  View()
# quant_S1 start up from R3

# check.pattern <- function(df, col1, col2) {
#   df %>%
#     group_by(Round) %>%
#     select(BewohnerId, col1, col2) %>%
#     filter(Round %in% c("R1", "R2")) %>%
#     filter(!is.na(col2)) %>%
#     View()
# }
# 
# check.pattern(adult.long, "quant", "quant_S1")


# Type
all.equal(adult.long$Type_N, adult.long$Type_S1)
# Type columns nearly identical

# DBS
adult.long %>%
  group_by(Round) %>%
  select(BewohnerId, DBS2_quant) %>%
  filter(Round %in% c("R1", "R2")) %>%
  filter(is.na(DBS2_quant)) %>%
  View()
# no specific pattern