source("env_setup.R")

adult.trans <- adult.long %>%
  select(HaushaltsId, BewohnerId, Round, Result, quant) %>%
  rename(Anti_N = quant) %>%
  mutate(AgeGroup = NA)

kinder.trans <- kinder.full %>%
  select(HaushaltsId, BewohnerId, Round, Result, Anti_N, AgeGroup)

all.data <- rbind(kinder.trans, adult.trans) %>%
  #delete duplicates
  distinct()

# family: parents are marked in AgeGroup as Erwachsene
family.positive <- all.data %>%
  mutate(Category = ifelse(is.na(AgeGroup), "Erwachs.", "Kinder")) %>%
  filter(Result == "Positive" & !is.na(Anti_N))

family.positive$CategoryLabel <- paste0(family.positive$Category, "\nn = ",
                                        ave(family.positive$Category,
                                            family.positive$Category, FUN = length))

kinder.positive <- family.positive %>%
  filter(Category == "Kinder") %>%
  mutate(
    AgeGroup = ifelse(AgeGroup %in% c("<10", "10-14"), "<14",
                      ifelse(AgeGroup %in% c("14-16", "16-18"), "14-18", "18-20"))
  )

kinder.positive$AgeGroupLabel <- paste0(kinder.positive$AgeGroup, "\nn = ",
                                      ave(kinder.positive$AgeGroup,
                                          kinder.positive$AgeGroup, FUN = length))

kinder.positive <- kinder.positive %>%
  mutate(
    AgeGroup = factor(AgeGroup,
                      levels = c("<14", "14-18", "18-20"))
  )

kinder.positive <- kinder.positive %>%
  mutate(
    AgeGroupLabel = factor(AgeGroupLabel,
                           levels = c("<14\nn = 24", "14-18\nn = 69", "18-20\nn = 131"))
  )



# First, create a summarized dataset
kinder_summary <- kinder.positive %>%
  filter(!is.na(Round)) %>%
  group_by(Round) %>%
  summarize(n = n(), .groups = 'drop')

# Merge this summary with your original dataset
kinder.positive <- kinder.positive %>%
  filter(!is.na(Round)) %>%
  left_join(kinder_summary, by = c("Round"))

# Now, modify your ggplot code to use the new n value in facet labels

# mean, quantile and iqr for each age group in kinder.positive
summarykinder <- kinder.positive %>%
  group_by(AgeGroupLabel) %>%
  reframe(
    mean = mean(Anti_N),
    quantile = quantile(Anti_N, probs = c(0.25, 0.5, 0.75)),
    iqr = IQR(Anti_N)
  ) %>%
  # delete replicate rows
  distinct()

# Kurtosis and skewness of Anti-N for each age group
kurtskew <- kinder.positive %>%
  group_by(AgeGroup) %>%
  reframe(
    Median = median(Anti_N, na.rm = TRUE),
    Mittel = mean(Anti_N, na.rm = TRUE),
    Varianz = var(Anti_N, na.rm = TRUE),
    Schiefe = skewness(Anti_N),
    Exz.Kurt = kurtosis(Anti_N) - 3
  ) %>%
  # delete replicate rows
  distinct()

kurtskew.adult <- family.positive %>%
  filter(Category == "Erwachs.") %>%
  ungroup() %>%
  reframe(
    AgeGroup = "Erwachs.",
    Median = median(Anti_N, na.rm = TRUE),
    Mittel = mean(Anti_N, na.rm = TRUE),
    Varianz = var(Anti_N, na.rm = TRUE),
    Schiefe = skewness(Anti_N, na.rm = TRUE),
    Exz.Kurt = kurtosis(Anti_N, na.rm = TRUE) - 3
  ) %>%
  # delete replicate rows
  distinct()

kurtskew <- rbind(kurtskew, kurtskew.adult) %>%
  mutate(
    Median = round(Median, 1),
    Mittel = round(Mittel, 1),
    Varianz = round(Varianz, 1),
    Exz.Kurt = round(Exz.Kurt, 1),
    Schiefe = round(Schiefe, 1)
  )

# bind kinder.positive with family.positive whose Category == "Erwachs."
kinder.positive <- rbind(kinder.positive, family.positive %>%
                           filter(Category == "Erwachs."))
# For Category == "Erwachs.", let AgeGroup = "Erwachs."
kinder.positive <- kinder.positive %>%
  mutate(AgeGroup = as.character(AgeGroup)) %>%
  mutate(AgeGroup = ifelse(Category == "Erwachs.", "Erwachs.", AgeGroup))

kinder.positive$AgeGroup[kinder.positive$AgeGroup == "Erwachs."] <- "Erwachsene"

kinder.positive$AgeGroupLabel <- paste0(kinder.positive$AgeGroup, " (n = ",
                                        ave(kinder.positive$AgeGroup,
                                            kinder.positive$AgeGroup, FUN = length),
                                        ")")
kinder.positive <- kinder.positive %>%
  mutate(
    AgeGroup = factor(AgeGroup,
                      levels = c("<14", "14-18", "18-20", "Erwachsene"))
  )

kinder.positive <- kinder.positive %>%
  mutate(
    AgeGroupLabel = factor(AgeGroupLabel,
                           levels = c("<14 (n = 19)", "14-18 (n = 69)", "18-20 (n = 126)",
                                      "Erwachsene (n = 2242)"))
  )
palette <- brewer.pal(9, "Purples")

# density plot for ages
antibodydichte <- kinder.positive %>%
  ggplot(aes(x = Anti_N, fill = AgeGroup)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ AgeGroupLabel, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c(palette[c(5, 7, 9)], "#E69F00")) +
  labs(title = "Dichteverteilung von Anti-N für verschiedene Altersgruppen",
       subtitle = "COI: 0 - 35",
       x = "Anti-N", y = "Dichte (wurzeltransformiert)", fill = "Altersgruppe",
       caption = "Kinder haben maximal 32 COI Anti-N,\nder Maximum von Erwachsenen liegt über 200.") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  xlim(0, 35) +
  scale_y_sqrt(breaks = c(0, 0.25, 0.5, 1))
antibodydichte
ggsave("antibodydichte.png", path = "Results/", dpi = 300, width = 8, height = 10)
