# adjust for special case
# for lines 927 928 929 in kinder.full, set BewohnerId to 3467P13
# for lines 930 in kinder.full, set BewohnerId to 3467P23
kinder.full$BewohnerId[927:929] <- "3467P13"
kinder.full$BewohnerId[930] <- "3467P23"


antibody <- kinder.full %>%
  filter(!is.na(Result)) %>%
  group_by(BewohnerId) %>%
  filter(any(Result == "Positive")) %>%
  select(BewohnerId, sample_days, Round, Result) %>%
  # sort by sample_days and BewohnerId
  arrange(BewohnerId, sample_days) %>%
  rename(sample = sample_days) %>%
  filter(!is.na(sample)) %>%
  # for each person each round, add a new column for the date of the previous round
  mutate(previous = lag(sample)) %>%
  select(BewohnerId, previous, sample, Round, Result) %>%
  # create two new columns: daysp, dayss: days since 2020-04-03 for previous and sample
  mutate(daysp = as.numeric(difftime(previous, as.Date("2020-04-03"))),
         dayss = as.numeric(difftime(sample, as.Date("2020-04-03")))) %>%
  # create one new column: daysdiff: difference between daysp and dayss
  mutate(daysdiff = dayss - daysp) %>%
  select(BewohnerId, previous, daysp, sample, dayss, daysdiff, Round, Result) %>%
  filter(!(daysdiff == 0 & Result == lag(Result)) | is.na(daysdiff))

# for each person, get the first sample where Result == "Positive", 
# and delete their observations prior to this date
antibody <- antibody %>%
  filter(cumsum(Result == "Positive") > 0) %>%
  # create column for firstpositive: the first sample date where Result == "Positive"
  group_by(BewohnerId) %>%
  mutate(firstpositive = min(dayss[Result == "Positive"]),
         l = min(daysp[Result == "Negative"]) - firstpositive,
         r = min(dayss[Result == "Negative"]) - firstpositive) %>%
  # if r returns Inf, replace with NA
  mutate(l = ifelse(l == Inf, NA_integer_, l),
         r = ifelse(r == Inf, NA_integer_, r)) %>%
  # for people Result == "Positive" but dayss == NA, set firstpositive to 0
  mutate(firstpositive = ifelse(is.na(firstpositive) & Result == "Positive", 0, firstpositive))

# right or interval censored
antibody <- antibody %>%
  group_by(BewohnerId) %>%
  mutate(censor = ifelse(is.na(r), 0, 3),
         event = ifelse(is.na(r), 0, 1))

# for each person, if l == NA and censor == 0, use firstpositive as l
antibody <- antibody %>%
  group_by(BewohnerId) %>%
  mutate(l = ifelse(is.na(l) & censor == 0, max(dayss) - firstpositive, l))

# in surv.kinder set, keep only one firstpositive and r for each person
antibody <- antibody %>%
  group_by(BewohnerId) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(BewohnerId, firstpositive, l, r, censor, event)

npfit <- ic_np(cbind(l, r) ~ 0, data = antibody)
plot(npfit, xlab = 'Time', ylab = 'Estimated Survival')

surv_object <- with(antibody, Surv(l, r, censor, type = "interval"))
plot(surv_object)

antibody.adults <- adult.long %>%
  filter(!is.na(Result) & !(BewohnerId %in% kinder.full$BewohnerId)) %>%
  group_by(BewohnerId) %>%
  filter(any(Result == "Positive")) %>%
  select(BewohnerId, sample_days, Round, Result) %>%
  # sort by sample_days and BewohnerId
  arrange(BewohnerId, sample_days) %>%
  rename(sample = sample_days) %>%
  filter(!is.na(sample)) %>%
  # for each person each round, add a new column for the date of the previous round
  mutate(previous = lag(sample)) %>%
  select(BewohnerId, previous, sample, Round, Result) %>%
  # create two new columns: daysp, dayss: days since 2020-04-03 for previous and sample
  mutate(daysp = as.numeric(difftime(previous, as.Date("2020-04-03"))),
         dayss = as.numeric(difftime(sample, as.Date("2020-04-03")))) %>%
  # create one new column: daysdiff: difference between daysp and dayss
  mutate(daysdiff = dayss - daysp) %>%
  select(BewohnerId, previous, daysp, sample, dayss, daysdiff, Round, Result) %>%
  filter(!(daysdiff == 0 & Result == lag(Result)) | is.na(daysdiff))

# for each person, get the first sample where Result == "Positive", 
# and delete their observations prior to this date
antibody.adults <- antibody.adults %>%
  filter(cumsum(Result == "Positive") > 0) %>%
  # create column for firstpositive: the first sample date where Result == "Positive"
  group_by(BewohnerId) %>%
  mutate(firstpositive = min(dayss[Result == "Positive"]),
         l = min(daysp[Result == "Negative"])- firstpositive,
         r = min(dayss[Result == "Negative"])- firstpositive) %>%
  # if r returns Inf, replace with NA
  mutate(l = ifelse(l == Inf, NA_integer_, l),
         r = ifelse(r == Inf, NA_integer_, r)) %>%
  # for people Result == "Positive" but dayss == NA, set firstpositive to 0
  mutate(firstpositive = ifelse(is.na(firstpositive) & Result == "Positive", 0, firstpositive))

# right or interval censored
antibody.adults <- antibody.adults %>%
  group_by(BewohnerId) %>%
  mutate(censor = ifelse(is.na(r), 0, 3),
         event = ifelse(is.na(r), 0, 1))

# for each person, if l == NA and censor == 0, use firstpositive as l
antibody.adults <- antibody.adults %>%
  group_by(BewohnerId) %>%
  mutate(l = ifelse(is.na(l) & censor == 0, max(dayss) - firstpositive, l))

# in surv.kinder set, keep only one firstpositive and r for each person
antibody.adults <- antibody.adults %>%
  group_by(BewohnerId) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(BewohnerId, firstpositive, l, r, censor, event) %>%
  mutate(Category = "Erwachsene")

#modelad <- icfit(Surv(l, r, type = "interval2") ~ 1, data = antibody.adults, conf.int = T,
#                 control = icfitControl(B = 20))

#modelkid <- icfit(Surv(l, r, type = "interval2") ~ 1, data = antibody, conf.int = T,
#                  control = icfitControl(B = 20))

#summary(modelad)
#summary(modelkid)

antibody$Category <- "Kinder"

antibody <- rbind(antibody, antibody.adults)

antibody$CategoryLabel <- paste0(antibody$Category, " (n = ",
                                    ave(antibody$Category,
                                        antibody$Category, FUN = length), ")")

antibody$CategoryLabel <- factor(antibody$CategoryLabel)

model <- survfit2(Surv(l, r, type = "interval2") ~ CategoryLabel, data = antibody)
model$n.risk <- round(model$n.risk)
model$n.event <- round(model$n.event)

surviftsurvival1 <- model %>%
  ggsurvfit(size = 1) +
  labs(
    x = "Tage",
    y = "geschätzte Survival"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  ggtitle("Nachweisbarkeit der Antikörper: Kinder vs. Erwachsene",
          subtitle = "(R Survival)") +
  scale_color_manual(values = c("#ff7f00", "#734fcf"),
                     labels = c("Erwachsene\nn=1465", "Kinder\nn=159")) +
  add_confidence_interval(type = "lines", size = 1, alpha = 0.5) +
  add_censor_mark() +
  scale_ggsurvfit() +
  theme(
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 13),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 13),
    legend.text = element_text(size = 13)
  )

npfit <- ic_np(cbind(l, r) ~ CategoryLabel, data = antibody)

# save the follow plot as png
modelic <- icfit(Surv(l, r, type = "interval2") ~ CategoryLabel, data = antibody)


antibody <- antibody %>%
  rename("Kategorie" = CategoryLabel) %>%
  mutate(r = ifelse(is.na(r), Inf, r))

# remove legend title
png("Results/kmkantibody_adchBase.png", width = 9, height = 7, units = 'in', res = 300)
plot(icfit(Surv(l, r, type = "interval2") ~ Kategorie, data = antibody),
     LEGEND = T, COL = c("#ff7f00", "#734fcf"), YLEG = 0.2, yscale = 100,
     conf.int = T, main = "Nachweisbarkeit der Antikörper: Kinder vs. Erwachsene\n(R Interval)",
     lwd = 2, XLAB = 'Tage', YLAB = 'geschätzte Survival')
dev.off()

summary(modelic)
