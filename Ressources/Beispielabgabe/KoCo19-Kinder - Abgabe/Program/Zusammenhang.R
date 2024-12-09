library(epitools)
library(ggmosaic)
library(ggthemes)
####setup
kinder <- readRDS("Work.Data/kinder.rds")
adult <- readRDS("Work.Data/adult.rds")
family <- readRDS("Work.Data/family.rds")
parents <- readRDS("Work.Data/parents.rds")
teens <- readRDS("Work.Data/teens.rds")
kinder.full <- readRDS("Work.Data/kinder.full.RDS")

#### add age_start to family
family <- family %>%
  group_by(BewohnerId) %>%
  mutate(Age_start = min(Age))

########Birthcountry
family.herkunft <- fill_na_with_mode(family, "HaushaltsId", "Birth_Country") %>%
  filter(!is.na(Result)) %>%
  group_by(BewohnerId) %>%
  mutate(Seroprevalance.final = ifelse(any(Result == "Positive"), "Ja", "Nein")) %>%
  filter(Age_start <= 20) %>%
  distinct(BewohnerId, .keep_all = TRUE) %>%
  select(HaushaltsId,BewohnerId, Birth_Country, Seroprevalance.final)
herkunft_table <- table(family.herkunft$Birth_Country, family.herkunft$Seroprevalance.final)
Xsq <- chisq.test(herkunft_table)$statistic
k <- sqrt(Xsq / (sum(herkunft_table) + Xsq))
kstern <- k / (sqrt(1 / 2))
# rename to corrected contingency coefficient
names(kstern) <- "corrected contingency coefficient"
kstern.herkunft <- kstern
herkunft_or <-  oddsratio(herkunft_table)$measure[[2]] 
herkunft_or <- round(herkunft_or, 3)
herkunft_or <- paste0(herkunft_or, " (DE)")


######### smokestatus
kinder.smoke <- family %>%
  group_by(HaushaltsId) %>%
  mutate(
    smokestatus = case_when(
      smokestatus == "Current smoker" ~ "Current smoker",
      any(smokestatus == "Current smoker", na.rm = TRUE) ~ "Passive smoker",
      TRUE ~ smokestatus
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(Result)) %>%
  group_by(BewohnerId) %>%
  mutate(Seroprevalance.final = ifelse(any(Result == "Positive"), "Ja", "Nein")) %>%
  filter(Age_start <= 20) %>%
  mutate(smokestatus = ifelse(is.na(smokestatus), "Non smoker", smokestatus))  %>%
  mutate(smokestatus = ifelse(smokestatus == "Past smoker", "Passive smoker", smokestatus)) %>%
  distinct(BewohnerId, .keep_all = TRUE) %>%
  select(HaushaltsId,BewohnerId, smokestatus, Seroprevalance.final)
kinder.smoke$smokestatus <- factor(kinder.smoke$smokestatus, levels = c("Non smoker", "Passive smoker", "Current smoker"))
smoke_table <- table(kinder.smoke$smokestatus, kinder.smoke$Seroprevalance.final)
Xsq <- chisq.test(smoke_table)$statistic
k <- sqrt(Xsq / (sum(smoke_table) + Xsq))
kstern <- k / (sqrt(1 / 2))
# rename to corrected contingency coefficient
names(kstern) <- "corrected contingency coefficient"
kstern.smoke <- kstern
smoke_orPS <- 1 / oddsratio(smoke_table)$measure[[2]] # PS 1.43 to none
smoke_orPS <- round(smoke_orPS, 3)
smoke_orCS <- 1 / oddsratio(smoke_table)$measure[[3]] # CS 2.75 to none
smoke_orCS <- round(smoke_orCS, 3)
smoke_or <- paste0(smoke_orPS, " (PS)\n", smoke_orCS, " (CS)")

kinder.smoke$smokestatus <- factor(kinder.smoke$smokestatus, levels = c("Non smoker", "Passive smoker", "Current smoker"))

############### risky employment
kinder.risky <- family %>%
  group_by(HaushaltsId) %>%
  filter(!all(is.na(any_risk_emp))) %>%
  mutate(any_risk_emp = as.character(any_risk_emp)) %>%
  mutate(
    any_risk_emp = ifelse(any(any_risk_emp == "Yes"), "Yes", any_risk_emp)
  )  %>%
  mutate(any_risk_emp = ifelse(is.na(any_risk_emp), "No", any_risk_emp)) %>%
  ungroup() %>%
  filter(!is.na(Result)) %>%
  group_by(BewohnerId) %>%
  filter(Age_start <= 20) %>%
  mutate(Seroprevalance.final = ifelse(any(Result == "Positive"), "Ja", "Nein")) %>%
  distinct(BewohnerId, .keep_all = TRUE) %>%
  select(HaushaltsId,BewohnerId, any_risk_emp, Seroprevalance.final)

risk_table <- table(kinder.risky$any_risk_emp, kinder.risky$Seroprevalance.final)
Xsq <- chisq.test(risk_table)$statistic
k <- sqrt(Xsq / (sum(risk_table) + Xsq))
kstern <- k / (sqrt(1 / 2))
# rename to corrected contingency coefficient
names(kstern) <- "corrected contingency coefficient"
kstern.risky <- kstern
# odds ratio
risky_or <- 1 / oddsratio(risk_table)$measure[[2]] # risky 1.43 to none
risky_or <- round(risky_or, 3)


#################### Seroprevalance: Kids and parents
family.sero <- family %>%
  filter(!is.na(Result)) %>%
  mutate(Category = ifelse(Age_start <= 20, "Kinder", "Eltern")) %>%
  group_by(BewohnerId) %>%
  mutate(seroprevalence = ifelse(any(Result == "Positive"), TRUE, FALSE)) %>%
  distinct(BewohnerId, .keep_all = TRUE) %>%
  ungroup() %>%
  select(HaushaltsId, BewohnerId, Category, seroprevalence) %>%
  group_by(HaushaltsId) %>%
  mutate(
    seroprevalence_parents = sum(seroprevalence[Category == "Eltern"])
  ) %>%
  ungroup() %>%
  filter(Category == "Kinder") %>%
  select(HaushaltsId, BewohnerId, seroprevalence, seroprevalence_parents) %>%
  mutate(seroprevalence_parents = ifelse(seroprevalence_parents >= 1, TRUE, FALSE))

sero_table <- table(family.sero$seroprevalence_parents, family.sero$seroprevalence)

Xsq <- chisq.test(sero_table)$statistic
k <- sqrt(Xsq / (sum(sero_table) + Xsq))
kstern <- k / (sqrt(1 / 2))
# rename to corrected contingency coefficient
names(kstern) <- "corrected contingency coefficient"
kstern.sero <- kstern
# odds ratio
sero_or <-  oddsratio(sero_table)$measure[[2]] 
sero_or <- round(sero_or, 3)





#########################
########################## the final dataframe
ksterns <- data.frame(
  Variable = c("Familienherkunft", "Rauchen",
               "Risikoarbeit", "Eltern"),
  KKKoeff = c(kstern.herkunft, kstern.smoke, kstern.risky, kstern.sero),
  OR = c(herkunft_or, smoke_or, risky_or, sero_or))
# add a column for interpretation
ksterns <- ksterns %>%
  mutate(
    KKKoeff = round(KKKoeff, 3)
  )








##########################
########################## Mosaicplot 
library(patchwork)
two_color_palette <- c( "burlywood1","burlywood3")
#
colnames(herkunft_table) <- c("Jemals infiziert", "Niemals infiziert")
rownames(herkunft_table) <- c("Deutschland", "Andere")
herkunft_df <- as.data.frame(herkunft_table) %>%
  rename(Geburtsland = Var1, Status = Var2)
herkunft_df$Status <- factor(herkunft_df$Status, levels = c("Niemals infiziert", "Jemals infiziert"))
p.her <- ggplot(data = herkunft_df) +
  geom_mosaic(aes(x = product(Geburtsland), fill = Status, weight = Freq)) +
  theme_minimal() +
  labs(
    title = "Familienherkunft",
    subtitle = "K* = 0.16\nOR(DE) = 2.3",
    x = "", 
    y = "",
  ) +
theme(plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      axis.ticks = element_line(color = "black", size = 0.5),
      axis.ticks.length = unit(0.25, "cm"),
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) +
  scale_fill_manual(values = two_color_palette) 
#
colnames(smoke_table) <- c("Jemals infiziert", "Niemals infiziert")
rownames(smoke_table) <- c("Nicht", "Passiv", "Aktiv")
smoke_df <- as.data.frame(smoke_table) %>%
  rename(smokestatus = Var1, Status = Var2)
smoke_df$Status <- factor(smoke_df$Status, levels = c("Niemals infiziert", "Jemals infiziert"))
p.smoke <- ggplot(data = smoke_df) +
  geom_mosaic(aes(x = product(smokestatus), fill = Status, weight = Freq)) +
  theme_minimal() +
  labs(
    title = "Raucherstatus",
    subtitle = "K* = 0.2\nOR(P.R) = 1.4, OR(A.R) = 2.8",
    x = "", 
    y = "",
  ) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.ticks.length = unit(0.25, "cm"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_fill_manual(values = two_color_palette) 
#
colnames(risk_table) <- c("Jemals infiziert", "Niemals infiziert")
rownames(risk_table) <- c("Nicht risikoreich", "Risikoreich")
risk_df <- as.data.frame(risk_table) %>%
  rename(riskstatus = Var1, Status = Var2)
risk_df$Status <- factor(risk_df$Status, levels = c("Niemals infiziert", "Jemals infiziert"))
p.risk <- ggplot(data = risk_df) +
  geom_mosaic(aes(x = product(riskstatus), fill = Status, weight = Freq)) +
  theme_minimal() +
  labs(
    title = "Beschäftigung im Haushalt",
    subtitle = "K* = 0.11\nOR(Ri) = 1.4",
    x = "", 
    y = "",
  ) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.ticks.length = unit(0.25, "cm"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_fill_manual(values = two_color_palette)
#
colnames(sero_table) <- c("Jemals infiziert", "Niemals infiziert")
rownames(sero_table) <- c("Eltern nie infiziert", "Eltern jemals\ninfiziert")
sero_df <- as.data.frame(sero_table) %>%
  rename(Elternstatus = Var1, Status = Var2)
sero_df$Status <- factor(sero_df$Status, levels = c("Jemals infiziert", "Niemals infiziert"))
sero_df$Elternstatus <- factor(sero_df$Elternstatus, levels = c("Eltern nie infiziert","Eltern jemals\ninfiziert"))
p.adult <- ggplot(data = sero_df) +
  geom_mosaic(aes(x = product(Elternstatus), fill = Status, weight = Freq)) +
  theme_minimal() +
  labs(
    title = "Seroprävalenz von Eltern",
    subtitle = "K* = 0.33\nOR(E.I) = 2.9",
    x = "", 
    y = "",
  ) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.ticks.length = unit(0.25, "cm"),
        legend.position = "none", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_fill_manual(values = two_color_palette) 

p.smoke <- p.smoke + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())
p.adult <- p.adult + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())

Zusammenhang_mos <- (p.her | p.smoke) /
  (p.risk | p.adult)

ggsave("Zusammenhang_mos.png", path = "Results/", width = 6, height = 5,
       device='png', dpi=300)


