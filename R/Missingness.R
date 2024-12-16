Worldbank <- readRDS("Data/cleaned/Worldbank.RDS")

# Show unique Countries and Years
# unique(Worldbank$Country_Name)
# unique(Worldbank$Year)

# plotting NAs
NA_plot_Country_vs_Series <- Worldbank %>%
  group_by(Country_Name)%>%
  summarize(across(-Year, ~sum(is.na(.x)))) %>%
  pivot_longer(-Country_Name, values_to = "NAs", names_to = "Series") %>%
  filter(NAs > 0) %>%
  ggplot(aes(x = Country_Name, y = Series))+
  geom_tile(aes(fill = NAs), alpha = .8) +
  scale_fill_gradientn(colours = c("lightblue", "blue", "darkblue"),
                       limits = c(0, 50))+
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_y_discrete(labels = c("Alkoholkonsum pro Kopf",
                              "Schüler-Lehrer-Verhältnis",
                              "Ernährungsunsicherheit",
                              "HIV",
                              "Tabaknutzung",
                              "Grundlegende Schulbildung",
                              "Gesundheitsausgaben",
                              "Staatsverschuldung",
                              "Netto-Nationaleinkommen pro Kopf")) +
  labs(x = "Land", y = "Merkmal", fill = "fehlende Beobachtungen")
ggsave("Plots/NA_plot_Country_vs_Series.png", NA_plot_Country_vs_Series, device = "png")

NA_plot_Year_vs_Series <- Worldbank %>%
  select(-Country_Name)%>%
  group_by(`Year`)%>%
  summarize(across(-Country_Code, ~sum(is.na(.x)))) %>%
  pivot_longer(-"Year", values_to = "NAs", names_to = "Series") %>%
  filter(NAs > 0) %>%
  ggplot(aes(x = `Year`, y = Series))+
  geom_tile(aes(fill = NAs), alpha = .8) +
  scale_fill_gradientn(colours = c("lightblue", "blue", "darkblue"),
                      limits = c(0, 50))+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_y_discrete(labels = c("Alkoholkonsum pro Kopf",
                              "Schüler-Lehrer-Verhältnis",
                              "Ernährungsunsicherheit",
                              "HIV",
                              "Tabaknutzung",
                              "Grundlegende Schulbildung",
                              "Gesundheitsausgaben",
                              "Staatsverschuldung",
                              "Netto-Nationaleinkommen pro Kopf")) +
  labs(x = "Jahr", y = "Merkmal", fill = "fehlende Beobachtungen")
ggsave("Plots/NA_plot_Year_vs_Series.png", NA_plot_Year_vs_Series, device = "png")

NA_plot_Year_vs_Country <- Worldbank %>%
  select(-Country_Code)%>%
  group_by(Year, Country_Name) %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  ungroup() %>%
  pivot_longer(c(-Year, -Country_Name), values_to = "NAs", names_to = "Series") %>%
  select(-Series) %>%
  group_by(Year, Country_Name) %>%
  summarize(NAs = sum(NAs)) %>%
  ggplot(aes(x = Year, y = Country_Name))+
  geom_tile(aes(fill = NAs)) +
  scale_fill_gradient(low = "white", high = "lightblue")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  theme_light()
ggsave("Plots/NA_plot_Year_vs_Country.png", NA_plot_Year_vs_Country, device = "png")

######

NA_plot_year <- Worldbank %>%
  select(-Country_Name, -Country_Code) %>%
  group_by(Year) %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  ungroup() %>%
  pivot_longer(-Year, names_to = "Series", values_to = "NAs") %>%
  group_by(Year) %>%
  summarise(NAs = sum(NAs)) %>%
  ggplot(aes(y = Year, x = NAs)) +
  geom_col(fill = "lightblue")+
  labs(y = "Jahr", x = "Fehlende Beobachtungen") + 
  theme_light()
ggsave("Plots/NA_plot_year.png", NA_plot_year, device = "png")

NA_plot_country <- Worldbank %>%
  select(-Country_Code, -`Year`) %>%
  group_by(Country_Name) %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  ungroup() %>%
  pivot_longer(-Country_Name, names_to = "Series", values_to = "NAs") %>%
  group_by(Country_Name) %>%
  summarise(NAs = sum(NAs)) %>%
  ggplot(aes(y = forcats::fct_reorder(Country_Name, NAs, .desc = FALSE), x = NAs)) +
  geom_col(fill = "lightblue")+
  labs(y = "Land", x = "Fehlende Beobachtungen") +
  theme_light()
ggsave("Plots/NA_plot_country.png", NA_plot_country, device = "png")

NA_plot_Series <- Worldbank %>%
  select(-Country_Code,-Country_Name, -`Year`) %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "Series", values_to = "NAs") %>% 
  filter(NAs > 0) %>%
  ggplot(aes(x = NAs, y = forcats::fct_reorder(Series, NAs, .desc = FALSE))) +
  geom_col(fill = "lightblue")+
  ylab("Series")+
  scale_y_discrete(labels = c("Alkoholkonsum pro Kopf",
                              "Schüler-Lehrer-Verhältnis",
                              "Ernährungsunsicherheit",
                              "HIV",
                              "Tabaknutzung",
                              "Grundlegende Schulbildung",
                              "Gesundheitsausgaben",
                              "Staatsverschuldung",
                              "Netto-Nationaleinkommen pro Kopf")) +
  labs(x = "fehlende Beobachtungen", y = "Merkmal")
ggsave("Plots/NA_plot_Series.png", NA_plot_Series, device = "png")
