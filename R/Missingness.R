Worldbank <- readr::read_rds("Data/Worldbank.RDS")

# Show unique Countries and Years
unique(Worldbank$`Country Name`)
unique(Worldbank$Year)

# plotting NAs

NA_plot_Country_vs_Series <- Worldbank %>%
  select(-`Country Name`)%>%
  group_by(`Country Code`)%>%
  summarize(across(-Year, ~sum(is.na(.x)))) %>%
  pivot_longer(-"Country Code", values_to = "NAs", names_to = "Series") %>%
  ggplot(aes(x = `Country Code`, y = Series))+
  geom_tile(aes(fill = NAs)) +
  #scale_fill_brewer(type = "seq")+
  scale_fill_gradient(low = "white", high = "lightblue")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  theme_light()
ggsave("Plots/NA_plot_Country_vs_Series.png", NA_plot_Country_vs_Series, device = "png")

NA_plot_Year_vs_Series <- Worldbank %>%
  select(-`Country Name`)%>%
  group_by(`Year`)%>%
  summarize(across(-"Country Code", ~sum(is.na(.x)))) %>%
  pivot_longer(-"Year", values_to = "NAs", names_to = "Series") %>%
  ggplot(aes(x = `Year`, y = Series))+
  geom_tile(aes(fill = NAs)) +
  #scale_fill_brewer(type = "seq")+
  scale_fill_gradient(low = "white", high = "lightblue")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  theme_light()
ggsave("Plots/NA_plot_Year_vs_Series.png", NA_plot_Year_vs_Series, device = "png")

NA_plot_Year_vs_Country <- Worldbank %>%
  select(-`Country Code`)%>%
  group_by(Year, `Country Name`) %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  ungroup() %>%
  pivot_longer(c(-Year, -"Country Name"), values_to = "NAs", names_to = "Series") %>%
  select(-Series) %>%
  group_by(Year, `Country Name`) %>%
  summarize(NAs = sum(NAs)) %>%
  ggplot(aes(x = Year, y = `Country Name`))+
  geom_tile(aes(fill = NAs)) +
  scale_fill_gradient(low = "white", high = "lightblue")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  theme_light()
ggsave("Plots/NA_plot_Year_vs_Country.png", NA_plot_Year_vs_Country, device = "png")

NA_plot_year <- Worldbank %>%
  select(-`Country Name`, -`Country Code`) %>%
  group_by(Year) %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  ungroup() %>%
  pivot_longer(-Year, names_to = "Series", values_to = "NAs") %>%
  group_by(Year) %>%
  summarise(NAs = sum(NAs)) %>%
  ggplot(aes(y = Year, x = NAs)) +
  geom_col(fill = "lightblue")+
  theme_light()
ggsave("Plots/NA_plot_year.png", NA_plot_year, device = "png")

NA_plot_country <- Worldbank %>%
  select(-`Country Code`, -`Year`) %>%
  group_by(`Country Name`) %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  ungroup() %>%
  pivot_longer(-`Country Name`, names_to = "Series", values_to = "NAs") %>%
  group_by(`Country Name`) %>%
  summarise(NAs = sum(NAs)) %>%
  ggplot(aes(y = forcats::fct_reorder(`Country Name`, NAs, .desc = FALSE), x = NAs)) +
  geom_col(fill = "lightblue")+
  ylab("Country")+
  theme_light()
ggsave("Plots/NA_plot_country.png", NA_plot_country, device = "png")

NA_plot_Series <- Worldbank %>%
  select(-`Country Code`,-`Country Name`, -`Year`) %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "Series", values_to = "NAs") %>%
  ggplot(aes(x = NAs, y = forcats::fct_reorder(Series, NAs, .desc = FALSE))) +
  geom_col(fill = "lightblue")+
  ylab("Series")+
  theme_light()
ggsave("Plots/NA_plot_Series.png", NA_plot_Series, device = "png")
