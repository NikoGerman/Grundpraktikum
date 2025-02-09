# ----------------------
# load libraries
# ----------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(patchwork)

# ----------------------
# set paths to datasets
# ----------------------
path_Worldbank1 <- "Data/raw/Worldbank1.xlsx"
path_Worldbank2 <- "Data/raw/Worldbank2.csv"
path_CO2 <- "Data/raw/Co2_emi_WB.csv"

# ----------------------
# set paths to additional datasets
# ----------------------
# original source continent data: 
# https://gist.github.com/stevewithington/20a69c0b6d2ff846ea5d35e5fc47f26c
path_Continents <- "Data/raw/Continents.RDS" 
# original source of country classifications: Worldbank
# downloadable here
# https://datacatalogfiles.worldbank.org/ddh-published/0037712/DR0090755/CLASS.xlsx
path_CountryClassifications <- "Data/raw/List of economies-Tabelle 1.csv"

# ----------------------
# set ggplot theme
# ----------------------
theme_set(theme_light() + 
            theme(plot.title = element_text(hjust = 0.5, size = 14)))
