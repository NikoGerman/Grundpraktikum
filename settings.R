# ----------------------
# load libraries
# ----------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(patchwork)

library(forcats)
# library(checkmate)
# library(viridis)
# library(readxl)
# library(DiagrammeR)
# library(DiagrammeRsvg)
# library(rsvg)

# ----------------------
# set paths to datasets
# ----------------------
path_Worldbank1 <- "Data/raw/Worldbank1.xlsx"
path_Worldbank2 <- "Data/raw/Worldbank2.csv"
path_CO2 <- "Data/raw/Co2_emi_WB.csv"
# original source continent data: 
# https://gist.github.com/stevewithington/20a69c0b6d2ff846ea5d35e5fc47f26c
path_Continents <- "Data/raw/Continents.RDS" 
# original source development data:
# https://www.imf.org/en/Publications/WEO/weo-database/2023/April/groups-and-aggregates#ea
path_AdvancedEconomies <- "Data/raw/AdvancedEconomies.RDS"

# ----------------------
# set ggplot theme
# ----------------------
theme_set(theme_light() + 
            theme(plot.title = element_text(hjust = 0.5, size = 14)))
