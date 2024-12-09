### sourcing this file can help set up the environment needed for this project. ###

## Warning: The `interval` package requires supporting packages that do not belong
## to CRAN. It may cause problem due to different R versions.
## In the project, we use R 4.3.2.

## If problems with installing Icens and Interval occur, please check R version.

# setting working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

if (!require("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

if (!require("Icens", quietly = TRUE)) {
  BiocManager::install("Icens")
}

# loading necessary packages
packages <- c("tidyr",
              "dplyr",
              "stringr",
              "checkmate",
              "lubridate",
              "ggplot2",
              "purrr",
              "knitr",
              "pROC",
              "rpart",
              "survival",
              "ggsurvfit",
              "survminer",
              "moments",
              "zoo",
              "kableExtra",
              "readr",
              "patchwork",
              "epitools",
              "ggmosaic",
              "e1071",
              "RColorBrewer",
              "icenReg",
              "interval",
              "scales",
              "ggthemes") ## add more packages if needed

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# loading data
folder_path <- "Work.Data/"
rds_files <- list.files(path = folder_path, pattern = ".RDS$", full.names = TRUE,
                        ignore.case = TRUE)
for (file in rds_files) {
  var_name <- gsub(".RDS", "", file, ignore.case = TRUE)
  var_name <- gsub("Work.Data/", "", var_name)
  data <- readRDS(file)
  assign(var_name, data)
}

# if variables named with "/" at first, which often occurs for MAC users, rename them
for (var in ls()) {
  if (str_detect(var, "/")) {
    new_var <- gsub("/", "", var)
    assign(new_var, get(var))
    rm(var)
  }
}

# remove unnecessary variables
rm(folder_path, rds_files, file, var_name, data, pkg, packages)

# source functions
source("Program/functions.R")