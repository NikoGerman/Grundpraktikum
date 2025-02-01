##### This file contains all the functions used in the project #####

### List of Functions ###
# 1. remove_NAs
# 2. vac_integrate
# 3. get_mode
# 4. fill_na_with_mode
# 5. to_fac_date
# 6. check_dates
# 7. detect_outliers_and_mistakes
# 8. prepare_outlier_visualization
# 9. mark_outliers


# FUNC1: remove_NAs, A function that deletes columns or rows if they are all NAs.
remove_NAs <- function(df) {
  require(dplyr)
  df <- df[rowSums(is.na(df)) < ncol(df), ] %>%
    select_if(~ !all(is.na(.)))
  return(df)
}

# FUNC2: vac_integrate, Integrate Vaccine Dates into Data Frame
#
# The `vac_integrate` function updates the vaccine date columns in a data frame. It checks for `NA`
# values in the `vac_1_date` and `vac_2_date` columns and replaces them with the corresponding
# `Vaccine_Date1` and `Vaccine_Date2` values, if available. It then removes the original
# `Vaccine_Date1` and `Vaccine_Date2` columns.
#
# Inputs:
# - df: A data frame containing the columns `vac_1_date`, `vac_2_date`, `Vaccine_Date1`, and
# `Vaccine_Date2`.
#
# return: A data frame with updated `vac_1_date` and `vac_2_date` columns, and without
# `Vaccine_Date1` and `Vaccine_Date2` columns.
vac_integrate <- function(df) {
  require(dplyr)
  df <- df %>%
    mutate(
      vac_1_date = ifelse(is.na(vac_1_date), Vaccine_Date1, vac_1_date),
      vac_2_date = ifelse(is.na(vac_2_date), Vaccine_Date2, vac_2_date)
    ) %>%
    select(-Vaccine_Date1, -Vaccine_Date2)
  return(df)
}

# FUNC3: get_mode, this is a function that gets the mode in data.
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# FUNC4: fill_na_with_mode, Fill NA with Mode by Group
#
# This function replaces `NA` values in specified columns with the most common value (mode)
# within a specified group. This operation is done on a data frame.
# Inputs:
# - data: A data frame where `NA` values are to be filled.
# - group: The grouping variable as a symbol. Grouping is based on this column.
# - target: A character vector of column names which `NA` values are to be filled with mode.
#
# return:
# A data frame with `NA` values in target columns replaced by the mode within the group.
fill_na_with_mode <- function(data, group, target) {
  require(dplyr)
  require(checkmate)
  assertDataFrame(data, add = "The input for 'data' should be a data frame.")
  assertString(group, add = "The 'group' argument should be a character
                string representing a column name.")
  assertCharacter(target, min.len = 1, add = "The 'target' argument
                  should be a non-empty character vector representing column names.")
  data %>%
    group_by(.data[[group]]) %>%
    mutate(across(all_of(target), ~if_else(is.na(.), get_mode(.[!is.na(.)]), .))) %>%
    ungroup()
}

# FUNC5: to_fac_date, this function changes the class of given columns to factor or date.
to_fac_date <- function(data, fac_cols = NULL, date_cols = NULL) {
  if (!is.null(fac_cols)) {
    for (i in fac_cols) {
      data[[i]] <- as.factor(data[[i]])
    }
  }
  if (!is.null(date_cols)) {
    for (i in date_cols) {
      data[[i]] <- as.Date(data[[i]])
    }
  }
  return(data)
}

# FUNC6: check_dates, check if vac_1_date and vac_2_date have abnormal values
check_dates <- function(data_frame) {
  # Check if the date columns exist in the data frame
  if (!('vac_1_date' %in% names(data_frame)) | !('vac_2_date' %in% names(data_frame))) {
    stop("Data frame must contain vac_1_date and vac_2_date columns")
  }
  # Function to check individual date
  check_date <- function(date) {
    if (!is.na(date) && (year(date) < 2019)) {
      return(NA)
    } else {
      return(date)
    }
  }
  # Apply the check_dates function to the vac_1_date and vac_2_date columns
  data_frame$vac_1_date <- as.Date(unlist(lapply(data_frame$vac_1_date, check_date)),
                                   origin = "1970-01-01")
  data_frame$vac_2_date <- as.Date(unlist(lapply(data_frame$vac_2_date, check_date)),
                                   origin = "1970-01-01")
  return(data_frame)
}


### Function 7 to 9 are for outlier detection and visualization,
### for the serological context, we do not need them. But we keep them here for future use.

# FUNC7: detect_outliers_and_mistakes, identify outliers and potential mistakes
# in all numeric columns of a given dataframe. This function uses either the
# Interquartile Range (IQR) method or the Standard Deviation (SD) method to detect
# these values based on user preference.
#
# Parameters:
# - data: A data frame containing numeric columns.
# - method: A character string indicating the method to be used.
# - low: A numeric value indicating the lower limit for the IQR method.
#
# Interquartile Range (IQR) Method: The IQR method is used for outlier detection.
# It calculates the IQR as the difference between the 75%-quantile 25%-quantile
# of the data. Outliers are then identified as those values that lie below
# Q1 - 1.5 * IQR or above Q3 + 1.5 * IQR. This method is robust to extreme
# values and is often used for skewed distributions.
#
# Standard Deviation (SD) Method: The SD method is typically used for identifying
# potential mistakes. It involves calculating the mean and standard deviation of
# the data. Values that fall below the mean minus 3 times the standard deviation
# or above the mean plus 3 times the standard deviation are flagged as potential mistakes.
# This method assumes a normal distribution of the data and can be more sensitive
# to extreme values compared to the IQR method.

detect_outliers_and_mistakes <- function(data, method = "SD", low = NULL) {
  # Function to detect outliers and mistakes in a single column
  detect_single_column <- function(column_data) {
    column_data <- na.omit(column_data)
    Q1 <- quantile(column_data, 0.25, na.rm = TRUE)
    Q3 <- quantile(column_data, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1

    lower_threshold <- Q1 - 1.5 * IQR
    upper_threshold <- Q3 + 1.5 * IQR

    if (method == "Q") {
      if (is.null(low)) {
        lower_limit <- quantile(column_data, 0.01, na.rm = TRUE)
      } else {
        lower_limit <- low
      }
      upper_limit <- quantile(column_data, 0.99, na.rm = TRUE)
    } else if (method == "SD") {
      lower_limit <- mean(column_data) - 3 * sd(column_data)
      upper_limit <- mean(column_data) + 3 * sd(column_data)
    } else {
      stop("Method of identifying mistakes must be either 'Q' or 'SD'")
    }

    outliers <- column_data[column_data < lower_threshold | column_data > upper_threshold]
    potential_mistakes <- column_data[column_data < lower_limit | column_data > upper_limit]

    list(Outlier = outliers, Mistake = potential_mistakes)
  }

  # Initialize list to store results
  results_list <- list()

  # Loop through each numeric column
  numeric_columns <- sapply(data, is.numeric)
  for (col_name in names(data)[numeric_columns]) {
    results_list[[col_name]] <- detect_single_column(data[[col_name]])
  }

  return(results_list)
}

# FUNC8: prepare_outlier_visualization, this function prepares the data for
# outlier and mistake visualization.
#
# Parameters:
# - df: A data frame containing the original data.
# - check: A list containing the results of the detect_outliers_and_mistakes function.
# - col_name: A character string indicating the column name to be processed.
#

prepare_outlier_visualization <- function(df, check, col_name) {
  df_vis <- df
  # Process only the specified column
  df_vis[[paste0(col_name, "_status")]] <- "Normal"
  df_vis[df_vis[[col_name]] %in% check[[col_name]]$Outlier,
         paste0(col_name, "_status")] <- "Outlier"
  df_vis[df_vis[[col_name]] %in% check[[col_name]]$Mistake,
         paste0(col_name, "_status")] <- "Mistake"

  return(df_vis %>% select(sample_date, col_name,
                           status = paste0(col_name, "_status")) %>% na.omit())
}

# FUNC9: mark_outliers, a variation of the prepare_outlier_visualization function
# that marks outliers and mistakes in the original data frame.
# Using standard method of IQR.

mark_outliers <- function(df, col_name) {
  df_vis <- df
  check <- detect_outliers_and_mistakes(df, "Q", 0)
  # Process only the specified column
  df_vis[[paste0(col_name, "_status")]] <- "Normal"
  df_vis[df_vis[[col_name]] %in% check[[col_name]]$Outlier,
         paste0(col_name, "_status")] <- "Outlier"
  df_vis[df_vis[[col_name]] %in% check[[col_name]]$Mistake,
         paste0(col_name, "_status")] <- "Mistake"

  return(df_vis)
}
