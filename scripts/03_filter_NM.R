##==============================================================================
## Project: QuEST - Script to clean up scan data
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================
library(dataRetrieval) # Download USGS discharge data
library(googledrive) # Download docs from Drive
library(tidyverse)
library(lubridate) # Edit date format
library(xts) # Time series
library(ggplot2)

#####################
#### Import Data ####
#####################
# Load data from Google Drive
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1DZktlQUHaot_r4e_fD9ip6zcxHWqslMP")
scan_csvs <- googledrive::drive_ls(path = scan, type = "csv")
3

# Create empty list to store data frames
scan_list <- list()

# Loop over each file in `scan_csvs` and read the data
scan_list <- lapply(seq_along(scan_csvs$id), function(i) {
  local_path <- file.path("googledrive", scan_csvs$name[i])
  
  # Download and read the file
  googledrive::drive_download(file = scan_csvs$id[i], path = local_path, overwrite = TRUE)
  
  # Read the CSV file
  df <- read_csv(local_path)
  
  # Return the data frame
  return(df)
})

# Assign names to each dataframe from the names in the Drive
names(scan_list) <- scan_csvs$name

#######################
#### Flag outliers ####
#######################

#### If you want to remove same sd for all dfs ####
# Define the number of standard deviations to use as the threshold
num_sd <- 4

# Function to flag outliers in a numeric vector
flag_outliers <- function(x, num_sd) {
  mean_value <- mean(x, na.rm = TRUE)
  sd_value <- sd(x, na.rm = TRUE)
  flag <- abs(x - mean_value) > num_sd * sd_value
  return(flag)
}

# Initialize the list to store the updated data frames
scan_outlier <- list()

# Apply the function to all numeric columns in each data frame in the list
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  
  # Flag outliers and create new columns for cleaned data
  df <- df %>%
    mutate(across(where(is.numeric), ~flag_outliers(., num_sd), .names = "is_outlier_{col}")) %>%
    mutate(across(where(is.numeric), 
                  ~ifelse(get(paste0("is_outlier_", cur_column())), NA, .), 
                  .names = "cleaned_{col}"))
  
  # Store the updated data frame in the list
  scan_outlier[[i]] <- df
}

# Assign names to the data frames in the list
names(scan_outlier) <- scan_csvs$name

##################################
#### Flag outliers one by one ####
##################################

#### Bubbles ####

# Define the number of standard deviations to use as the threshold
num_sd <- 2

# Function to flag outliers in a numeric vector
flag_outliers <- function(x, num_sd) {
  mean_value <- mean(x, na.rm = TRUE)
  sd_value <- sd(x, na.rm = TRUE)
  ifelse(abs(x - mean_value) > num_sd * sd_value, TRUE, FALSE)
}

# Initialize the list to store the updated data frames
scan_outliers <- list()

# Flag outliers and create new columns for cleaned data
scan_outliers[["NMUSF21_Bubbles_filtered.csv"]] <-  scan_list[["NMUSF21_Bubbles_filtered.csv"]] %>%
  mutate(across(where(is.numeric), ~flag_outliers(., num_sd), .names = "is_outlier_{col}")) %>%
  mutate(across(where(is.numeric), 
                ~ifelse(get(paste0("is_outlier_", cur_column())), NA, .), 
                .names = "cleaned_{col}"))

#### Blossom ####

# Define the number of standard deviations to use as the threshold
num_sd <- 4

# Function to flag outliers in a numeric vector
flag_outliers <- function(x, num_sd) {
  mean_value <- mean(x, na.rm = TRUE)
  sd_value <- sd(x, na.rm = TRUE)
  ifelse(abs(x - mean_value) > num_sd * sd_value, TRUE, FALSE)
}

# Flag outliers and create new columns for cleaned data
scan_outliers[["NMUSF20_Blossom_filtered.csv"]] <-  scan_list[["NMUSF20_Blossom_filtered.csv"]] %>%
  mutate(across(where(is.numeric), ~flag_outliers(., num_sd), .names = "is_outlier_{col}")) %>%
  mutate(across(where(is.numeric), 
                ~ifelse(get(paste0("is_outlier_", cur_column())), NA, .), 
                .names = "cleaned_{col}"))

#### Buttercup ####
# Define the number of standard deviations to use as the threshold
num_sd <- 2

# Function to flag outliers in a numeric vector
flag_outliers <- function(x, num_sd) {
  mean_value <- mean(x, na.rm = TRUE)
  sd_value <- sd(x, na.rm = TRUE)
  ifelse(abs(x - mean_value) > num_sd * sd_value, TRUE, FALSE)
}

# Flag outliers and create new columns for cleaned data
scan_outliers[["NMUSF12_Buttercup_filtered.csv"]] <-  scan_list[["NMUSF12_Buttercup_filtered.csv"]] %>%
  mutate(across(where(is.numeric), ~flag_outliers(., num_sd), .names = "is_outlier_{col}")) %>%
  mutate(across(where(is.numeric), 
                ~ifelse(get(paste0("is_outlier_", cur_column())), NA, .), 
                .names = "cleaned_{col}"))

#####################################
#### Plot all variables separate ####
#####################################

# Function to plot each variable separately in the same panel
plot_variables <- function(df, file_name) {
  df_long <- df %>%
    select(dateTime, Temp, cleaned_TSS, cleaned_TOC, 'cleaned_NO3.N', cleaned_NO3, cleaned_DOC, Flow_Inst) %>%
    pivot_longer(cols = -dateTime, names_to = "Variable", values_to = "Value")
  
  ggplot(data = df_long, aes(x = dateTime, y = Value, color = Variable)) +
    geom_line() +
    facet_wrap(~Variable, scales = "free_y", ncol = 1) +  # Separate plot for each variable, stacked vertically
    scale_x_datetime(date_breaks = "7 days", date_labels = "%m/%d") +
    ggtitle(file_name) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Measured Value") +
    theme(legend.position = "none")  # Hide legend since we have separate panels
}

# Plots
print(plot_variables(scan_outliers[[1]], scan_csvs$name[1]))
print(plot_variables(scan_outliers[[2]], scan_csvs$name[2]))
print(plot_variables(scan_outliers[[3]], scan_csvs$name[3]))
