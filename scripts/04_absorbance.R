##==============================================================================
## Project: QuEST
## Script to visualize compensated fingerprints
## Here we will plot some images but will not be saving cleaned data back. 
## It is just so see what your data looks like and understand what needs to get done
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(googledrive) #Download docs from Drive
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl) #to read excel 
library(lubridate) # Edit date format
library(xts) # Time series

#####################
#### Import Data ####
#####################
# Load data from Google Drive
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1np2B4bSWaNMIYE2FHL3YOnZ20FRudsEy")
# List all Excel files in the folder
scan_excel <- googledrive::drive_ls(path = scan)

# Create empty list to store data frames
scan_list <- list()

# Loop over each file in the `scan_excel` data frame
for (i in seq_along(scan_excel$id)) {
  # Define the local file path
  local_path <- file.path("googledrive", scan_excel$name[i])
  
  # Download the file
  googledrive::drive_download(
    file = scan_excel$id[i],
    path = local_path,
    overwrite = TRUE
  )
  
  # List all sheets in the Excel file
  sheet_names <- excel_sheets(local_path)
  print(paste("Sheets in", scan_excel$name[i], ":", paste(sheet_names, collapse = ", ")))
  
  # Specify the sheet you want to read (by name or index)
  # Read the second sheet
  sheet_to_read <- sheet_names[2]  # You can change this to a specific sheet name or index
  
  # Read the header row (row 2) from the specified sheet
  header <- read_excel(local_path, sheet = sheet_to_read, skip = 1, n_max = 1, col_names = FALSE)
  
  # Convert the header to a character vector and clean empty names
  col_names <- as.character(unlist(header[1, ]))
  col_names[col_names == ""] <- paste0("X", seq_along(col_names[col_names == ""]))
  
  # Read the data starting from row 4 using the header as column names
  data <- read_excel(local_path, sheet = sheet_to_read, skip = 4, col_names = col_names)
  
  # Store the data in the list with the sheet name as the key
  scan_list[[paste(scan_excel$name[i], sheet_to_read, sep = "_")]] <- data
}

###################################################
#### Reshape the data from wide to long format ####
###################################################

# Create an empty list to store reshaped data for each file
reshaped_data_list <- list()

# Loop over each item in the scan_list
for (file_name in names(scan_list)) {
  
  # Extract the data for the current file
  data <- scan_list[[file_name]]
  
  # Reshape the data from wide to long format
  long_data <- data %>%
    pivot_longer(
      cols = ends_with("nm"),
      names_to = "Wavelength",    # Name new 'Wavelength' column
      values_to = "Absorbance"    # Name new 'Absorbance' column
    ) %>%
    mutate(Wavelength = as.numeric(gsub("[^0-9.]", "", Wavelength))) # Convert wavelength to numeric and remove nm from value
  
  # Store the reshaped data in the list with the file name as the key
  reshaped_data_list[[file_name]] <- long_data
}

# Now you have reshaped data for all files in `reshaped_data_list`

###########################################################
#### Plot Absorbance vs Wavelength for a specific date ####
###########################################################

# Plot for a specific file - CHOOSE ONE
file_to_plot <- reshaped_data_list[["2024-07-30_NMUSF20_Blossom.xlsx_24080205 Compensated Fingerprin"]]
file_to_plot <- reshaped_data_list[["2024-07-26_NMUSF21_Bubbles.xlsx_24080206 Compensated Fingerprin"]]
file_to_plot <- reshaped_data_list[["2024-07-30_NMUSF12_Buttercup.xlsx_24080204 Compensated Fingerprin"]]
# Plot for a specific date
date_to_plot <- as.POSIXct("2024-07-18 02:15:00", tzone = "UTC")

# Filter the data for the specific date
filtered_data <- file_to_plot %>%
  filter(`Parameter:` == date_to_plot)

# Create the plot
ggplot(filtered_data, aes(x = Wavelength, y = Absorbance)) +
  geom_line() +s
  labs(title = paste(date_to_plot, file_name), 
       x = "Wavelength (nm)", y = "Absorbance") +
  theme_minimal()

