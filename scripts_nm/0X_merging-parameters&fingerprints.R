##==============================================================================
## Project: QuEST
## Script to merge parameters and fingerprint s::can data
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(googledrive)
library(readxl)
library(dplyr)

########################################
#### Clear folders that we will use ####
########################################
# List and delete all files in the folder
files <- list.files(path = "scan_figs", full.names = TRUE)
file.remove(files)

files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

#########################
#### Import abs data ####
#########################
# Load data from Google Drive
scan <- googledrive::as_id("https://drive.google.com/drive/u/1/folders/1np2B4bSWaNMIYE2FHL3YOnZ20FRudsEy")
scan_xls <- googledrive::drive_ls(path = scan, type = "xlsx")

# Create empty list to store data frames
scan_abs <- list()

# Loop over each file in scan_xls and read the data
scan_abs <- lapply(seq_along(scan_xls$id), function(i) {
  local_path <- file.path("googledrive", scan_xls$name[i])
  
  # Download the file
  googledrive::drive_download(file = scan_xls$id[i], path = local_path, overwrite = TRUE)
  
  # Read the second sheet
  header <- read_excel(local_path, sheet = 3, skip = 1, col_names = FALSE)  # Read sheet 2
  col_names <- as.character(unlist(header[1, ]))
  
  # Determine the number of columns
  num_cols <- length(col_names)
  # Create col_types vector
  col_types = c("date",  rep("text", num_cols - 1))
  
  # Read the Excel file, specifying the column types
  df <- read_excel(local_path, skip = 1, sheet = 2, col_types = col_types)
  
  # Return the data frame
  return(df)
})

names(scan_abs) <- scan_xls$name

#### Tidy abs data ####

# Change Date format
scan_abs <- lapply(scan_abs, function(df) {
  df <- df %>%
    mutate(`Parameter:` = as.POSIXct(`Parameter:`, format = "%Y-%m-%d %H:%M:%S", tz = "US/Mountain"))  # Use backticks for column name
  
  return(df)
})

head(scan_abs[[1]])

# Check parameter names
print(names(scan_abs[[1]]))

# Change some names for easier manipulation
scan_abs <- lapply(scan_abs, function(df) {
  # Check if "Parameter:" exists and rename it to "dateTime"
  if ("Parameter:" %in% names(df)) {
    df <- df %>%
      rename(DateTime = `Parameter:`)
  }
  
  return(df)  # Return the modified dataframe
})

###############################
#### Import parameter data ####
###############################

# Create empty list to store data frames
scan_params <- list()

# Loop over each file in scan_xls and read the data
scan_params <- lapply(seq_along(scan_xls$id), function(i) {
  local_path <- file.path("googledrive", scan_xls$name[i])
  
  # Read the first sheet
  header <- read_excel(local_path, sheet = 1, skip = 1, col_names = FALSE)  # Read sheet 1
  col_names <- as.character(unlist(header[1, ]))
  
  # Determine the number of columns
  num_cols <- length(col_names)
  # Create col_types vector
  col_types = c("date",  rep("text", num_cols - 1))
  
  # Read the Excel file, specifying the column types
  df <- read_excel(local_path, skip = 1, sheet = 1, col_types = col_types)
  
  # Return the data frame
  return(df)
})

# Check the contents of the list
str(scan_params)

#### Tidy parameters data ####

# Change some names for easier manipulation
scan_params <- lapply(scan_params, function(df) {
  
  # Make sure numeric variables are numeric
  df <- df %>%
    mutate(dateTime = as.POSIXct(dateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Mountain"))
  
  return(df)
})

names(scan_params) <- scan_xls$name

############################################
#### Merge parameter and absorbance data####
############################################

# Function to get the corresponding parameter file name
get_param_name <- function(abs_name) {
  gsub("\\.xlsx$", "_filtered.csv", abs_name)
}

# Merge parameter and absorbance data based on dateTime
merged_data_list <- lapply(seq_along(scan_abs), function(i) {
  abs_name <- names(scan_abs)[i]  # Get the absorbance file name
  param_name <- get_param_name(abs_name)  # Get the corresponding parameter file name
  
  # Find the corresponding parameter data frame
  param_df <- scan_params[[which(names(scan_params) == param_name)]]
  
  # Merge the data frames (param data first)
  merged_df <- left_join(param_df, scan_abs[[i]], by = "dateTime")
  
  # Create a new file name based on the parameter name
  new_file_name <- gsub("_filtered\\.csv$", "", param_name)  # Remove _filtered.csv
  new_file_name <- paste0(new_file_name, "_merged.csv")  # Add _merged.csv
  
  # Save the merged data frame to a new CSV file
  write.csv(merged_df, new_file_name, row.names = FALSE)
  
  return(merged_df)
})

# Set names for the merged data frames
names(merged_data_list) <- names(scan_abs)

###################################
#### Save merged data to Drive ####
###################################

# Function to remove file extension
remove_extension <- function(file_name) {
  sub("\\.[[:alnum:]]+$", "", file_name)
}

# Loop through each data frame in the list
for (i in seq_along(merged_data_list)) {
  # Access the current data frame
  df <- merged_data_list[[i]]
  
  # Define the file name and path
  clean_name <- remove_extension(scan_xls$name[i])
  file_name <- paste0("googledrive/", clean_name, "_merged.csv")
  
  # Save the new data frame to a CSV file
  write.csv(df, file_name, row.names=FALSE, quote=FALSE)
  
  # Define the target folder ID in Google Drive
  # This is the "cleaned" folder
  drive_folder_id <- "1QsjPCu8AVhePe7DGWBhRha26HXbyshcu"
  
  # Upload the file to the specified Google Drive folder
  drive_upload(media = file_name, path = as_id(drive_folder_id))
}

