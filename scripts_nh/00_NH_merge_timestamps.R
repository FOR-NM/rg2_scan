##==============================================================================
## Project: QuEST
## Script to merge scan files in one (using timestamp)

## haven't edited this one yet, have not needed it
##==============================================================================

library(readxl) #to read excel 
library(googledrive)
library(dplyr)

########################################
#### Clear folders that we will use ####
########################################
# List and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

##########################
#### Import scan data ####
##########################

#### List and download all files in the folder ####
# This is the "raw" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1Txv_Q6wLuCzhD-7cWMDWueuv85uC8BIw")
# List all CSV files in the folder
scan_csvs <- googledrive::drive_ls(path = scan)
3

# Create empty list to store data frames
scan_list <- list()

# Loop over each file in the `scan_csvs` data frame
for (i in seq_along(scan_csvs$id)) {
  # Define the local file path
  local_path <- file.path("googledrive", scan_csvs$name[i])
  
  # Download the file
  googledrive::drive_download(
    file = scan_csvs$id[i],
    path = local_path,
    overwrite = TRUE
  )
  
  # Read the header row (row 2)
  header <- read_excel(local_path, skip = 1, n_max = 1, col_names = FALSE)
  # Convert the header to a character vector and clean empty names
  col_names <- as.character(unlist(header[1, ]))
  col_names[col_names == ""] <- paste0("X", seq_along(col_names[col_names == ""]))
  
  # Read the data starting from row 2 using the header as column names
  data <- read_excel(local_path, skip = 2, col_names = col_names)
  
  # Store the data in the list
  scan_list[[scan_csvs$name[i]]] <- data
}


####################################
#### Combine data for each site ####
####################################

# Loop through each data frame in the list to change DateTime column name
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  
  # Change names for easier handling
  colnames(df)[1] ="DateTime"
  
  # Update the data frame in the list
  scan_list[[i]] <- df
}

# Site names
site_names <- c("SSM01", "SSM20", "SST13")

# Group files in `scan_list` by matching `site_names` in file names

scan_list_by_site <- lapply(site_names, function(site) {
  # names(scan_list) gives the names of all files in scan_list.
  site_files <- names(scan_list)[grepl(site, names(scan_list))] 
  # grep checks if the current site (e.g., SSM01) appears in each file name in scan_list. 
  # This returns a logical vector (TRUE for matches, FALSE otherwise).
  scan_list[site_files] # Select only the files for this site
  # The [ ] indexing selects only the file names where the match is TRUE.
})

# Name the list by site
names(scan_list_by_site) <- site_names

# Combine data for each site
combined_by_site <- lapply(scan_list_by_site, function(site_data_list) {
  # Bind rows of all data frames for the site
  bind_rows(site_data_list) %>%
    arrange(DateTime) %>%  # Ensure chronological order if 'DateTime' exists
    distinct(DateTime, .keep_all = TRUE) # Remove duplicates
})

##############################
#### Save combined files  ####
##############################

# Ensure DateTime column is properly formatted
combined_by_site <- lapply(combined_by_site, function(df) {
  df$DateTime <- format(df$DateTime, "%Y-%m-%d %H:%M:%S") # Ensure consistent format
  return(df)
})

lapply(names(combined_by_site), function(site) {
  write.csv(combined_by_site[[site]], file.path("data", paste0(site, "_params.csv")))
})
  
lapply(names(combined_by_site), function(site) {
  file <- paste0("data/", site, "_params.csv")
  # this is the in use folder
  drive_folder_id <- "1qpsqrmcnALNS9OVtoIDICdEuW5LkVuIR"
  # Upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
})
  
##==============================================================================
## Now we need to do the same thing but for the compensated abs tab
## abs file is in the same excel in second tab of file
##==============================================================================

##########################
#### Import scan data ####
##########################

# Create empty list to store data frames
scan_list <- list()

# Loop over each file in the `scan_csvs` data frame
for (i in seq_along(scan_csvs$id)) {
  # Define the local file path
  local_path <- file.path("googledrive", scan_csvs$name[i])
  
  # Download the file
  googledrive::drive_download(
    file = scan_csvs$id[i],
    path = local_path,
    overwrite = TRUE
  )
  
  # Read the header row (row 2)
  header <- read_excel(local_path, sheet = 2, skip = 1, n_max = 1, col_names = FALSE)
  # Convert the header to a character vector and clean empty names
  col_names <- as.character(unlist(header[1, ]))
  col_names[col_names == ""] <- paste0("X", seq_along(col_names[col_names == ""]))
  
  # Read the data starting from row 4 using the header as column names
  data <- read_excel(local_path,sheet = 2, skip = 4, col_names = col_names)
  
  # Store the data in the list
  scan_list[[scan_csvs$name[i]]] <- data
}

####################################
#### Combine data for each site ####
####################################

# Change DateTime column name
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  
  # Change names for easier handling
  colnames(df)[1] ="DateTime"
  
  # Update the data frame in the list
  scan_list[[i]] <- df
}

# Site names
site_names <- c("SSM01", "SSM20", "SST13")

# Group files in `scan_list` by matching `site_names` in file names
scan_list_by_site <- lapply(site_names, function(site) {
  # names(scan_list) gives the names of all files in scan_list.
  site_files <- names(scan_list)[grepl(site, names(scan_list))] 
  # grep checks if the current site (e.g., SSM01) appears in each file name in scan_list. 
  # This returns a logical vector (TRUE for matches, FALSE otherwise).
  scan_list[site_files] # Select only the files for this site
  # The [ ] indexing selects only the file names where the match is TRUE.
})

# Name the list by site
names(scan_list_by_site) <- site_names

# Combine data for each site
combined_by_site <- lapply(scan_list_by_site, function(site_data_list) {
  # Bind rows of all data frames for the site
  bind_rows(site_data_list) %>%
    arrange(DateTime) %>%  # Ensure chronological order if 'DateTime' exists
    distinct(DateTime, .keep_all = TRUE) # Remove duplicates
})

##############################
#### Save combined files  ####
##############################

# Ensure DateTime column is properly formatted
combined_by_site <- lapply(combined_by_site, function(df) {
  df$DateTime <- format(df$DateTime, "%Y-%m-%d %H:%M:%S") # Ensure consistent format
  return(df)
})

lapply(names(combined_by_site), function(site) {
  write.csv(combined_by_site[[site]], file.path("data", paste0(site, "_abs.csv")))
})

lapply(names(combined_by_site), function(site) {
  file <- paste0("data/", site, "_abs.csv")
  # this is the in use folder
  drive_folder_id <- "1qpsqrmcnALNS9OVtoIDICdEuW5LkVuIR"
  # Upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
})
