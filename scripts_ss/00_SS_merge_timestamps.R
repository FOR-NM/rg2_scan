##==============================================================================
## Project: QuEST
## Script to merge scan files in one (using timestamp)
##==============================================================================

library(readxl) #to read excel 
library(googledrive)
library(dplyr)

########################################
#### Clear folders that we will use ####
########################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

##########################
#### Import scan data ####
##########################
#### list and download all files in the folder ####
# this is the "raw" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1x6tPgXn-DgmBVvFTMG0TEo2AxqHqQLwV")
# list all CSV files in the folder
scan_csvs <- googledrive::drive_ls(path = scan)
3

# create empty list to store data frames
scan_list_param <- list()

# loop over each file in the `scan_csvs` data frame
for (i in seq_along(scan_csvs$id)) {
  # define the local file path
  local_path <- file.path("googledrive", scan_csvs$name[i])
  
  # download the file
  googledrive::drive_download(
    file = scan_csvs$id[i],
    path = local_path,
    overwrite = TRUE
  )
  
  # read the header row (row 2)
  header <- read_excel(local_path, skip = 1, n_max = 1, col_names = FALSE)
  # convert the header to a character vector and clean empty names
  col_names <- as.character(unlist(header[1, ]))
  col_names[col_names == ""] <- paste0("X", seq_along(col_names[col_names == ""]))
  
  # read the data starting from row 4 using the header as column names
  data <- read_excel(local_path, skip = 4, col_names = col_names)
  
  # store the data in the list
  scan_list_param[[scan_csvs$name[i]]] <- data
}

####################################
#### Combine data for each site ####
####################################
# loop through each data frame in the list to change DateTime column name
for (i in seq_along(scan_list_param)) {
  # Access the current data frame
  df <- scan_list_param[[i]]
  
  # change names for easier handling
  colnames(df)[1] ="DateTime"
  
  # update the data frame in the list
  scan_list_param[[i]] <- df
}

# site names
site_names <- c("SSM01", "SSM20", "SST13")

# group files in `scan_list_param` by matching `site_names` in file names

scan_list_by_site <- lapply(site_names, function(site) {
  # names(scan_list_param) gives the names of all files in scan_list_param
  site_files <- names(scan_list_param)[grepl(site, names(scan_list_param))] 
  # grep checks if the current site (e.g., SSM01) appears in each file name in scan_list_param. 
  # This returns a logical vector (TRUE for matches, FALSE otherwise).
  scan_list_param[site_files] # select only the files for this site
  # The [ ] indexing selects only the file names where the match is TRUE.
})

# name the list by site
names(scan_list_by_site) <- site_names

# combine data for each site
combined_by_site <- lapply(scan_list_by_site, function(site_data_list) {
  # bind rows of all data frames for the site
  bind_rows(site_data_list) %>%
    arrange(DateTime) %>%  # chronological order if 'DateTime' exists
    distinct(DateTime, .keep_all = TRUE) # remove duplicates
})

##############################
#### Save combined files  ####
##############################
# ensure DateTime column is properly formatted
combined_by_site <- lapply(combined_by_site, function(df) {
  df$DateTime <- format(df$DateTime, "%Y-%m-%d %H:%M:%S") 
  return(df)
})

lapply(names(combined_by_site), function(site) {
  write.csv(combined_by_site[[site]], file.path("data", paste0(site, "_params.csv")))
})
  
lapply(names(combined_by_site), function(site) {
  file <- paste0("data/", site, "_params.csv")
  # this is the time stamps folder
  drive_folder_id <- "1qpsqrmcnALNS9OVtoIDICdEuW5LkVuIR"
  # Upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
})
  
##==============================================================================
## now we need to do the same thing but for the compensated abs tab
## abs file is in the same excel in second tab of file
##==============================================================================
##########################
#### Import scan data #### 
##########################
# create empty list to store data frames
scan_list <- list()

# loop over each file in the `scan_csvs` data frame
for (i in seq_along(scan_csvs$id)) {
  # define the local file path
  local_path <- file.path("googledrive", scan_csvs$name[i])
  
  # download the file
  googledrive::drive_download(
    file = scan_csvs$id[i],
    path = local_path,
    overwrite = TRUE
  )
  
  # read the header row (row 2)
  header <- read_excel(local_path, sheet = 2, skip = 1, n_max = 1, col_names = FALSE)
  # convert the header to a character vector and clean empty names
  col_names <- as.character(unlist(header[1, ]))
  col_names[col_names == ""] <- paste0("X", seq_along(col_names[col_names == ""]))
  
  # read the data starting from row 4 using the header as column names
  data <- read_excel(local_path,sheet = 2, skip = 4, col_names = col_names)
  
  # Clean the 'DateTime' column name.
  colnames(data)[1] <- "DateTime"
  
  # Force all columns (except DateTime) to be numeric ***
    data <- data %>%
    # Use across() to target all columns EXCEPT 'DateTime' and Measured.status
    mutate(across(!c(DateTime, 'Measured status'), as.numeric))
  # Note: Any non-numeric value in a spectral column will become NA here.
  
  # store the data in the list
  scan_list[[scan_csvs$name[i]]] <- data
}

####################################
#### Combine data for each site ####
####################################
# site names
site_names <- c("SSM01", "SSM20", "SST13")

# group files in `scan_list` by matching `site_names` in file names
scan_list_by_site <- lapply(site_names, function(site) {
  # names(scan_list) gives the names of all files in scan_list.
  site_files <- names(scan_list)[grepl(site, names(scan_list))] 
  # grep checks if the current site (e.g., SSM01) appears in each file name in scan_list. 
  scan_list[site_files] # select only the files for this site
})

# name the list by site
names(scan_list_by_site) <- site_names

# combine data for each site
combined_by_site <- lapply(scan_list_by_site, function(site_data_list) {
  
  # --- NEW: Check and standardize columns before binding ---
  
  # 1. Get the union of all column names across all files for this site
  all_names <- unique(unlist(lapply(site_data_list, names)))
  
  # 2. Iterate through each data frame and align its columns
  site_data_list_aligned <- lapply(site_data_list, function(df) {
    # Identify columns that are missing in the current dataframe
    missing_cols <- setdiff(all_names, names(df))
    
    # Add missing columns filled with NA
    for (col in missing_cols) {
      # Use `NA_real_` to ensure new columns are added as numeric (double) type, 
      # matching the expected type of spectral data.
      df[[col]] <- NA_real_ 
    }
    
    # Select and reorder columns to match the 'canonical' order (DateTime first)
    df <- df[, all_names]
    
    return(df)
  })
  
  # --- END NEW: Alignment is complete ---
  
  # bind rows of all aligned data frames for the site
  bind_rows(site_data_list_aligned) %>% # Use the ALIGNED list
    arrange(DateTime) %>%  # ensure chronological order
    distinct(DateTime, .keep_all = TRUE) # remove duplicates
})

SSM20_EXAMPLE <- scan_list$"2024-09-06_SSM20_SN24160203.xlsx"
SSM20_combined <- combined_by_site$SSM20

##########################
#### Clean ####
##########################
combined_by_site <- lapply(scan_list_by_site, function(site_data_list) {
  
  # Step A: Label every row in every file before merging
  processed_files <- lapply(site_data_list, function(df) {
    df$DateTime <- as.POSIXct(df$DateTime)
    
    # DYNAMIC SELECTION: Find spectral columns for THIS specific data frame
    current_spec_cols <- grep("^[0-9]", colnames(df))
    
    # Calculate variance using the columns found in this specific file
    df$row_variance <- apply(df[, current_spec_cols, drop = FALSE], 1, sd, na.rm = TRUE)
    
    # Initial flagging
    df$temp_status <- ifelse(!is.na(df$row_variance) & df$row_variance > 0.001, "Good", "Corrupted")
    return(df)
  })
  
  # Step B: Bind and prioritize
  bind_rows(processed_files) %>%
    group_by(DateTime) %>%
    arrange(DateTime, desc(temp_status), desc(row_variance)) %>%
    mutate(
      Status = case_when(
        n() == 1 & first(temp_status) == "Good" ~ "Original_Good",
        n() > 1 & first(temp_status) == "Good" ~ "Replaced_Good",
        first(temp_status) == "Corrupted" ~ "Corrupted_No_Match",
        TRUE ~ "Original_Good"
      )
    ) %>%
    slice(1) %>% 
    ungroup() %>%
    select(-row_variance, -temp_status)
})

# Access your data
SSM01_clean <- combined_by_site[["SSM01"]]
SSM20_clean <- combined_by_site[["SSM20"]]
SST13_clean <- combined_by_site[["SST13"]]

# see how many "corrupted" rows were removed
nrow(bind_rows(scan_list_by_site[["SSM01"]])) - nrow(SSM01_clean)
nrow(bind_rows(scan_list_by_site[["SSM20"]])) - nrow(SSM20_clean)
nrow(bind_rows(scan_list_by_site[["SST13"]])) - nrow(SST13_clean)

##############################
#### Save combined files  ####
##############################
# ensure DateTime column is properly formatted
combined_by_site <- lapply(combined_by_site, function(df) {
  df$DateTime <- format(df$DateTime, "%Y-%m-%d %H:%M:%S") # ensure consistent format
  return(df)
})

lapply(names(combined_by_site), function(site) {
  write.csv(
    combined_by_site[[site]], 
    file.path("data", paste0(site, "_abs.csv")),
    row.names = FALSE # <--- THIS IS THE FIX TO AVOID REPEATED SPECTRAL VALUES
  )
})

lapply(names(combined_by_site), function(site) {
  file <- paste0("data/", site, "_abs.csv")
  # this is the time stamps folder
  drive_folder_id <- "1qpsqrmcnALNS9OVtoIDICdEuW5LkVuIR"
  # Upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
})

# #### --------------------------------------------------------------------- ####
# #####################################################################
# ## 1. SETUP: LIST, DOWNLOAD, AND CLEAN INDIVIDUAL FILES
# #####################################################################
# # Define the Google Drive folder ID/URL
# drive_id <- as_id("1x6tPgXn-DgmBVvFTMG0TEo2AxqHqQLwV")
# # List all files in the folder
# scan_csvs <- drive_ls(path = drive_id)
# 
# # Create a directory to store downloaded files temporarily
# if (!dir.exists("googledrive_temp")) {
#   dir.create("googledrive_temp")
# }
# 
# # Create empty list to store processed data frames
# scan_list <- list()
# 
# # Loop over each file
# for (i in seq_along(scan_csvs$id)) {
#   file_name <- scan_csvs$name[i]
#   local_path <- file.path("googledrive_temp", file_name)
#   
#   # Download the file
#   drive_download(
#     file = scan_csvs$id[i],
#     path = local_path,
#     overwrite = TRUE,
#     verbose = FALSE
#   )
#   
#   # 1. Read the header row (Row 2, so skip 1)
#   header <- read_excel(local_path, sheet = 2, skip = 1, n_max = 1, col_names = FALSE)
#   
#   # Convert to a character vector and clean empty names
#   col_names <- as.character(unlist(header[1, ]))
#   col_names[col_names == ""] <- paste0("X_Unused_", seq_along(col_names[col_names == ""]))
#   
#   # Clean up column names to ensure consistency (e.g., "200.00 nm" -> "X200.00.nm")
#   col_names <- make.names(col_names, unique = TRUE)
#   
#   # 2. Define strict column types to prevent incorrect coercion
#   num_cols <- length(col_names)
#   
#   # Create a vector of column types: Col 1='DateTime' (date), Col 2='Measured Status' (text), 
#   # and all others (spectral data) as 'numeric'.
#   col_type_vector <- c(
#     'date',       # Column 1: Date/Time (readxl interprets Excel date formats)
#     'text',       # Column 2: Measured status (must be text)
#     rep('numeric', num_cols - 2) # Columns 3 to end: Spectral data (must be numeric)
#   )
#   
#   # 3. Read the data starting from row 5 (skip 4)
#   data <- read_excel(
#     local_path,
#     sheet = 2,
#     skip = 4,
#     col_names = col_names,
#     col_types = col_type_vector
#   )
#   
#   # 4. Final cleaning and standardization of essential column names
#   colnames(data)[1] <- "DateTime"
#   colnames(data)[2] <- "Measured_Status"
#   
#   # Store the clean data frame
#   scan_list[[file_name]] <- data
# }
# 
# # Clean up downloaded files (optional but recommended)
# # unlink("googledrive_temp", recursive = TRUE)
# 
# ## -------------------------------------------------------------------
# 
# ##############################################################
# #### 2. COMBINE DATA FRAMES BY SITE WITH ROBUST ALIGNMENT ####
# ##############################################################
# site_names <- c("SSM01", "SSM20", "SST13")
# 
# # Group files in `scan_list` by site name
# scan_list_by_site <- lapply(site_names, function(site) {
#   site_files <- names(scan_list)[grepl(site, names(scan_list))] 
#   scan_list[site_files]
# })
# names(scan_list_by_site) <- site_names
# 
# # Combine data for each site with explicit column alignment (to prevent data repetition)
# combined_by_site <- lapply(scan_list_by_site, function(site_data_list) {
#   
#   # 1. Find the canonical list of all column names across all files for this site
#   all_names <- unique(unlist(lapply(site_data_list, names)))
#   
#   # 2. Align columns of every data frame in the list
#   site_data_list_aligned <- lapply(site_data_list, function(df) {
#     missing_cols <- setdiff(all_names, names(df))
#     
#     # Add missing columns, filling with NA_real_ to preserve numeric type
#     for (col in missing_cols) {
#       df[[col]] <- NA_real_ 
#     }
#     
#     # Reorder columns to ensure consistent binding order
#     df <- df[, all_names]
#     
#     return(df)
#   })
#   
#   # 3. Bind rows and clean duplicates/sorting
#   bind_rows(site_data_list_aligned) %>%
#     arrange(DateTime) %>%
#     # Use distinct() to handle repeated timestamps: .keep_all = TRUE keeps the first entry found
#     distinct(DateTime, .keep_all = TRUE) 
# })
# 
# ## -------------------------------------------------------------------
# 
# #####################################################################
# ## 3. FINAL OUTPUT (OPTIONAL: SAVE TO CSV)
# #####################################################################
# # Example: Access the combined data for the SSM20 site
# # combined_data_ssm20 <- combined_by_site[["SSM20"]]
# 
# # Optional: Save the combined data frames to CSV files
# # Create output directory
# if (!dir.exists("data_combined")) {
#   dir.create("data_combined")
# }
# 
# lapply(names(combined_by_site), function(site) {
#   file_path <- file.path("data_combined", paste0(site, "_combined_abs.csv"))
#   write.csv(
#     combined_by_site[[site]], 
#     file_path,
#     # Crucially, write_csv defaults to row.names=FALSE and is generally cleaner than write.csv
#     na = "" # Specify how NAs should be written (as empty string)
#   )
#   message(paste("Saved combined data for", site, "to", file_path))
# })
