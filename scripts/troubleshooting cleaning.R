# Load data from Google Drive
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1DZktlQUHaot_r4e_fD9ip6zcxHWqslMP")
scan_csvs <- googledrive::drive_ls(path = scan, type = "xlsx")
3

# Create empty list to store data frames
scan_list <- list()

# Loop over each file in `scan_csvs` and read the data
scan_list <- lapply(seq_along(scan_csvs$id), function(i) {
  local_path <- file.path("googledrive", scan_csvs$name[i])
  
  # Download and read the file
  googledrive::drive_download(file = scan_csvs$id[i], path = local_path, overwrite = TRUE)
  header <- read_excel(local_path, skip = 1, n_max = 1, col_names = FALSE)
  col_names <- as.character(unlist(header[1, ]))
  col_names[col_names == ""] <- paste0("X", seq_along(col_names[col_names == ""]))
  
  # Return the data frame
  read_excel(local_path, skip = 4, col_names = col_names)
})

names(scan_list) <- scan_csvs$name

#################
#### Tidying ####
#################
# Change some names for easier manipulation
scan_list <- lapply(scan_list, function(df) {
  colnames(df)[c(1, 2, 6, 8, 12, 14, 16, 11)] <- c("dateTime", "DOC", "NO3-N", "NO3", "TOC", "TSS", "Temp", "Voltage")
  
  # Make sure numeric variables are numeric
  df <- df %>%
    mutate(across(c(DOC, `NO3-N`, NO3, TOC, TSS, Temp), as.numeric)) %>%
    mutate(dateTime = as.POSIXct(dateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Mountain"))
  
  return(df)
})

#########################################################################################
#### Count number of service dates (out of water days) and 'ABOVE' and 'BELOW' values####
#########################################################################################

# When scan is out of water it records as NO_MEDIUM
# Replace 'NO_MEDIUM' values with NA 
# Also when it reads < lower error limit or  > upper error limit, it flags as 'VAL_BELOW' or 'VAL_ABOVE'
# Replace 'VAL_BELOW' or 'VAL_ABOVE' flagged values with NA 

### First, count how many logs with , 'VAL_BELOW' or 'VAL_ABOVE' each one has ###
# Initialize a list to store the counts for each file
count_list <- list()

# Loop over each data frame in the list
for (file_name in names(scan_list)) {
  
  # Get the data frame
  data <- scan_list[[file_name]]
  
  # Filter to only character columns
  char_data <- data[, sapply(data, is.character)]
  
  # Count the number of rows that contain VAL_BELOW, VAL_ABOVE, or NO_MEDIUM
  val_below_count <- sum(apply(char_data, 1, function(row) any(row == "VAL_BELOW", na.rm = TRUE)))
  val_above_count <- sum(apply(char_data, 1, function(row) any(row == "VAL_ABOVE", na.rm = TRUE)))
  no_medium_count <- sum(apply(char_data, 1, function(row) any(row == "NO_MEDIUM", na.rm = TRUE)))
  
  # Store the counts in a data frame
  count_list[[file_name]] <- data.frame(
    File = file_name,
    VAL_BELOW = val_below_count,
    VAL_ABOVE = val_above_count,
    NO_MEDIUM = no_medium_count
  )
}

# Combine all the individual data frames into one
final_count_table <- do.call(rbind, count_list)

# Print the final table
print(final_count_table)

############################
#### Save data to Drive ####
############################

# Save the final table to a CSV file
write.csv(final_count_table, "final_NAcount_table.csv", row.names = TRUE)

###################################################################################
#### Clean out service dates (out of water days) and 'ABOVE' and 'BELOW' values####
###################################################################################
#### Now we change them to NAs ####
# Apply the transformation across each data frame in the list
scan_filtered <- lapply(scan_list, function(df) {
  
  # Identify all the measured value columns and their corresponding status columns
  measured_cols <- c("DOC", "NO3-N", "NO3", "TOC", "TSS")
  status_cols <- paste0(measured_cols, "eq [mg/l] - Measured status")
  
  # Loop over each measured column and its corresponding status column
  for (i in seq_along(measured_cols)) {
    measured_col <- measured_cols[i]
    status_col <- status_cols[i]
    new_col_name <- paste0(measured_col, "_clean")
    
    # Check if the status column exists in the data frame
    if (status_col %in% colnames(df)) {
      # Replace with NA if status is "NO_MEDIUM", "VAL_ABOVE", or "VAL_BELOW"
      df[[new_col_name]] <- ifelse(df[[status_col]] %in% c("NO_MEDIUM", "VAL_ABOVE", "VAL_BELOW"), NA, df[[measured_col]])
    } else {
      # If status column doesn't exist, just copy the measured column to new column
      df[[new_col_name]] <- df[[measured_col]]
    }
  }
  
  return(df)
})

##############################################################
#### Check for 'VAL_BELOW' and 'VAL_ABOVE' removed values ####
##############################################################
#Compare the min and max values before and after filtering

# Summary statistics for the cleaned columns
lapply(scan_list, function(df) {
  summary(df[, !grepl("_clean", colnames(df))])
})

# Summary statistics for the cleaned columns
lapply(scan_filtered, function(df) {
  summary(df[, grepl("_clean", colnames(df))])
})

# Check if 'NA' is present where status was flagged
check_na_replacement <- function(df, measured_cols, status_cols) {
  result <- list()
  for (i in seq_along(measured_cols)) {
    measured_col <- measured_cols[i]
    status_col <- status_cols[i]
    clean_col <- paste0(measured_col, "_clean")
    
    # Verify if NA was set correctly
    result[[clean_col]] <- any(is.na(df[[clean_col]])) && all(
      !df[[status_col]] %in% c("NO_MEDIUM", "VAL_ABOVE", "VAL_BELOW") | is.na(df[[clean_col]])
    )
  }
  return(result)
}

# Apply the check function
checks <- lapply(scan_filtered, check_na_replacement, measured_cols = c("DOC", "NO3-N", "NO3", "TOC", "TSS"), status_cols = paste0(c("DOC", "NO3-N", "NO3", "TOC", "TSS"), "eq [mg/l] - Measured status"))
print(checks)

###############################
#### Load Servicing Times #####
###############################
# Download and read sensor event log
service_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1KdjN1nmeeqtgxk6k3rImtb-wpVXVyLk4")
googledrive::drive_download(as_id(service_tibble$id[service_tibble$name == "sensor_event_log"]), overwrite = TRUE, path = "googledrive/sensor_event_log.xlsx")
# Let's call the file "service"
service <- readxl::read_excel("googledrive/sensor_event_log.xlsx") %>%
  # Filter using
  filter(model == "s::can", observation %in% c("out of water", "deployed"), site_code == "NM") %>%
  mutate(datetimeMT = as.POSIXct(paste(date, format(as.POSIXct(time, format = "%H:%M:%S"), "%H:%M:%S")), format = "%Y-%m-%d %H:%M", tz = "US/Mountain"))

###############################################
#### Add instrument name or serial number  ####
###############################################
# Create function to extract file name (they all start with B)
extract_id <- function(file_name) {
  str_extract(file_name, "B\\w+")
}

# Apply function to add extracted name to all data frames 
scan_filtered <- mapply(function(df, file_name) {
  df <- add_column(df, serial_number = extract_id(file_name))
  return(df)
}, scan_filtered, scan_csvs$name, SIMPLIFY = FALSE)

#############################################################
#### Delete all the rows before the deployment date-time ####
#############################################################
# The first few rows before deployment are usually junk. Let's get rid of those
# Create function 
scan_filtered <- lapply(scan_filtered, function(df) {
  # Extract each s::can name/serial number
  serial_number <- df$serial_number[1]
  # Extract the deployed time of each instrument 
  deployed_time <- service$datetimeMT[service$serial_number == serial_number & service$observation == "deployed"]
  
  # Filter, keep data that is over or equal the deployed time
  df <- df %>% filter(dateTime >= deployed_time)
  return(df)
})
