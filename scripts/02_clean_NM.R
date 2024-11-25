##==============================================================================
## Project: QuEST - Script to tidy up a bit scan data and plot it
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================
library(dataRetrieval) # Download USGS discharge data
library(googledrive) # Download docs from Drive
library(tidyverse)
library(readxl) # to read Excel
library(lubridate) # Edit date format
library(xts) # Time series
library(ggplot2)

########################################
#### Clear folders that we will use ####
########################################
# List and delete all files in the folder
files <- list.files(path = "scan_figs", full.names = TRUE)
file.remove(files)

files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

#####################
#### Import Data ####
#####################
# Load data from Google Drive
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1np2B4bSWaNMIYE2FHL3YOnZ20FRudsEy")
scan_csvs <- googledrive::drive_ls(path = scan, type = "xlsx")
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
  
  # Read the data starting from row 4 using the header as column names
  data <- read_excel(local_path, skip = 4, col_names = col_names)
  
  # Store the data in the list
  scan_list[[scan_csvs$name[i]]] <- data
}

#################
#### Tidying ####
#################
# Change some names for easier manipulation
scan_list <- lapply(scan_list, function(df) {
  colnames(df)[c(1, 2, 6, 8, 12, 14, 16, 11)] <- c("dateTime", "DOC", "NO3N", "NO3", "TOC", "TSS", "Temp", "Voltage")
  
  # Make sure numeric variables are numeric
  df <- df %>%
    mutate(across(c(DOC, NO3N, NO3, TOC, TSS, Temp), as.numeric)) %>%
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

#### Save data to Drive ####
# Save the final table to a CSV file
# write.csv(final_count_table, "final_NAcount_table.csv", row.names = TRUE)

#####################################################
#### Clean out service dates (out of water days) ####
#####################################################
#### Now we change them to NAs ####
# Apply the transformation across each data frame in the list
scan_filtered <- lapply(scan_list, function(df) {
  
  # Identify all the measured value columns and their corresponding status columns
  measured_cols <- c("DOC", "NO3N", "NO3", "TOC", "TSS")
  status_cols <- paste0(measured_cols, "eq [mg/l] - Measured status")
  
  # Loop over each measured column and its corresponding status column
  for (i in seq_along(measured_cols)) {
    measured_col <- measured_cols[i]
    status_col <- status_cols[i]
    new_col_name <- paste0(measured_col, "_clean")
    
    # Check if the status column exists in the data frame
    if (status_col %in% colnames(df)) {
      # Replace with NA if status is "NO_MEDIUM"
      df[[new_col_name]] <- ifelse(df[[status_col]] %in% c("NO_MEDIUM"), NA, df[[measured_col]])
    } else {
      # If status column doesn't exist, just copy the measured column to new column
      df[[new_col_name]] <- df[[measured_col]]
    }
  }
  
  return(df)
})

###############################
#### Load Servicing Times #####
###############################

# get data from googledrive
service_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1KdjN1nmeeqtgxk6k3rImtb-wpVXVyLk4")
googledrive::drive_download(as_id(service_tibble$id[service_tibble$name=="sensor_event_log"]), overwrite = TRUE,path="googledrive/sensor_event_log.xlsx")

# read in file and filter to s::can service days and deployments
service = readxl::read_excel("googledrive/sensor_event_log.xlsx")
service = service[service$model=="s::can",]
service = service[service$observation=="out of water" | service$observation=="deployed",]


# format date and time
service$date = as.Date(service$date)
service$time <- format(as.POSIXct(service$time, format="%H:%M:%S"), "%H:%M:%S")
service$datetime = paste(service$date,  service$time, sep = " ")
# convert to POIXct and set timezone
service$datetimeMT<-as.POSIXct(service$datetime, 
                               format = "%Y-%m-%d %H:%M",
                               tz="US/Mountain")

# remove rows with no exact times & make one new for deployed
deployedtimes = service[!is.na(service$datetimeMT),]
deployedtimes = service[service$observation == "deployed", ]

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
scan_filtered1 <- lapply(scan_filtered, function(df) {
  # Extract each s::can name/serial number
  serial_number <- df$serial_number[1]
  # Extract the deployed time of each instrument 
  deployed_time <- service$datetimeMT[service$serial_number == serial_number & service$observation == "deployed"]
  
  # Filter, keep data that is equal to or after the deployed time
  df <- df %>% filter(dateTime >= deployed_time)
  return(df)
})

#####################################
#### Plot all variables together ####
#####################################
# Plot after filtering pre-deployed and out of water times
plot_variables <- function(df, file_name) {
  ggplot(data = df) +
    geom_line(aes(x = dateTime, y = Temp, color = 'Temperature')) +
    geom_line(aes(x = dateTime, y = TSS_clean, color = 'TSS')) +
    geom_line(aes(x = dateTime, y = TOC_clean, color = 'TOC')) +
    geom_line(aes(x = dateTime, y = NO3N_clean, color = 'NO3N')) +
    geom_line(aes(x = dateTime, y = NO3_clean, color = 'NO3')) +
    geom_line(aes(x = dateTime, y = DOC_clean, color = 'DOC')) +
    scale_x_datetime(date_breaks = "2 days", date_labels = "%m/%d") +
    ggtitle(file_name) +
    theme(axis.text.x = element_text(angle = 45)) +
    ylab("Measured")
}

# Plot
print(plot_variables(scan_filtered1[[1]], scan_csvs$name[1]))
print(plot_variables(scan_filtered1[[2]], scan_csvs$name[2]))
print(plot_variables(scan_filtered1[[3]], scan_csvs$name[3]))

# Save figures to folder
for (i in seq_along(scan_filtered)) {
  ggsave(paste0("scan_figs/", scan_csvs$name[i], "_Measured.png"), plot_variables(scan_filtered[[i]], scan_csvs$name[i]))
}

#####################################
#### Plot all variables separate ####
#####################################

# Function to plot each variable separately in the same panel
plot_variables <- function(df, file_name) {
  # Ensure column selection works correctly
  df_long <- df %>%
    dplyr::select("dateTime", "Temp", "TSS_clean", "TOC_clean", "NO3N_clean", "NO3_clean", "DOC_clean") %>%
    pivot_longer(cols = -dateTime, names_to = "Variable", values_to = "Value")
  
  # Generate the plot
  ggplot(data = df_long, aes(x = dateTime, y = Value, color = Variable)) +
    geom_line() +
    facet_wrap(~Variable, scales = "free_y", ncol = 1) +  # Separate plot for each variable, stacked vertically
    scale_x_datetime(date_breaks = "7 days", date_labels = "%m/%d") +
    ggtitle(file_name) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Measured Value") +
    theme(legend.position = "none")  # Hide legend since we have separate panels
}

# Generate plots
print(plot_variables(scan_filtered1[[1]], scan_csvs$name[1]))
print(plot_variables(scan_filtered1[[2]], scan_csvs$name[2]))
print(plot_variables(scan_filtered1[[3]], scan_csvs$name[3]))

### Save figures to folder ###
for (i in seq_along(scan_filtered)) {
  ggsave(paste0("scan_figs/", scan_csvs$name[i], "_separate.png"), plot_variables(scan_filtered[[i]], scan_csvs$name[i]))
}

##################################
#### Pull USGS discharge data ####
##################################
# These are codes and functions specific to the USGS package (dataRetrieval)
retrieve_usgs_data <- function(start_date, end_date, site_no = "08315480", p_code = "00060") {
  #Retrieve the USGS discharge data as an instantaneous (uv) data type.
  usgs_data <- readNWISuv(siteNumbers = site_no, parameterCd = p_code, startDate = start_date, endDate = end_date)
  #Rename columns to more user-friendly names.
  usgs_data <- renameNWISColumns(usgs_data)
}

# Retrieve USGS data for different s::can sites, each has different deployment dates
santafeUSGS_20 <- retrieve_usgs_data("2024-05-08", "2024-11-14")
santafeUSGS_12 <- retrieve_usgs_data("2024-05-07", "2024-11-13")
santafeUSGS_21 <- retrieve_usgs_data("2024-06-27", "2024-10-29")

#####################################
#### Plot all variables separate ####
#####################################

# Function to retrieve and plot USGS data with separate facets for each variable
plot_usgs_faceted <- function(df, usgs_df, label) {
  # Convert to xts and merge data frames
  df_xts <- xts(df, order.by = df$dateTime)
  usgs_xts <- xts(usgs_df, order.by = usgs_df$dateTime)
  combined_xts <- merge(df_xts, usgs_xts, join = "outer")
  combined_df <- data.frame(dateTime = index(combined_xts), coredata(combined_xts))
  
  # Convert columns to numeric, if necessary
  combined_df <- combined_df %>% mutate(across(c(Temp, TSS_clean, TOC_clean, NO3N_clean, NO3_clean, DOC_clean, Flow_Inst), as.numeric))
  
  # Reshape data to long format for faceting
  combined_long <- combined_df %>%
    dplyr::select(dateTime, Temp, TSS_clean, TOC_clean, NO3N_clean, NO3_clean, DOC_clean, Flow_Inst) %>%
    pivot_longer(cols = -dateTime, names_to = "Variable", values_to = "Value")
  
  # Plot using ggplot with facet_wrap for each variable
  ggplot(data = combined_long, aes(x = dateTime, y = Value, color = Variable)) +
    geom_line() +
    facet_wrap(~Variable, scales = "free_y", ncol = 1) +  # Separate facet for each variable
    scale_x_datetime(date_breaks = "1 week", date_labels = "%m/%d") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab(label) +
    ggtitle(label) +
    theme(legend.position = "none")  # Hide the legend as it's redundant with faceting
}

# Plot
print(plot_usgs_faceted(scan_filtered1[[1]], santafeUSGS_12, scan_csvs$name[1]))
print(plot_usgs_faceted(scan_filtered1[[2]], santafeUSGS_20, scan_csvs$name[2]))
print(plot_usgs_faceted(scan_filtered1[[3]], santafeUSGS_21, scan_csvs$name[3]))

### Save figures to folder ###
for (i in seq_along(scan_filtered)) {
  # Match the correct USGS data with each scan
  usgs_data <- switch(i,
                      santafeUSGS_12,
                      santafeUSGS_20,
                      santafeUSGS_21)
  
  # Generate the plot
  plot <- plot_usgs_faceted(scan_filtered1[[i]], usgs_data, scan_csvs$name[i])
  
  # Save the plot to a file
  ggsave(paste0("scan_figs/", scan_csvs$name[i], "_sep-outlier.png"), plot)
}

####################################
#### Merge USGS and s::can data ####
####################################

# Function to merge USGS data with s::can data
merge_usgs_with_scan <- function(scan_df, usgs_df) {
  
  # Convert both data frames to xts objects
  scan_xts <- xts(scan_df, order.by = scan_df$dateTime)
  usgs_xts <- xts(usgs_df, order.by = usgs_df$dateTime)
  
  # Merge the xts objects
  combined_xts <- merge(scan_xts, usgs_xts, join = "outer")
  
  # Convert back to data frame
  combined_df <- data.frame(dateTime = index(combined_xts), coredata(combined_xts))
  
  return(combined_df)
}

# List of USGS data frames corresponding to scan_filtered
usgs_list <- list(
  santafeUSGS_12,
  santafeUSGS_20,
  santafeUSGS_21
)

# Merge USGS data with scan_filtered data frames
scan_with_usgs <- mapply(merge_usgs_with_scan, scan_filtered, usgs_list, SIMPLIFY = FALSE)

####################################
#### Save cleaned data to Drive ####
####################################

# Function to remove file extension
remove_extension <- function(file_name) {
  sub("\\.[[:alnum:]]+$", "", file_name)
}

# Loop through each data frame in the list
for (i in seq_along(scan_with_usgs)) {
  # Access the current data frame
  df <- scan_with_usgs[[i]]
  
  # Define the file name and path
  clean_name <- remove_extension(scan_csvs$name[i])
  file_name <- paste0("googledrive/", clean_name, "_filtered.csv")
  
  # Save the new data frame to a CSV file
  write.csv(df, file_name, row.names=FALSE, quote=FALSE)
  
  # Define the target folder ID in Google Drive
  drive_folder_id <- "1DZktlQUHaot_r4e_fD9ip6zcxHWqslMP"
  
  # Upload the file to the specified Google Drive folder
  drive_upload(media = file_name, path = as_id(drive_folder_id))
}
