##==============================================================================
## Project: QuEST - Script to tidy up South Sandy scan data and plot it
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================
library(googledrive) # Download docs from Drive
library(tidyverse)
library(readxl) # to read Excel
library(lubridate) # edit date format
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
# Load data from Google Drive. his is the "merged" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1gAfaUZKCoarEaSrPnhdUXy5lQ49Z_xm1")
scan_csvs <- googledrive::drive_ls(path = scan, type = "csv")
3

# create empty list to store data frames
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
  
  # read the header row (row 2)
  header <- read.csv(local_path)
  
  # read the data starting from row 4 using the header as column names
  data <- read.csv(local_path)
  
  # store the data in the list
  scan_list[[scan_csvs$name[i]]] <- data
}

head(scan_list)

#################
#### Tidying ####
#################
# change some names for easier manipulation
scan_list <- lapply(scan_list, function(df) {
  # rename columns by matching the existing names
  df <- df %>%
    dplyr::rename(
      DOC_mg.l = DOCeq..mg.l....Measured.value,
      NO3.N_mg.l = NO3.Neq..mg.l....Measured.value,
      NO3_mg.l = NO3eq..mg.l....Measured.value,
      TOC_mg.l = TOCeq..mg.l....Measured.value,
      TSS_mg.l = TSSeq..mg.l....Measured.value,
      Temp_C = Temperature...C....Measured.value
    )
  # ensure numeric variables are converted to numeric
  df <- df %>%
    mutate(across(c(DOC_mg.l, NO3.N_mg.l, NO3_mg.l, TOC_mg.l, TSS_mg.l, Temp_C), as.numeric)) %>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Mountain"))
  
  return(df)
})

##################################################################################
#### Count number of service dates (No Medium) and 'ABOVE' and 'BELOW' values ####
##################################################################################

# When scan is out of water it records as NO_MEDIUM
# replace 'NO_MEDIUM' values with NA 
# Also when it reads < lower error limit or  > upper error limit, it flags as 'VAL_BELOW' or 'VAL_ABOVE'
# replace 'VAL_BELOW' or 'VAL_ABOVE' flagged values with NA 

### first, count how many logs with , 'VAL_BELOW' or 'VAL_ABOVE' each one has ###
# Initialize a list to store the counts for each file
count_list <- list()

# Loop over each data frame in the list
for (file_name in names(scan_list)) {
  
  # Get the data frame
  data <- scan_list[[file_name]]
  
  # filter to only character columns
  char_data <- data[, sapply(data, is.character)]
  
  # count the number of rows that contain VAL_BELOW, VAL_ABOVE, or NO_MEDIUM
  val_below_count <- sum(apply(char_data, 1, function(row) any(row == "VAL_BELOW", na.rm = TRUE)))
  val_above_count <- sum(apply(char_data, 1, function(row) any(row == "VAL_ABOVE", na.rm = TRUE)))
  no_medium_count <- sum(apply(char_data, 1, function(row) any(row == "NO_MEDIUM", na.rm = TRUE)))
  
  # store the counts in a data frame
  count_list[[file_name]] <- data.frame(
    File = file_name,
    VAL_BELOW = val_below_count,
    VAL_ABOVE = val_above_count,
    NO_MEDIUM = no_medium_count
  )
}

# combine all the individual data frames into one
final_count_table <- do.call(rbind, count_list)

# print the final table
print(final_count_table)

### save data ###
# save the final table to a CSV file
# write.csv(final_count_table, "final_NAcount_table.csv", row.names = TRUE)

##################################################
#### Clean  service dates (out of water days) ####
##################################################
# apply changes to status columns across all data frames in the list
scan_list <- lapply(scan_list, function(df) {
  
  # rename columns by matching the existing names
  df <- df %>%
    rename(
      DOC_status = DOCeq..mg.l....Measured.status,  # rename the status column for DOC
      NO3.N_status = NO3.Neq..mg.l....Measured.status,  # rename the status column for NO3.N
      NO3_status = NO3eq..mg.l....Measured.status,  # rename the status column for NO3
      TOC_status = TOCeq..mg.l....Measured.status,  # rename the status column for TOC
      TSS_status = TSSeq..mg.l....Measured.status  # rename the status column for TSS
    )
  
  # ensure numeric variables are converted to numeric
  df <- df %>%
    mutate(across(c(DOC_mg.l, NO3.N_mg.l, NO3_mg.l, TOC_mg.l, TSS_mg.l, Temp_C), as.numeric)) %>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Mountain"))
  
  # Define status values to replace with NA
  status_values_to_replace <- c("NO_MEDIUM", "VAL_BELOW:NO_MEDIUM")
  
  # create new cleaned columns (e.g., DOC_clean, NO3_clean) and set to NA if the status column has invalid values
  df <- df %>%
    mutate(
      DOC_mg.l_clean = ifelse(DOC_status %in% status_values_to_replace, NA, DOC_mg.l),
      NO3.N_mg.l_clean = ifelse(NO3.N_status %in% status_values_to_replace, NA, NO3.N_mg.l),
      NO3_mg.l_clean = ifelse(NO3_status %in% status_values_to_replace, NA, NO3_mg.l),
      TOC_mg.l_clean = ifelse(TOC_status %in% status_values_to_replace, NA, TOC_mg.l),
      TSS_mg.l_clean = ifelse(TSS_status %in% status_values_to_replace, NA, TSS_mg.l)
    )
  
  # find all spectral columns (those starting with "X" and ending with ".nm")
  spectra_cols <- grep("^X[0-9]+\\.[0-9]+\\.nm$", colnames(df), value = TRUE)
  
  # Debug: Print the spectral columns found
  print(paste("Spectral columns in", deparse(substitute(df)), ":", toString(spectra_cols)))
  
  # If there are spectral columns, clean them
  if (length(spectra_cols) > 0) {
    # Loop through each spectral column and apply the NA logic based on status
    for (col in spectra_cols) {
      # Debug: Check which column is being processed
      print(paste("Processing spectral column:", col))
      
      # Apply the NA logic based on status values
      df[[col]] <- ifelse(
        df$DOC_status %in% status_values_to_replace |
          df$NO3.N_status %in% status_values_to_replace |
          df$NO3_status %in% status_values_to_replace |
          df$TOC_status %in% status_values_to_replace |
          df$TSS_status %in% status_values_to_replace,
        NA, df[[col]]
      )
    }
  }
  
  # return the cleaned dataframe
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
service$datetimePT<-as.POSIXct(service$datetime, 
                               format = "%Y-%m-%d %H:%M",
                               tz="US/Pacific")

# remove rows with no exact times & make one new for deployed
deployedtimes = service[!is.na(service$datetimePT),]
deployedtimes = service[service$observation == "deployed", ]

###############################################
#### Add instrument name or serial number  ####
###############################################
# create function to extract file name (they all start with B)
extract_id <- function(file_name) {
  str_extract(file_name, "D\\w+\\_")
}

# Apply function to add extracted name to all data frames 
scan_filtered <- mapply(function(df, file_name) {
  df <- add_column(df, serial_number = extract_id(file_name))
  return(df)
}, scan_list, scan_csvs$name, SIMPLIFY = FALSE)

#############################################################
#### Delete all the rows before the deployment date-time ####
#############################################################
# the first few rows before deployment are usually junk. Let's get rid of those
# create function 
scan_filtered1 <- lapply(scan_filtered, function(df) {
  # extract each s::can name/serial number
  serial_number <- df$serial_number[1]
  # extract the deployed time of each instrument 
  deployed_time <- service$datetimePT[service$site == serial_number & service$observation == "deployed"]
  
  # filter, keep data that is equal to or after the deployed time
  df <- df %>% filter(DateTime >= deployed_time)
  return(df)
})

#####################################
#### Plot all variables together ####
#####################################
# plot after filtering pre-deployed and out of water times
plot_variables <- function(df, file_name) {
  ggplot(data = df) +
    geom_line(aes(x = DateTime, y = Temp_C, color = 'Temp')) +
    geom_line(aes(x = DateTime, y = TSS_mg.l_clean, color = 'TSS')) +
    geom_line(aes(x = DateTime, y = TOC_mg.l_clean, color = 'TOC')) +
    geom_line(aes(x = DateTime, y = NO3.N_mg.l_clean, color = 'NO3.N')) +
    geom_line(aes(x = DateTime, y = NO3_mg.l_clean, color = 'NO3')) +
    geom_line(aes(x = DateTime, y = DOC_mg.l_clean, color = 'DOC')) +
    scale_x_datetime(date_breaks = "5 days", date_labels = "%m/%d") +
    ggtitle(file_name) +
    theme(axis.text.x = element_text(angle = 45)) +
    ylab("Measured")
}

# plot not cleaned
print(plot_variables(scan_list[[1]], scan_csvs$name[1]))
print(plot_variables(scan_list[[2]], scan_csvs$name[2]))
print(plot_variables(scan_list[[3]], scan_csvs$name[3]))

# plot cleaned
print(plot_variables(scan_list[[1]], scan_csvs$name[1]))
print(plot_variables(scan_list[[2]], scan_csvs$name[2]))
print(plot_variables(scan_list[[3]], scan_csvs$name[3]))

# save figures to folder
#for (i in seq_along(scan_filtered)) {
 # ggsave(paste0("scan_figs/", scan_csvs$name[i], "_Measured.png"), plot_variables(scan_filtered[[i]], scan_csvs$name[i]))
#}

#####################################
#### Plot all variables separate ####
#####################################
# function to plot each variable separately in the same panel
plot_variables <- function(df, file_name) {
  # ensure column selection works correctly
  df_long <- df %>%
    dplyr::select("DateTime", "Temp_C", "TSS_mg.l_clean", "TOC_mg.l_clean", "NO3.N_mg.l_clean", "NO3_mg.l_clean", "DOC_mg.l_clean") %>%
    pivot_longer(cols = -DateTime, names_to = "Variable", values_to = "Value")
  
  # generate the plot
  ggplot(data = df_long, aes(x = DateTime, y = Value, color = Variable)) +
    geom_line() +
    facet_wrap(~Variable, scales = "free_y", ncol = 1) +  # separate plot for each variable, stacked vertically
    scale_x_datetime(date_breaks = "7 days", date_labels = "%m/%d") +
    ggtitle(file_name) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Measured Value") +
    theme(legend.position = "none")  # Hide legend since we have separate panels
}

# plot not cleaned
print(plot_variables(scan_list[[1]], scan_csvs$name[1]))
print(plot_variables(scan_list[[2]], scan_csvs$name[2]))
print(plot_variables(scan_list[[3]], scan_csvs$name[3]))

# plot cleaned
print(plot_variables(scan_filtered1[[1]], scan_csvs$name[1]))
print(plot_variables(scan_filtered1[[2]], scan_csvs$name[2]))
print(plot_variables(scan_filtered1[[3]], scan_csvs$name[3]))

### save figures to folder ###
for (i in seq_along(scan_list)) {
  ggsave(paste0("scan_figs/", scan_csvs$name[i], "_separate.png"), plot_variables(scan_list[[i]], scan_csvs$name[i]))
}

####################################
#### Save cleaned data to Drive ####
####################################

# ensure DateTime column is properly formatted
scan_filtered1 <- lapply(scan_filtered1, function(df) {
  df$DateTime <- format(df$DateTime, "%Y-%m-%d %H:%M:%S") 
  return(df)
})

# function to remove file extension
remove_extension <- function(file_name) {
  sub("\\.[[:alnum:]]+$", "", file_name)
}

# Loop through each data frame in the list
for (i in seq_along(scan_filtered1)) {
  # Access the current data frame
  df <- scan_filtered1[[i]]
  
  # Define the file name and path
  clean_name <- remove_extension(scan_csvs$name[i])
  file_name <- paste0("googledrive/", clean_name, "_clean.csv")
  
  # save the new data frame to a CSV file
  write.csv(df, file_name, row.names=FALSE, quote=FALSE)
  
  # Define the target folder ID in Google Drive
  drive_folder_id <- "1C7Z6v38dyhLKO6kGNzcjSRfTDl64j0WC"
  
  # Upload the file to the specified Google Drive folder
  drive_upload(media = file_name, path = as_id(drive_folder_id))
}
