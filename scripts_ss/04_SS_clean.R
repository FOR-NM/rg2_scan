##==============================================================================
## Project: QuEST - Script to tidy up South Sandy scan data and plot it
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================
library(googledrive) # download docs from Drive
library(tidyverse)
library(readxl) # to read Excel
library(lubridate) # edit date format
library(ggplot2)

########################################
#### Clear folders that we will use ####
########################################
# list and delete all files in the folder
files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

#####################
#### Import Data ####
#####################
# load data from Google Drive. his is the "merged" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1qpsqrmcnALNS9OVtoIDICdEuW5LkVuIR")
scan_csvs <- googledrive::drive_ls(path = scan, type = "csv")
3

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
  header <- read.csv(local_path)
  
  # read the data starting from row 4 using the header as column names
  data <- read.csv(local_path)
  
  # store the data in the list
  scan_list[[scan_csvs$name[i]]] <- data
}

head(scan_list)

#### remove abs files from list ####
# we just want merged parameters and abs file
# check position of abs and params files
# scan_list = scan_list[-c(1:3)]

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
    )
  # ensure numeric variables are converted to numeric
  df <- df %>%
    mutate(across(c(DOC_mg.l, NO3.N_mg.l, NO3_mg.l, TOC_mg.l, TSS_mg.l), as.numeric)) %>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Mountain"))
  
  return(df)
})

##################################################################################
#### Count number of service dates (No Medium) and 'ABOVE' and 'BELOW' values ####
##################################################################################
# when scan is out of water it records as NO_MEDIUM
# replace 'NO_MEDIUM' values with NA 
# also when it reads < lower error limit or  > upper error limit, it flags as 'VAL_BELOW' or 'VAL_ABOVE'
# replace 'VAL_BELOW' or 'VAL_ABOVE' flagged values with NA 

### first, count how many logs with , 'VAL_BELOW' or 'VAL_ABOVE' each one has ###
# initialize a list to store the counts for each file
count_list <- list()

# loop over each data frame in the list
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

#### save data ####
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
    mutate(across(c(DOC_mg.l, NO3.N_mg.l, NO3_mg.l, TOC_mg.l, TSS_mg.l), as.numeric)) %>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Mountain"))
  
  # define status values to replace with NA
  status_values_to_replace <- c("NO_MEDIUM", "VAL_BELOW:NO_MEDIUM", "VAL_BELOW", "VAL_ABOVE")
  
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
  
  # debug: Print the spectral columns found
  print(paste("Spectral columns in", deparse(substitute(df)), ":", toString(spectra_cols)))
  
  # if there are spectral columns, clean them
  if (length(spectra_cols) > 0) {
    # loop through each spectral column and apply the NA logic based on status
    for (col in spectra_cols) {
      # debug: Check which column is being processed
      print(paste("Processing spectral column:", col))
      
      # apply the NA logic based on status values
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

#####################################
#### Plot all variables together ####
#####################################
# plot after filtering pre-deployed and out of water times
plot_variables <- function(df, file_name) {
  ggplot(data = df) +
    geom_line(aes(x = DateTime, y = TSS_mg.l_clean, color = 'TSS')) +
    geom_line(aes(x = DateTime, y = TOC_mg.l_clean, color = 'TOC')) +
    geom_line(aes(x = DateTime, y = NO3.N_mg.l_clean, color = 'NO3.N')) +
    geom_line(aes(x = DateTime, y = NO3_mg.l_clean, color = 'NO3')) +
    geom_line(aes(x = DateTime, y = DOC_mg.l_clean, color = 'DOC')) +
    scale_x_datetime(date_breaks = "2 days", date_labels = "%m/%d") +
    ggtitle(file_name) +
    theme(axis.text.x = element_text(angle = 45)) +
    ylab("Measured")
}

# plot
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
    dplyr::select("DateTime", "TSS_mg.l_clean", "TOC_mg.l_clean", "NO3.N_mg.l_clean", "NO3_mg.l_clean", "DOC_mg.l_clean") %>%
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

# generate plots
print(plot_variables(scan_list[[1]], scan_csvs$name[1]))
print(plot_variables(scan_list[[2]], scan_csvs$name[2]))
print(plot_variables(scan_list[[3]], scan_csvs$name[3]))

### save figures to folder ###
#for (i in seq_along(scan_filtered)) {
 # ggsave(paste0("scan_figs/", scan_csvs$name[i], "_separate.png"), plot_variables(scan_filtered[[i]], scan_csvs$name[i]))
#}

####################################
#### Save cleaned data to Drive ####
####################################
# format DateTime column
scan_list <- lapply(scan_list, function(df) {
  df$DateTime <- format(df$DateTime, "%Y-%m-%d %H:%M:%S") 
  return(df)
})

lapply(names(scan_list), function(site) {
  write.csv(scan_list[[site]], file.path("data/", paste0(site, "_clean.csv")))
})

lapply(names(scan_list), function(site) {
  file <- paste0("data/", site, "_clean.csv")
  # this is the in use folder
  drive_folder_id <- "1qpsqrmcnALNS9OVtoIDICdEuW5LkVuIR"
  # Upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
})
