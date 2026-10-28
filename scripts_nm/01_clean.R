##==============================================================================
## Project: QuEST - Script to tidy up a bit scan data and plot it
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================
library(googledrive) # download docs from Drive
library(tidyverse)
library(lubridate) # edit date format
library(ggplot2)

#####################
#### Import Data ####
#####################
# load data from Google drive. This is the "merge timestamps" folder
#scan <- googledrive::as_id("https://drive.google.com/drive/folders/1-dUxVn1hBWy2MpHeIjVt-2QSujpVhijy")
# list all CSV files in the folder
#scan_csvs <- googledrive::drive_ls(path = scan)
input_path<- "data/merged_params_and_abs/" #local path

scan<- input_path
scan_csvs<- list.files(path = scan, pattern = "\\.csv$")

# create empty list to store data frames
scan_list <- list()

# loop over each file in the `scan_csvs` data frame
for (i in seq_along(scan_csvs)) {
  # define the local file path
  local_path <- file.path(input_path, scan_csvs[i])
  
  # read the data starting from row 4 using the header as column names
  data <- read_csv(local_path)
  
  # store the data in the list
  scan_list[[scan_csvs[i]]] <- data
}

#################
#### Tidying ####
#################
# change some names for easier manipulation
scan_list <- lapply(scan_list, function(df) {
  # rename columns by matching the existing names
  df <- df %>%
    rename(
      'DOC_mg.l' = 'DOCeq..mg.l....Measured.value',
      'NO3.N_mg.l' = 'NO3.Neq..mg.l....Measured.value',
      'NO3_mg.l' =  'NO3eq..mg.l....Measured.value',
      'TOC_mg.l' = 'TOCeq..mg.l....Measured.value',
      'TSS_mg.l' = 'TSSeq..mg.l....Measured.value',
      'Temp_C' = "Temperature_21...C....Measured.value",
    )
  
  # ensure numeric variables are converted to numeric
  df <- df %>%
    mutate(across(c(DOC_mg.l, NO3.N_mg.l, NO3_mg.l, TOC_mg.l, TSS_mg.l, Temp_C), as.numeric)) %>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
  
  return(df)
})

#####################################################################
#### Plot all variables separate before removing no medium reads ####
#####################################################################
# function to plot each variable separately in the same panel
plot_variables <- function(df, file_name) {
  # ensure column selection works correctly
  df_long <- df %>%
    dplyr::select("DateTime", "Temp_C", "TSS_mg.l", "TOC_mg.l", "NO3.N_mg.l", "NO3_mg.l", "DOC_mg.l") %>%
    pivot_longer(cols = -DateTime, names_to = "Variable", values_to = "Value")
  
  # generate the plot
  ggplot(data = df_long, aes(x = DateTime, y = Value, color = Variable)) +
    geom_line() +
    facet_wrap(~Variable, scales = "free_y", ncol = 1) +  # separate plot for each variable, stacked vertically
    scale_x_datetime(date_breaks = "15 days", date_labels = "%m/%d") +
    ggtitle(file_name) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Measured Value") +
    theme(legend.position = "none")  # hide legend since we have separate panels
}

# generate plots
print(plot_variables(scan_list[[1]], scan_csvs[1]))
print(plot_variables(scan_list[[2]], scan_csvs[2]))
print(plot_variables(scan_list[[3]], scan_csvs[3]))
print(plot_variables(scan_list[[4]], scan_csvs[4]))
print(plot_variables(scan_list[[5]], scan_csvs[5]))

### save figures to folder ###
for (i in seq_along(scan_list)) {
  ggsave(paste0("scan_figs/", scan_csvs[i], "raw.png"), plot_variables(scan_list[[i]], scan_csvs[i]))
}

#########################################################################################
#### Count number of service dates (out of water days) and 'ABOVE' and 'BELOW' values####
#########################################################################################
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
  math_err_count <- sum(apply(char_data, 1, function(row) any(row == "MATH_ERR", na.rm = TRUE)))
  volt_low_count <- sum(apply(char_data, 1, function(row) any(row == "VOLT_LOW", na.rm = TRUE)))
  volt_high_count <- sum(apply(char_data, 1, function(row) any(row == "VOLT_HIGH", na.rm = TRUE)))
  hw_deffect_count <- sum(apply(char_data, 1, function(row) any(row == "HW_DEFECT", na.rm = TRUE)))
  
  # store the counts in a data frame
  count_list[[file_name]] <- data.frame(
    File = file_name,
    VAL_BELOW = val_below_count,
    VAL_ABOVE = val_above_count,
    NO_MEDIUM = no_medium_count,
    MATH_ERR = math_err_count,
    VOLT_LOW = volt_low_count,
    VOLT_HIGH = volt_high_count,
    HW_DEFECT = hw_deffect_count
  )
}

# combine all the individual data frames into one
final_count_table <- do.call(rbind, count_list)

# print the final table
print(final_count_table)

### save the final table to a CSV file
write.csv(final_count_table, "final_NAcount_table.csv", row.names = TRUE)

#################################################
#### Clean service dates (out of water days) ####
#################################################
# apply changes to status columns across all data frames in the list
scan_list <- lapply(scan_list, function(df) {
  
  # rename columns by matching the existing names
  df <- df %>%
    rename(
      'DOC_status' = 'DOCeq..mg.l....Measured.status',  # rename the status column for DOC
      'NO3.N_status' = 'NO3.Neq..mg.l....Measured.status',  # rename the status column for NO3.N
      'NO3_status' = 'NO3eq..mg.l....Measured.status',  # rename the status column for NO3
      'TOC_status' = 'TOCeq..mg.l....Measured.status',  # rename the status column for TOC
      'TSS_status' = 'TSSeq..mg.l....Measured.status'  # rename the status column for TSS
    )
  
  # ensure numeric variables are converted to numeric
  df <- df %>%
    mutate(across(c(DOC_mg.l, NO3.N_mg.l, NO3_mg.l, TOC_mg.l, TSS_mg.l, Temp_C), as.numeric)) %>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
  
  # define status values to replace with NA
  status_values_to_replace <- c("NO_MEDIUM", "VAL_BELOW:NO_MEDIUM")
  
  # overwrite the value columns in place with NA wherever their own status
  # flags a bad read -- no separate "_clean" columns. The *_status columns
  # (kept as-is) already say WHY a value is NA, so there's no need for an
  # extra flag column on top of that.
  df <- df %>%
    mutate(
      DOC_mg.l = ifelse(DOC_status %in% status_values_to_replace, NA, DOC_mg.l),
      NO3.N_mg.l = ifelse(NO3.N_status %in% status_values_to_replace, NA, NO3.N_mg.l),
      NO3_mg.l = ifelse(NO3_status %in% status_values_to_replace, NA, NO3_mg.l),
      TOC_mg.l = ifelse(TOC_status %in% status_values_to_replace, NA, TOC_mg.l),
      TSS_mg.l = ifelse(TSS_status %in% status_values_to_replace, NA, TSS_mg.l)
    )
  
  # find all spectral columns (those starting with "X" and ending with ".nm")
  spectra_cols <- grep("^X[0-9]+\\.[0-9]+\\.nm$", colnames(df), value = TRUE)
  
  # If there are spectral columns, clean them
  if (length(spectra_cols) > 0) {
    # loop through each spectral column and apply the NA logic based on status
    for (col in spectra_cols) {
      
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
  
  # return the cleaned dataframe -- without this, the last expression run
  # above is the if/for block, which evaluates to NULL, so lapply() would
  # silently collect a list of NULLs instead of the cleaned dataframes
  return(df)
})

#####################################
#### Plot all variables separate ####
#####################################
# function to plot each variable separately in the same panel, to do: rename 
plot_variables <- function(df, file_name) {
  # ensure column selection works correctly
  df_long <- df %>%
    dplyr::select("DateTime", "Temp_C", "TSS_mg.l", "TOC_mg.l", "NO3.N_mg.l", "NO3_mg.l", "DOC_mg.l") %>%
    pivot_longer(cols = -DateTime, names_to = "Variable", values_to = "Value")
  
  # generate the plot
  ggplot(data = df_long, aes(x = DateTime, y = Value, color = Variable)) +
    geom_line() +
    facet_wrap(~Variable, scales = "free_y", ncol = 1) +  # separate plot for each variable, stacked vertically
    scale_x_datetime(date_breaks = "15 days", date_labels = "%m/%d") +
    ggtitle(file_name) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Measured Value") +
    theme(legend.position = "none")  # hide legend since we have separate panels
}

# generate plots and save 
for (i in seq_along(scan_csvs)){
  print(plot_variables(scan_filtered[[i]], scan_csvs[i]))
  ggsave(paste0("scan_figs/", scan_csvs[i], ".png"), plot_variables(scan_filtered[[i]], scan_csvs[i]))
}

# ###################################
# #### Plot just a section of it #### To do: erase this? or place it somewhere else 
# ###################################
# Date1 <- as.Date("2025-08-25", "%Y-%m-%d")
# Date2 <- as.Date("2025-08-30", "%Y-%m-%d")
# 
# # create empty list to store data frames
# scan_subdf <- list()
# 
# # loop through each data frame in the list to change DateTime column name
# for (i in seq_along(scan_filtered1)) {
#   # Access the current data frame
#   df <- scan_filtered1[[i]]
#   
#   # change time frame
#   subdf <- df[df$DateTime < Date2 & df$DateTime > Date1,]
#   
#   # update the data frame in the list
#   scan_subdf[[i]] <- subdf
# }
# 
# # function to plot each variable separately in the same panel
# plot_variables <- function(df, file_name) {
#   # ensure column selection works correctly
#   df_long <- df %>%
#     dplyr::select("DateTime", "Temp_C", "TSS_mg.l", "TOC_mg.l", "NO3.N_mg.l", "NO3_mg.l", "DOC_mg.l") %>%
#     pivot_longer(cols = -DateTime, names_to = "Variable", values_to = "Value")
#   
#   # generate the plot
#   ggplot(data = df_long, aes(x = DateTime, y = Value, color = Variable)) +
#     geom_line() +
#     facet_wrap(~Variable, scales = "free_y", ncol = 1) +  # separate plot for each variable, stacked vertically
#     scale_x_datetime(date_breaks = "7 days", date_labels = "%m/%d") +
#     ggtitle(file_name) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#     ylab("Measured Value") +
#     theme(legend.position = "none")  # hide legend since we have separate panels
# }
# 
# # generate plots
# print(plot_variables(scan_subdf[[1]], scan_csvs$name[1]))
# print(plot_variables(scan_subdf[[2]], scan_csvs$name[2]))
# print(plot_variables(scan_subdf[[3]], scan_csvs$name[3]))
# 
# ### save figures to folder ###
# for (i in seq_along(scan_subdf)) {
#   ggsave(paste0("scan_figs/", scan_csvs$name[i], "_subdf.png"), plot_variables(scan_subdf[[i]], scan_csvs$name[i]))
# }

####################################
#### Save cleaned data to Drive ####
####################################
# function to remove file extension
remove_extension <- function(file_name) {
  sub("\\.[[:alnum:]]+$", "", file_name)
}

# loop through each data frame in the list
for (i in seq_along(scan_filtered)) {
  # access the current data frame
  df <- scan_filtered[[i]]
  
  # define the file name and path
  clean_name <- remove_extension(scan_csvs[i])
  file_name <- file.path("data/clean", paste0(clean_name, "_clean.csv"))
  
  # save the new data frame to a CSV file
  write.csv(df, file_name, row.names=FALSE, quote=FALSE)
  
  # define the target folder ID in Google Drive, this is the "clean" folder
  drive_folder_id <- "1j_I0brd1bzAfHlG3is0WRRayPR1MEcUs"
  
  # upload the file to the specified Google Drive folder
  drive_put(media = file_name, path = as_id(drive_folder_id))
}
