##==============================================================================
## Project: QuEST
## Script to visualize raw scan data from South Sandy
## Here we will plot some images but will not be saving cleaned data back. 
## It is just so see what your data looks like and understand what needs to get done
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(googledrive) #Download docs from Drive
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate) # Edit date format

########################################
#### Clear folders that we will use ####
########################################
# List and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

#################################
#### Import & Visualize Data ####
#################################
# Load data from Google drive, this is the "raw" folder
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

################################
#### Format DateTime column ####
################################

# Loop through each data frame in the list
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  
  # Convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$datetime, format = "%Y-%m-%d %H:%M:%S")
  # Update the data frame in the list
  scan_list[[i]] <- df
}

# Check the contents of the list and make sure there are no NAs
str(scan_list)


#################
#### Tidying #### 
#################
## Rename columns
# Loop through each data frame in the list
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  
  # Change names for easier handling
  df <- df %>%
    dplyr::rename(
      DOC_mg.l = 'DOCeq [mg/l] - Measured value',
      NO3.N_mg.l = 'NO3-Neq [mg/l] - Measured value',
      NO3_mg.l = 'NO3eq [mg/l] - Measured value',
      TOC_mg.l = 'TOCeq [mg/l] - Measured value',
      TSS_mg.l = 'TSSeq [mg/l] - Measured value',
      Temp_C = 'Temperature_19 [°C] - Measured value'
    )
  
  # Convert relevant columns to numeric
  cols_to_convert <- c("DOC_mg.l", "NO3.N_mg.l", "NO3_mg.l", "TOC_mg.l", "TSS_mg.l", "Temp_C")
  df[cols_to_convert] <- lapply(df[cols_to_convert], as.numeric)
  
  # Update the data frame in the list
  scan_list[[i]] <- df
}

### Keep rows with only 15-minute intervals ###
# # Loop through each data frame in the list
# for (i in seq_along(scan_list)) {
# # Access the current data frame
#   df <- scan_list[[i]]
#   
#   # Filter function 
#   df <- df %>%
#     filter(format(df$dateTime, "%M") %in% c("00", "15", "30", "45"))
#   # Update the data frame in the list
#   scan_list[[i]] <- df
# #}

# #### Clean out by specifics of each data set ####
# #Look at your data and decide if you need to do anything else with it
# scan_list[1]
# 
# # IN THIS CASE I am going to remove the first few rows of data to clean it more since they are junk
# 
# scan_list[[1]] <- scan_list[[1]][-c(1:93), ]
# 
# #Second data frame:
# scan_list[2]
# 
# scan_list[[2]] <- scan_list[[2]][-c(1:28), ]
# 
# #Third data frame:
# scan_list[3]
# 
# scan_list[[3]] <- scan_list[[3]][-c(1:9), ]

##################
#### Plotting ####
##################

### DOC ###
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = DOC_mg.l)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
}

### NO3 ###
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = NO3_mg.l)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
}

### NO3N ###
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = NO3.N_mg.l)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
  
}

### TOC ###
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = TOC_mg.l)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
  
}

### TSS ###
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = TSS_mg.l)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
  
}

### Temp ###
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = Temp_C)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
}


###########################
#### Plot all together ####
###########################
 
for (i in seq_along(scan_list)) {
  
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df) + 
    geom_line(aes(x=DateTime, y=Temp_C, color='Temp_C')) +
    geom_line(aes(x=DateTime, y=TSS_mg.l, color='TSS')) +
    geom_line(aes(x=DateTime, y=TOC_mg.l, color='TOC')) +
    geom_line(aes(x=DateTime, y=NO3.N_mg.l, color='NO3-N')) +
    geom_line(aes(x=DateTime, y=NO3_mg.l, color='NO3')) +
    geom_line(aes(x=DateTime, y=DOC_mg.l, color='DOC')) +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45)) +
    ylab("Measured")
  print(p)
}
