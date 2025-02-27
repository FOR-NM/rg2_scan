##==============================================================================
## Project: QuEST
## Script to visualize raw scan data from NM
## Here we will plot some images but will not be saving cleaned data back. 
## It is just so see what your data looks like and understand what needs to get done
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(dataRetrieval) # Download USGS discharge data
library(googledrive) #Download docs from Drive
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(tidyr)
library(readxl) #to read excel 
library(lubridate) # Edit date format
library(xts) # Time series

#################################
#### Import & Visualize Data ####
#################################
# Load data from Google drive. This is the "in use" folder
scan <- googledrive::as_id("https://drive.google.com/drive/u/1/folders/1np2B4bSWaNMIYE2FHL3YOnZ20FRudsEy")
# List all CSV files in the folder
scan_csvs <- googledrive::drive_ls(path = scan)

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
### Rename columns and change to values to numeric ###
# Loop through each data frame in the list
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  
  # Change names for easier handling
  colnames(df)[1] ="dateTime"
  colnames(df)[2] ="DOC"
  colnames(df)[6] ="NO3N"
  colnames(df)[8] ="NO3"
  colnames(df)[12] ="TOC"
  colnames(df)[14] ="TSS"
  colnames(df)[16] ="Temp"
  colnames(df)[11] ="Voltage"
  
  # Make sure values are numeric 
  df$DOC <- as.numeric(df$DOC)
  df$NO3N <- as.numeric(df$NO3N)
  df$NO3 <- as.numeric(df$NO3)
  df$TOC <- as.numeric(df$TOC)
  df$TSS <- as.numeric(df$TSS)
  df$Temp <- as.numeric(df$Temp)
  
  # Update the data frame in the list
  scan_list[[i]] <- df
}

### Keep rows with only 15-minute intervals ###
# Loop through each data frame in the list
#for (i in seq_along(scan_list)) {
# Access the current data frame
  #df <- scan_list[[i]]
  
  # Filter function 
  #df <- df %>%
    #filter(format(df$dateTime, "%M") %in% c("00", "15", "30", "45"))
  # Update the data frame in the list
  #scan_list[[i]] <- df
#}

#### Clean out by specifics of each data set ####
#Look at your data and decide if you need to do anything else with it
scan_list[1]

# IN THIS CASE I am going to remove the first few rows of data to clean it more since they are junk

scan_list[[1]] <- scan_list[[1]][-c(1:93), ]

#Second data frame:
scan_list[2]

scan_list[[2]] <- scan_list[[2]][-c(1:28), ]

#Third data frame:
scan_list[3]

scan_list[[3]] <- scan_list[[3]][-c(1:9), ]

##################
#### Plotting ####
##################

### DOC ###
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = dateTime, y = DOC)) + 
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
  p <- ggplot(data = df, aes(x = dateTime, y = NO3)) + 
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
  p <- ggplot(data = df, aes(x = dateTime, y = NO3N)) + 
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
  p <- ggplot(data = df, aes(x = dateTime, y = TOC)) + 
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
  p <- ggplot(data = df, aes(x = dateTime, y = TSS)) + 
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
  p <- ggplot(data = df, aes(x = dateTime, y = Temp)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
}

### Voltage ###
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = dateTime, y = Voltage)) + 
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
    geom_line(aes(x=dateTime, y=Temp, color='Temperature')) +
    geom_line(aes(x=dateTime, y=TSS, color='TSS')) +
    geom_line(aes(x=dateTime, y=TOC, color='TOC')) +
    geom_line(aes(x=dateTime, y=NO3N, color='NO3-N')) +
    geom_line(aes(x=dateTime, y=NO3, color='NO3')) +
    geom_line(aes(x=dateTime, y=DOC, color='DOC')) +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45)) +
    ylab("Measured")
  print(p)
}
 

