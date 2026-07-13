##==============================================================================
## Project: FOR-NM
## Adapted from QuEST
## Original Author: Manuela Londono
## Modified by : Marcela Mendoza 
## Script to visualize raw scan data from NM
## Here we will plot some images but will not be saving cleaned data back. 
## It is just so see what your data looks like and understand what needs to get done
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(tidyr)
library(readxl) #to read excel 
library(lubridate) # Edit date format
library(xts) # time series

#################################
#### Import & Visualize Data ####
#################################
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
### rename columns and change to values to numeric ###
# loop through each data frame in the list
for (i in seq_along(scan_list)) {
  # access the current data frame
  df <- scan_list[[i]]
  print(colnames(df))
  
  # change names for easier handling
  
  df <- df %>% rename(
    "dateTime" = "DateTime",
    "DOC" = "DOCeq..mg.l....Measured.value", 
    "NO3N"= "NO3.Neq..mg.l....Measured.value", 
    "NO3" = "NO3eq..mg.l....Measured.value", 
    "TOC" = "TOCeq..mg.l....Measured.value", 
    "TSS" = "TSSeq..mg.l....Measured.value",
    "Temp"= "Temperature_21...C....Measured.value", 
    "Voltage"= "Supply.Voltage..V....Measured.value"
  )
  # make sure values are numeric 
  df$DOC <- as.numeric(df$DOC)
  df$NO3N <- as.numeric(df$NO3N)
  df$NO3 <- as.numeric(df$NO3)
  df$TOC <- as.numeric(df$TOC)
  df$TSS <- as.numeric(df$TSS)
  df$Temp <- as.numeric(df$Temp)
  
  # update the data frame in the list
  scan_list[[i]] <- df
}

### keep rows with only 15-minute intervals ###
# loop through each data frame in the list
#for (i in seq_along(scan_list)) {
# access the current data frame
  #df <- scan_list[[i]]
  
  # filter function 
  #df <- df %>%
    #filter(format(df$dateTime, "%M") %in% c("00", "15", "30", "45"))
  # update the data frame in the list
  #scan_list[[i]] <- df
#}

#### clean out by specifics of each data set ####
# look at your data and decide if you need to do anything else with it
scan_list[1]


##################
#### Plotting ####
##################
### DOC ###
for (i in seq_along(scan_list)) {
  # access the current data frame
  df <- scan_list[[i]]
  # plot
  p <- ggplot(data = df, aes(x = dateTime, y = DOC)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
}

### NO3 ###
for (i in seq_along(scan_list)) {
  # access the current data frame
  df <- scan_list[[i]]
  # plot
  p <- ggplot(data = df, aes(x = dateTime, y = NO3)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
}

### NO3N ###
for (i in seq_along(scan_list)) {
  # access the current data frame
  df <- scan_list[[i]]
  # plot
  p <- ggplot(data = df, aes(x = dateTime, y = NO3N)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
  
}

### TOC ###
for (i in seq_along(scan_list)) {
  # access the current data frame
  df <- scan_list[[i]]
  # plot
  p <- ggplot(data = df, aes(x = dateTime, y = TOC)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
  
}

### TSS ###
for (i in seq_along(scan_list)) {
  # access the current data frame
  df <- scan_list[[i]]
  # plot
  p <- ggplot(data = df, aes(x = dateTime, y = TSS)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
  
}

### Temp ###
for (i in seq_along(scan_list)) {
  # access the current data frame
  df <- scan_list[[i]]
  # plot
  p <- ggplot(data = df, aes(x = dateTime, y = Temp)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
}

### Voltage ###
for (i in seq_along(scan_list)) {
  # access the current data frame
  df <- scan_list[[i]]
  # plot
  p <- ggplot(data = df, aes(x = dateTime, y = Voltage)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
}

###########################
#### Plot all together ####
###########################
for (i in seq_along(scan_list)) {
  
  # access the current data frame
  df <- scan_list[[i]]
  # plot
  p <- ggplot(data = df) + 
    geom_line(aes(x=dateTime, y=Temp, color='Temperature')) +
    geom_line(aes(x=dateTime, y=TSS, color='TSS')) +
    geom_line(aes(x=dateTime, y=TOC, color='TOC')) +
    geom_line(aes(x=dateTime, y=NO3N, color='NO3-N')) +
    geom_line(aes(x=dateTime, y=NO3, color='NO3')) +
    geom_line(aes(x=dateTime, y=DOC, color='DOC')) +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs[i])) +
    theme(axis.text.x = element_text(angle=45)) +
    ylab("Measured")
  print(p)
  ggsave(paste0("scan_figs/", scan_csvs[i], ".png"), p)
}
 
# To do : re- run aggregation of abs and params for USF41 and run this script 
