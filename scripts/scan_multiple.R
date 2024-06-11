---
title: "S::CAN"
---

library(dataRetrieval) # Download USGS discharge data
library(googledrive) #Download docs from Drive
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(tidyr)
library(readxl)
library(lubridate) # Edit date format
library(xts) # Time series

#############################
## Import & Visualize Data ##
#############################
# Load data from Google drive
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1DZktlQUHaot_r4e_fD9ip6zcxHWqslMP")
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


##############
## Cleaning ## 
##############
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
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  
  # Filter function 
  df <- df %>%
    filter(format(df$dateTime, "%M") %in% c("00", "15", "30", "45"))
  # Update the data frame in the list
  scan_list[[i]] <- df
}

#### Clean out by specifics of each data set ####
#Look at your data and decide if you need to do anything else with it, in this case I am going to remove the first few rows of data 
scan_list[1]

N <- 5
scan_list[1] <- scan_list[1][[-(1:N), , drop = FALSE]]

##############
## Plotting ##
##############

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
  ggsave(paste0("scan_figs/DOC_", scan_csvs$name[i], ".png"))
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
  ggsave(paste0("scan_figs/NO3_", scan_csvs$name[i], ".png"))
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
  ggsave(paste0("scan_figs/NO3N_", scan_csvs$name[i], ".png"))
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
  ggsave(paste0("scan_figs/TOC_", scan_csvs$name[i], ".png"))
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
  ggsave(paste0("scan_figs/TSS_", scan_csvs$name[i], ".png"))
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
  ggsave(paste0("scan_figs/Temp_", scan_csvs$name[i], ".png"))
}

#######################
## Plot all together ##
#######################

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
    theme(axis.text.x = element_text(angle=45)) +
    ylab("Measured")
  ggsave(paste0("scan_figs/Temp_", scan_csvs$name[i], ".png"))
}

print(p)

##############################
## Pull USGS discharge data ##
##############################

### Close-by gauge USGS ID ###
# AR: 7049000
# AL: 2465493?
# NH: 1073319
# NM: 8315480
# NV: 10347310

# Download functions
siteNo <- "08315480"
pCode <- "00060" #this code is for discharge data
#check first date entry
head(scan_list[[1]][["dateTime"]])
start.date <- "2024-05-08"
#check last date entry
tail(scan_list[[1]][["dateTime"]])
end.date <- "2024-05-23"

# Retrieve data
santafeUSGS <- readNWISuv(siteNumbers = siteNo,
                          parameterCd = pCode,
                          startDate = start.date,
                          endDate = end.date)

# Change column names
santafeUSGS <- renameNWISColumns(santafeUSGS)

### Plot it ###
ts <- ggplot(data = santafeUSGS,
             aes(dateTime, Flow_Inst)) +
  geom_line()
ts

#### Plot with s::can data ####
### For only one df ###
# Convert data frames to xts objects
scan_ts <- xts(scan_list[[1]], order.by = scan_list[[1]]$dateTime)
santafeUSGS_ts <- xts(santafeUSGS, order.by = santafeUSGS$dateTime)

# Merge the xts objects
combined_xts <- merge(scan_ts, santafeUSGS_ts, join = "outer")

p <- ggplot(data = combined_xts) + 
  geom_line(aes(x=dateTime, y=Temp, color='Temperature')) +
  geom_line(aes(x=dateTime, y=DOC, color='DOC')) +
  geom_line(aes(x=dateTime, y=Flow_Inst, color='Flow')) +
  theme(axis.text.x = element_text(angle=45))
  
print(p)

# Merge the data frames by the 'dateTime' column
# merge time series 
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  merged_ts <- ts(c(scan_list[i], santafeUSGS),                
                start = start(scan_list[i]), 
                frequency = frequency(scan_list[i])) 
  # Update the data frame in the list
  scan_list[[i]] <- merged_ts
}


# Plot
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df) + 
    geom_line(aes(x=dateTime, y=df$Temp, color='Temperature')) +
    geom_line(aes(x=dateTime, y=df$TSS, color='TSS')) +
    geom_line(aes(x=dateTime, y=df$TOC, color='TOC')) +
    geom_line(aes(x=dateTime, y=df$NO3N, color='NO3-N')) +
    geom_line(aes(x=dateTime, y=df$NO3, color='NO3')) +
    geom_line(aes(x=dateTime, y=df$DOC, color='DOC')) +
    geom_line(aes(x=dateTime, y=Flow_Inst, color='Flow'))
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    theme(axis.text.x = element_text(angle=45)) +
    ylab("Measured")
    ggsave(paste0("scan_figs/Measured_", scan_csvs$name[i], ".png"))

}

####################
## Date specifics ##
####################
### If dates needs to be more specific ####
#start date
#this will get data from 2023 and 2024 starting April 1st
start.date1 = c(2023:2024)
start.date <- vector(mode="character", length=length(start.date1))
for (i in 1:length(start.date1)){
  start.date[i] = paste(start.date1[i], "04","01", sep="-")
}
start.date
start.date = as.Date(start.date)
#do I need it as a date?

#end date
#this will get data from 2023 and 2024 ending October 31st
end.date1 = c(2023:2024)
end.date <- vector("character", length(end.date1))
for (i in seq_along(end.date1)){
  end.date[i] = paste(end.date1[i], "10","31", sep="-")
}
end.date
end.date = as.Date(end.date)

#retrieve data for all dates on all 8 gauges
siteNumber <- c("07049000", "02465493", "01073319", "08315480", "010347310")
#this code retrieves the discharge data
pCode <- "00060"

# Initialize an empty data frame to hold the results
discharge <- data.frame()

# Loop through each date range and retrieve the corresponding data
for (i in seq_along(start.date)) {
  # Retrieve data for the current date range
  temp <- readNWISdv(siteNumbers = siteNumber,
                     parameterCd = pCode,
                     startDate = start.date[i],
                     endDate = end.date[i])
  
  # Append the data to the results data frame
  discharge <- rbind(discharge, temp)
}

# Change column names
discharge <- renameNWISColumns(discharge)

### Plot it ###
# just one site
ts <- ggplot(data = discharge[discharge$site_no == "08315480",],
             aes(Date, Flow)) +
  geom_line()
ts