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
  ggsave(paste0("scan_figs/", scan_csvs$name[i], "_DOC.png"))
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
  ggsave(paste0("scan_figs/", scan_csvs$name[i], "_NO3.png"))
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
  ggsave(paste0("scan_figs/", scan_csvs$name[i], "_NO3N.png"))
  
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
  ggsave(paste0("scan_figs/", scan_csvs$name[i], "_TOC.png"))
  
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
  ggsave(paste0("scan_figs/", scan_csvs$name[i], "_TSS.png"))
  
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
  ggsave(paste0("scan_figs/", scan_csvs$name[i], "_Temp.png"))
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
  ggsave(paste0("scan_figs/", scan_csvs$name[i], "_V.png"))
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
  ggsave(paste0("scan_figs/", scan_csvs$name[i], "_Measured.png"))
}

print(p)

#################################
#### Pull USGS discharge data ####
##################################

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
head(scan_list[[1]][["dateTime"]]) # check start date for Blossom (USF20)
start.date <- "2024-05-08"
#check last date entry
tail(scan_list[[1]][["dateTime"]]) # check end date for Blossom (USF20)
end.date <- "2024-07-30"

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
# Convert data frames to xts objects to line up dateTimes
scan_ts <- xts(scan_list[[1]], order.by = scan_list[[1]]$dateTime)
santafeUSGS_ts <- xts(santafeUSGS, order.by = santafeUSGS$dateTime)

# Merge the xts objects
combined_xts <- merge(scan_ts, santafeUSGS_ts, join = "outer")
# Convert xts object to data.frame... do I really have to do this?
combined_df <- data.frame(dateTime = index(combined_xts), coredata(combined_xts))

# Verify dateTime is in POSIXct
class(combined_df$dateTime)

# Convert y-values to numeric
combined_df$Temp <- as.numeric(as.character(combined_df$Temp))
combined_df$TSS <- as.numeric(as.character(combined_df$TSS))
combined_df$TOC <- as.numeric(as.character(combined_df$TOC))
combined_df$NO3N <- as.numeric(as.character(combined_df$NO3N))
combined_df$NO3 <- as.numeric(as.character(combined_df$NO3))
combined_df$DOC <- as.numeric(as.character(combined_df$DOC))
combined_df$Flow_Inst <- as.numeric(as.character(combined_df$Flow_Inst))

### Plot ###

p <- ggplot(data = combined_df) + 
  geom_line(aes(x=dateTime, y=Temp, color='Temperature')) +
  geom_line(aes(x=dateTime, y=TSS, color='TSS')) +
  geom_line(aes(x=dateTime, y=TOC, color='TOC')) +
  geom_line(aes(x=dateTime, y=NO3N, color='NO3-N')) +
  geom_line(aes(x=dateTime, y=NO3, color='NO3')) +
  geom_line(aes(x=dateTime, y=DOC, color='DOC')) +
  geom_line(aes(x=dateTime, y=Flow_Inst, color='Flow')) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
  scale_y_continuous(breaks = seq(0, 20, by = 5)) +
  theme(axis.text.x = element_text(angle=45)) +
  ylab("Measured")
  
print(p)

 #### Now merge all your scan data with USGS data ####
# Create empty list to store data frames
scan_ts <- list()
# Convert data frames to xts objects to line up dateTimes
for (i in seq_along(scan_list)) {
  # Access the current data frame (df)
  df <- scan_list[[i]]
  # Convert df into time series (ts)
  ts <- xts(df, order.by = df$dateTime)
  
  scan_ts[[scan_csvs$name[i]]] <- ts

}
# Convert USGS data to xts objects to line up dateTimes with scan data
santafeUSGS_ts <- xts(santafeUSGS, order.by = santafeUSGS$dateTime)

# Merge the xts objects
for (i in seq_along(scan_ts)) {
  # Access the time series list
  ts <- scan_ts[[i]]
  # Merge
  xts <- merge(ts, santafeUSGS_ts, join = "outer")
  
  scan_ts[[scan_csvs$name[i]]] <- xts
  
}

# Convert xts object to data.frame... do I really have to do this?
# Create empty list to store data frames
scan_USGS <- list()
for (i in seq_along(scan_ts)) {
  # Access the time series list
  xts <- scan_ts[[i]]
  # Go back to data frames
  combined_df <- data.frame(dateTime = index(xts), coredata(xts))
  
  scan_USGS[[scan_csvs$name[i]]] <- combined_df
  
}

### Plot ###
for (i in seq_along(scan_USGS)) {
  # Access list
  df <- scan_USGS[[i]]
  
  # Convert y-values to numeric
  df$Temp <- as.numeric(as.character(df$Temp))
  df$TSS <- as.numeric(as.character(df$TSS))
  df$TOC <- as.numeric(as.character(df$TOC))
  df$NO3N <- as.numeric(as.character(df$NO3N))
  df$NO3 <- as.numeric(as.character(df$NO3))
  df$DOC <- as.numeric(as.character(df$DOC))
  df$Flow_Inst <- as.numeric(as.character(df$Flow_Inst))
  
  # Plot
  p <- ggplot(data = df) + 
    geom_line(aes(x=dateTime, y=Temp, color='Temperature')) +
    geom_line(aes(x=dateTime, y=TSS, color='TSS')) +
    geom_line(aes(x=dateTime, y=TOC, color='TOC')) +
    geom_line(aes(x=dateTime, y=NO3N, color='NO3-N')) +
    geom_line(aes(x=dateTime, y=NO3, color='NO3')) +
    geom_line(aes(x=dateTime, y=DOC, color='DOC')) +
    geom_line(aes(x=dateTime, y=Flow_Inst, color='Flow')) +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    scale_y_continuous(breaks = seq(0, 20, by = 5)) +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45)) +
    ylab("Measured")
  #save plots
  ggsave(paste0("scan_figs/", scan_csvs$name[i], "scan_USGS.png"))

}

print(p)

##############################
#### Save Images to Drive ####
##############################

# Define the local folder path and the target folder ID in Google Drive
local_folder <- "scan_figs/"
drive_folder_id <- "1Unk7b1SVFBg-8Z7JM_yN7IhuLmBmFEK5"

# List all files in the local folder
files <- list.files(local_folder, full.names = TRUE)

# Upload each file to the specified Google Drive folder
lapply(files, function(file) {
  drive_upload(
    media = file,
    path = as_id(drive_folder_id)
  )
})

########################
#### Date specifics ####
########################
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