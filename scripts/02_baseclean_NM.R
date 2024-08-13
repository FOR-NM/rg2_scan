##==============================================================================
## Project: QuEST
## Script to clean up scan data
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

#####################
#### Import Data ####
#####################
# Load data from Google drive
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1DZktlQUHaot_r4e_fD9ip6zcxHWqslMP")
# List all CSV files in the folder
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

#####################################################
#### Clean out service dates (out of water days) ####
#####################################################
#When scan is out of water it records as NO_MEDIUM, we'll use that to clean
# Loop through each data frame in the list
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  
  # Replace 'NO_MEDIUM' with NA in character columns only
  df <- df %>%
    mutate(across(where(is.character), ~ na_if(.x, "NO_MEDIUM")))
  
  # Update the data frame in the list
  scan_list[[i]] <- df
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
  
  # convert to POIXct and set timezone
  df$dateTime<-as.POSIXct(df$dateTime, 
                                 format = "%Y-%m-%d %H:%M:%S",
                                 tz="US/Mountain")
  
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

# Add instrument name to data frame of each
# Function to extract the ID from the file name
extract_id <- function(file_name) {
  str_extract(file_name, "B\\w+")
}

# Add the instrument name or serial number to data
for (i in seq_along(scan_list)) {
  df <- scan_list[[i]]
  file_name <- scan_csvs$name[i]
  extracted_id <- extract_id(file_name)
  df <- add_column(df, serial_number = extracted_id)
  scan_list[[i]] <- df
}

#############################################################
#### Delete all the rows before the deployment date-time ####
#############################################################

# Initialize an empty list to store filtered data frames
scan_filtered <- list()

# Loop through each data frame in the list
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  
  # Identify the "deployed" time for the specific instrument
  serial_number <- df$serial_number[1]
  deployed_time <- deployedtimes$datetimeMT[
    deployedtimes$serial_number == serial_number
  ]
  
  # Convert to POSIXct if not already in that format
  deployed_time <- as.POSIXct(deployed_time, tz = "America/Denver")
  df$dateTime <- as.POSIXct(df$dateTime, tz = "America/Denver")
  
  # Filter out data that occurs before the "deployed" time
  filtered_data <- df[df$dateTime >= deployed_time, ]
  
  # Store the filtered data in the new list
  scan_filtered[[scan_csvs$name[i]]] <- filtered_data
}

#### this is how it would work just for one file ####
# Identify the "deployed" time for the specific instrument
#deployed_time <- deployedtimes$datetimeMT[
 # deployedtimes$serial_number=="blossom"
#]
# Filter out data that occurs before the "deployed" time
#filtered_data <- df[
 # df$dateTime >= deployed_time,
#]

#######################################
#### Remove below and above values ####
#######################################
# There are some values above and below range, let's remove them?
# Loop through each data frame in the list
for (i in seq_along(scan_filtered)) {
  # Access the current data frame
  df <- scan_filtered[[i]]
  
  # Replace 'VAL_ABOVE' and 'VAL_BELOW' with NA in character columns only
  df <- df %>%
    mutate(across(where(is.character), ~ na_if(.x, "VAL_ABOVE") %>%
                    na_if("VAL_BELOW")))
  
  # Update the data frame in the list
  scan_filtered[[i]] <- df
}

#####################################
#### Plot all variables together ####
#####################################

for (i in seq_along(scan_filtered)) {
  
  # Access the current data frame
  df <- scan_filtered[[i]]
  # Plot
  p <- ggplot(data = df) + 
    geom_line(aes(x=dateTime, y=Temp, color='Temperature')) +
    geom_line(aes(x=dateTime, y=TSS, color='TSS')) +
    geom_line(aes(x=dateTime, y=TOC, color='TOC')) +
    geom_line(aes(x=dateTime, y=NO3N, color='NO3-N')) +
    geom_line(aes(x=dateTime, y=NO3, color='NO3')) +
    geom_line(aes(x=dateTime, y=DOC, color='DOC')) +
    scale_x_datetime(date_breaks = "7 days", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45)) +
    ylab("Measured")
  ggsave(paste0("scan_figs/", scan_csvs$name[i], "_Measured.png"))
}

#################################
#### Pull USGS discharge data ####
##################################

### Close-by gauge USGS ID ###
# AR: 7049000
# AL: 2465493?
# NH: 1073319
# NM: 8315480
# NV: 10347310

# Define gauge and parameter code
siteNo <- "08315480"
pCode <- "00060" #this code is for discharge data

#### For first one = USF20 #### 
#check first date entry
head(scan_filtered[[1]][["dateTime"]]) # check start date for Blossom (USF20)
start.date <- "2024-05-08"
#check last date entry
tail(scan_filtered[[1]][["dateTime"]]) # check end date for Blossom (USF20)
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

#### Plot USGS with USF20 s::can data ####
### For only one df ###
# Convert data frames to xts objects to line up dateTimes
scan_ts <- xts(scan_filtered[[1]], order.by = scan_filtered[[1]]$dateTime)
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
  scale_x_datetime(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_y_continuous(breaks = seq(0, 20, by = 5)) +
  theme(axis.text.x = element_text(angle=45)) +
  ylab("Blossom")

print(p)

### Plot TSS with flow ###
# Since that is the one that has weird peaks
p <- ggplot(data = combined_df) + 
  geom_line(aes(x=dateTime, y=TSS, color='TSS')) +
  geom_line(aes(x=dateTime, y=Flow_Inst, color='Flow')) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_y_continuous(breaks = seq(0, 20, by = 5)) +
  theme(axis.text.x = element_text(angle=45)) +
  ggtitle("USF20") +
  ylab("Blossom")

print(p)

### Plot TOC with flow ###
# Since that is the one that has weird peaks
p <- ggplot(data = combined_df) + 
  geom_line(aes(x=dateTime, y=TOC, color='TOC')) +
  geom_line(aes(x=dateTime, y=Flow_Inst, color='Flow')) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_y_continuous(breaks = seq(0, 20, by = 5)) +
  theme(axis.text.x = element_text(angle=45)) +
  ggtitle("USF20") +
  ylab("Blossom")

print(p)

### Plot TOC with flow ###
# Since that is the one that has weird peaks
p <- ggplot(data = combined_df) + 
  geom_line(aes(x=dateTime, y=DOC, color='DOC')) +
  geom_line(aes(x=dateTime, y=Flow_Inst, color='Flow')) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_y_continuous(breaks = seq(0, 20, by = 5)) +
  theme(axis.text.x = element_text(angle=45)) +
  ggtitle("USF20") +
  ylab("Blossom")

print(p)

#### For second one = USF12 #### 
#check first date entry
head(scan_filtered[[2]][["dateTime"]]) # check start date for Blossom (USF20)
start.date <- "2024-05-07"
#check last date entry
tail(scan_filtered[[2]][["dateTime"]]) # check end date for Blossom (USF20)
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

#### Plot USGS with USF12 s::can data ####
### For only one df ###
# Convert data frames to xts objects to line up dateTimes
scan_ts <- xts(scan_filtered[[2]], order.by = scan_filtered[[2]]$dateTime)
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
  scale_x_datetime(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_y_continuous(breaks = seq(0, 20, by = 5)) +
  theme(axis.text.x = element_text(angle=45)) +
  ylab("Buttercup")

print(p)
### Plot DOC with flow ###
# Since that is the one that has weird peaks
p <- ggplot(data = combined_df) + 
  geom_line(aes(x=dateTime, y=DOC, color='DOC')) +
  geom_line(aes(x=dateTime, y=Flow_Inst, color='Flow')) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_y_continuous(breaks = seq(0, 20, by = 5)) +
  theme(axis.text.x = element_text(angle=45)) +
  ggtitle("USF12") +
  ylab("Buttercup")

print(p)

### Plot TSS with flow ###
# Since that is the one that has weird peaks
p <- ggplot(data = combined_df) + 
  geom_line(aes(x=dateTime, y=TSS, color='TSS')) +
  geom_line(aes(x=dateTime, y=Flow_Inst, color='Flow')) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_y_continuous(breaks = seq(0, 20, by = 5)) +
  theme(axis.text.x = element_text(angle=45)) +
  ggtitle("USF12") +
  ylab("Buttercup")

print(p)

### Plot TOC with flow ###
# Since that is the one that has weird peaks
p <- ggplot(data = combined_df) + 
  geom_line(aes(x=dateTime, y=TOC, color='TOC')) +
  geom_line(aes(x=dateTime, y=Flow_Inst, color='Flow')) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_y_continuous(breaks = seq(0, 20, by = 5)) +
  theme(axis.text.x = element_text(angle=45)) +
  ggtitle("USF12") +
  ylab("Buttercup")

print(p)

#### For third one = bubbles #### 
#check first date entry
head(scan_filtered[[3]][["dateTime"]]) # check start date for Blossom (USF20)
start.date <- "2024-06-27"
#check last date entry
tail(scan_filtered[[3]][["dateTime"]]) # check end date for Blossom (USF20)
end.date <- "2024-07-26"

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
scan_ts <- xts(scan_filtered[[3]], order.by = scan_filtered[[3]]$dateTime)
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
  scale_x_datetime(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_y_continuous(breaks = seq(0, 20, by = 5)) +
  theme(axis.text.x = element_text(angle=45)) +
  ylab("Bubbles")

print(p)

### Plot TSS with flow ###
# Since that is the one that has weird peaks
p <- ggplot(data = combined_df) + 
  geom_line(aes(x=dateTime, y=TSS, color='TSS')) +
  geom_line(aes(x=dateTime, y=Flow_Inst, color='Flow')) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_y_continuous(breaks = seq(0, 20, by = 5)) +
  theme(axis.text.x = element_text(angle=45)) +
  ggtitle("USF21") +
  ylab("Bubbles")

print(p)

### Plot TOC with flow ###
# Since that is the one that has weird peaks
p <- ggplot(data = combined_df) + 
  geom_line(aes(x=dateTime, y=TOC, color='TOC')) +
  geom_line(aes(x=dateTime, y=Flow_Inst, color='Flow')) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_y_continuous(breaks = seq(0, 20, by = 5)) +
  theme(axis.text.x = element_text(angle=45)) +
  ggtitle("USF21") +
  ylab("Bubbles")

print(p)

### Plot TOC with flow ###
# Since that is the one that has weird peaks
p <- ggplot(data = combined_df) + 
  geom_line(aes(x=dateTime, y=DOC, color='DOC')) +
  geom_line(aes(x=dateTime, y=Flow_Inst, color='Flow')) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_y_continuous(breaks = seq(0, 20, by = 5)) +
  theme(axis.text.x = element_text(angle=45)) +
  ggtitle("USF21") +
  ylab("Bubbles")

print(p)

#######################
#### Flag outliers ####
#######################

#### Blossom ####
# Define the number of standard deviations to use as the threshold
num_sd <- 4

# Function to flag outliers in a numeric vector
flag_outliers <- function(x, num_sd) {
  mean_value <- mean(x, na.rm = TRUE)
  sd_value <- sd(x, na.rm = TRUE)
  ifelse(abs(x - mean_value) > num_sd * sd_value, "Outlier", "Normal")
}

# Apply the function to all numeric columns in each data frame in the list
for (i in seq_along(scan_filtered)) {
  # Access the current data frame
  df <- scan_filtered[[i]]
  
  # Flag outliers
  df <- df %>%
    mutate(across(where(is.numeric), ~flag_outliers(., num_sd), .names = "flag_{col}"))
  
  # Store the updated data frame in the list
  scan_filtered[[i]] <- df
}

#Retrieve and Plot USGS Data:

# Check start and end date for Blossom (USF20)
start.date <- "2024-05-08"
end.date <- "2024-07-26"

# Retrieve data from NWIS
siteNo <- "08315480"
pCode <- "00060" # Parameter code for streamflow

santafeUSGS <- readNWISuv(siteNumbers = siteNo,
                          parameterCd = pCode,
                          startDate = start.date,
                          endDate = end.date)

# Change column names
santafeUSGS <- renameNWISColumns(santafeUSGS)

# Plot USGS data
ts <- ggplot(data = santafeUSGS, aes(dateTime, Flow_Inst)) +
  geom_line()
print(ts)

#Merge and Filter Data:

# Convert data frames to xts objects to line up dateTimes
scan_ts <- xts(scan_filtered[[1]], order.by = scan_filtered[[1]]$dateTime)
santafeUSGS_ts <- xts(santafeUSGS, order.by = santafeUSGS$dateTime)

# Merge the xts objects
combined_xts <- merge(scan_ts, santafeUSGS_ts, join = "outer")

# Convert xts object to data.frame
combined_df <- data.frame(dateTime = index(combined_xts), coredata(combined_xts))

# Verify dateTime is in POSIXct
combined_df$dateTime <- as.POSIXct(combined_df$dateTime)

# Filter the data to exclude outliers
filtered_data <- combined_df %>%
  filter(if_all(starts_with("flag_"), ~ . == "Normal"))

# Convert y-values to numeric
numeric_columns <- c("Temp", "TSS", "TOC", "NO3N", "NO3", "DOC", "Flow_Inst")
filtered_data[numeric_columns] <- lapply(filtered_data[numeric_columns], as.numeric)

# Plot the filtered data
p <- ggplot(data = filtered_data) + 
  geom_line(aes(x = dateTime, y = Temp, color = 'Temperature')) +
  geom_line(aes(x = dateTime, y = TSS, color = 'TSS')) +
  geom_line(aes(x = dateTime, y = TOC, color = 'TOC')) +
  geom_line(aes(x = dateTime, y = NO3N, color = 'NO3-N')) +
  geom_line(aes(x = dateTime, y = NO3, color = 'NO3')) +
  geom_line(aes(x = dateTime, y = DOC, color = 'DOC')) +
  geom_line(aes(x = dateTime, y = Flow_Inst, color = 'Flow')) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_y_continuous() +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Blossom") +
  ggtitle("Filtered blossom X4sd")

print(p)

#### Buttercup ####
# Define the number of standard deviations to use as the threshold
num_sd <- 4

# Function to flag outliers in a numeric vector
flag_outliers <- function(x, num_sd) {
  mean_value <- mean(x, na.rm = TRUE)
  sd_value <- sd(x, na.rm = TRUE)
  ifelse(abs(x - mean_value) > num_sd * sd_value, "Outlier", "Normal")
}

# Apply the function to all numeric columns in each data frame in the list
for (i in seq_along(scan_filtered)) {
  # Access the current data frame
  df <- scan_filtered[[i]]
  
  # Flag outliers
  df <- df %>%
    mutate(across(where(is.numeric), ~flag_outliers(., num_sd), .names = "flag_{col}"))
  
  # Store the updated data frame in the list
  scan_filtered[[i]] <- df
}

#Retrieve and Plot USGS Data:

# Check start and end date for Blossom (USF20)
start.date <- "2024-05-07"
end.date <- "2024-07-26"

# Retrieve data from NWIS
siteNo <- "08315480"
pCode <- "00060" # Parameter code for streamflow

santafeUSGS <- readNWISuv(siteNumbers = siteNo,
                          parameterCd = pCode,
                          startDate = start.date,
                          endDate = end.date)

# Change column names
santafeUSGS <- renameNWISColumns(santafeUSGS)

# Plot USGS data
ts <- ggplot(data = santafeUSGS, aes(dateTime, Flow_Inst)) +
  geom_line()
print(ts)

#Merge and Filter Data:

# Convert data frames to xts objects to line up dateTimes
scan_ts <- xts(scan_filtered[[2]], order.by = scan_filtered[[2]]$dateTime)
santafeUSGS_ts <- xts(santafeUSGS, order.by = santafeUSGS$dateTime)

# Merge the xts objects
combined_xts <- merge(scan_ts, santafeUSGS_ts, join = "outer")

# Convert xts object to data.frame
combined_df <- data.frame(dateTime = index(combined_xts), coredata(combined_xts))

# Verify dateTime is in POSIXct
combined_df$dateTime <- as.POSIXct(combined_df$dateTime)

# Filter the data to exclude outliers
filtered_data <- combined_df %>%
  filter(if_all(starts_with("flag_"), ~ . == "Normal"))

# Convert y-values to numeric
numeric_columns <- c("Temp", "TSS", "TOC", "NO3N", "NO3", "DOC", "Flow_Inst")
filtered_data[numeric_columns] <- lapply(filtered_data[numeric_columns], as.numeric)

# Plot the filtered data
p <- ggplot(data = filtered_data) + 
  geom_line(aes(x = dateTime, y = Temp, color = 'Temperature')) +
  geom_line(aes(x = dateTime, y = TSS, color = 'TSS')) +
  geom_line(aes(x = dateTime, y = TOC, color = 'TOC')) +
  geom_line(aes(x = dateTime, y = NO3N, color = 'NO3-N')) +
  geom_line(aes(x = dateTime, y = NO3, color = 'NO3')) +
  geom_line(aes(x = dateTime, y = DOC, color = 'DOC')) +
  geom_line(aes(x = dateTime, y = Flow_Inst, color = 'Flow')) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_y_continuous() +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Buttercup") +
  ggtitle("Filtered buttercup X4sd")

print(p)

#### Bubbles ####
# Define the number of standard deviations to use as the threshold
num_sd <- 2

# Function to flag outliers in a numeric vector
flag_outliers <- function(x, num_sd) {
  mean_value <- mean(x, na.rm = TRUE)
  sd_value <- sd(x, na.rm = TRUE)
  ifelse(abs(x - mean_value) > num_sd * sd_value, "Outlier", "Normal")
}

# Apply the function to all numeric columns in each data frame in the list
for (i in seq_along(scan_filtered)) {
  # Access the current data frame
  df <- scan_filtered[[i]]
  
  # Flag outliers
  df <- df %>%
    mutate(across(where(is.numeric), ~flag_outliers(., num_sd), .names = "flag_{col}"))
  
  # Store the updated data frame in the list
  scan_filtered[[i]] <- df
}

#Retrieve and Plot USGS Data:

# Check start and end date for Bubbles (USF21)
start.date <- "2024-06-27"
end.date <- "2024-07-26"

# Retrieve data from NWIS
siteNo <- "08315480"
pCode <- "00060" # Parameter code for streamflow

santafeUSGS <- readNWISuv(siteNumbers = siteNo,
                          parameterCd = pCode,
                          startDate = start.date,
                          endDate = end.date)

# Change column names
santafeUSGS <- renameNWISColumns(santafeUSGS)

# Plot USGS data
ts <- ggplot(data = santafeUSGS, aes(dateTime, Flow_Inst)) +
  geom_line()
print(ts)

#Merge and Filter Data:

# Convert data frames to xts objects to line up dateTimes
scan_ts <- xts(scan_filtered[[3]], order.by = scan_filtered[[3]]$dateTime)
santafeUSGS_ts <- xts(santafeUSGS, order.by = santafeUSGS$dateTime)

# Merge the xts objects
combined_xts <- merge(scan_ts, santafeUSGS_ts, join = "outer")

# Convert xts object to data.frame
combined_df <- data.frame(dateTime = index(combined_xts), coredata(combined_xts))

# Verify dateTime is in POSIXct
combined_df$dateTime <- as.POSIXct(combined_df$dateTime)

# Filter the data to exclude outliers
filtered_data <- combined_df %>%
  filter(if_all(starts_with("flag_"), ~ . == "Normal"))

# Convert y-values to numeric
numeric_columns <- c("Temp", "TSS", "TOC", "NO3N", "NO3", "DOC", "Flow_Inst")
filtered_data[numeric_columns] <- lapply(filtered_data[numeric_columns], as.numeric)

#Plot the Filtered Data:

# Plot the filtered data
p <- ggplot(data = filtered_data) + 
  geom_line(aes(x = dateTime, y = Temp, color = 'Temperature')) +
  geom_line(aes(x = dateTime, y = TSS, color = 'TSS')) +
  geom_line(aes(x = dateTime, y = TOC, color = 'TOC')) +
  geom_line(aes(x = dateTime, y = NO3N, color = 'NO3-N')) +
  geom_line(aes(x = dateTime, y = NO3, color = 'NO3')) +
  geom_line(aes(x = dateTime, y = DOC, color = 'DOC')) +
  geom_line(aes(x = dateTime, y = Flow_Inst, color = 'Flow')) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_y_continuous() +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Bubbles") +
  ggtitle("Filtered bubbles X2sd")

print(p)

####################################
#### Save cleaned data to Drive ####
####################################

# Function to remove file extension
remove_extension <- function(file_name) {
  sub("\\.[[:alnum:]]+$", "", file_name)
}

# Loop through each data frame in the list
for (i in seq_along(scan_filtered)) {
  # Access the current data frame
  df <- scan_filtered[[i]]
  
  # Define the file name and path
  clean_name <- remove_extension(scan_csvs$name[i])
  file_name <- paste0("googledrive/", clean_name, ".csv")
  
  # Save the new data frame to a CSV file
  write.csv(df, file_name, row.names=FALSE, quote=FALSE)
  
  # Define the target folder ID in Google Drive
  drive_folder_id <- "1DZktlQUHaot_r4e_fD9ip6zcxHWqslMP"
  
  # Upload the file to the specified Google Drive folder
  drive_upload(media = file_name, path = as_id(drive_folder_id))
}

