##==============================================================================
## Project: QuEST
## Script to merge parameters and fingerprint s::can data
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(googledrive)
library(dplyr)
library(openxlsx)

########################################
#### Clear folders that we will use ####
########################################
# List and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

##==============================================================================
## LMP27
##==============================================================================

#######################################
#### Import abs and parameter data ####
#######################################
# Load data from Google drive, this is the "raw" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1Txv_Q6wLuCzhD-7cWMDWueuv85uC8BIw")
# List all CSV files in the folder
scan_xlsx <- googledrive::drive_ls(path = scan)

# Load only LMP27 files
googledrive::drive_download(file = scan_xlsx$id[scan_csvs$name=="NHLMP27_SCN_24110220.xlsx"], 
                            path = "googledrive/NHLMP27_SCN_24110220.xlsx",
                            overwrite = T)

LMP27_params <- read_excel("googledrive/NHLMP27_SCN_24110220.xlsx", sheet = 1, skip = 1)
LMP27_abs <- read_excel("googledrive/NHLMP27_SCN_24110220.xlsx", sheet = 2, skip = 1)

#############################
#### Tidy both data sets ####
#############################

# Change datetime format
LMP27_params <- LMP27_params %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))
LMP27_abs <- LMP27_abs %>%
  # Rename the column 'Parameter:' to 'datetime'
  rename(datetime = `Parameter:`) %>%
  # Convert the datetime column to POSIXct format
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))

#################################
#### Merge parameter and abs ####
#################################

# Param data first
LMP27_merged <- left_join(LMP27_params, LMP27_abs, by = "datetime")

#########################################
#### Save merged LMP27 file to Drive ####
#########################################

# Make sure it is in datetime format
LMP27_merged$datetime <- format(LMP27_merged$datetime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(LMP27_merged,"data/01_LMP27_absparams.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1llXcmKVhauTAHcnTuXuhhatPtEaMoeW2"

# Upload the file to the specified Google Drive folder
drive_upload(media = "data/LMP27_absparams.csv", path = as_id(drive_folder_id))

##==============================================================================
## LMP72
##==============================================================================

#######################################
#### Import abs and parameter data ####
#######################################

# Load only LMP72 files
googledrive::drive_download(file = scan_xlsx$id[scan_csvs$name=="NHLMP72_SCN_24110221.xlsx"], 
                            path = "googledrive/NHLMP72_SCN_24110221.xlsx",
                            overwrite = T)

LMP72_params <- read_excel("googledrive/NHLMP72_SCN_24110221.xlsx", sheet = 1, skip = 1)
LMP72_abs <- read_excel("googledrive/NHLMP72_SCN_24110221.xlsx", sheet = 2, skip = 1)

#############################
#### Tidy both data sets ####
#############################

# Change datetime format
LMP72_params <- LMP72_params %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))
LMP72_abs <- LMP72_abs %>%
  # Rename the column 'Parameter:' to 'datetime'
  rename(datetime = dateTime) %>%
  # Convert the datetime column to POSIXct format
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))

#################################
#### Merge parameter and abs ####
#################################

# Param data first
LMP72_merged <- left_join(LMP72_params, LMP72_abs, by = "datetime")

#########################################
#### Save merged LMP72 file to Drive ####
#########################################

# Make sure it is in datetime format
LMP72_merged$datetime <- format(LMP72_merged$datetime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(LMP72_merged,"data/LMP72_absparams.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1llXcmKVhauTAHcnTuXuhhatPtEaMoeW2"

# Upload the file to the specified Google Drive folder
drive_upload(media = "data/LMP72_absparams.csv", path = as_id(drive_folder_id))

##==============================================================================
## NCB
##==============================================================================

#######################################
#### Import abs and parameter data ####
#######################################

# Load only NCB files
googledrive::drive_download(file = scan_xlsx$id[scan_csvs$name=="NHNCB_SCN_24110219.xlsx"], 
                            path = "googledrive/NHNCB_SCN_24110219.xlsx",
                            overwrite = T)

NCB_params <- read_excel("googledrive/NHNCB_SCN_24110219.xlsx", sheet = 1, skip = 1)
NCB_abs <- read_excel("googledrive/NHNCB_SCN_24110219.xlsx", sheet = 2, skip = 1)

#############################
#### Tidy both data sets ####
#############################

# Change datetime format
NCB_params <- NCB_params %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))
NCB_abs <- NCB_abs %>%
  # Rename the column 'Parameter:' to 'datetime'
  rename(datetime = `Parameter:`) %>%
  # Convert the datetime column to POSIXct format
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))

#################################
#### Merge parameter and abs ####
#################################

# Param data first
NCB_merged <- left_join(NCB_params, NCB_abs, by = "datetime")

#########################################
#### Save merged LMP27 file to Drive ####
#########################################

# Make sure it is in datetime format
NCB_merged$datetime <- format(NCB_merged$datetime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(NCB_merged,"data/NCB_absparams.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1llXcmKVhauTAHcnTuXuhhatPtEaMoeW2"

# Upload the file to the specified Google Drive folder
drive_upload(media = "data/NCB_absparams.csv", path = as_id(drive_folder_id))

##==============================================================================
## CTB
##==============================================================================

#######################################
#### Import abs and parameter data ####
#######################################

# Load only CTB files
googledrive::drive_download(file = scan_xlsx$id[scan_csvs$name=="NHCTB_SCN_24110222.xlsx"], 
                            path = "googledrive/NHCTB_SCN_24110222.xlsx",
                            overwrite = T)

CTB_params <- read_excel("googledrive/NHCTB_SCN_24110222.xlsx", sheet = 1, skip = 1)
CTB_abs <- read_excel("googledrive/NHCTB_SCN_24110222.xlsx", sheet = 2, skip = 1)

#############################
#### Tidy both data sets ####
#############################

# Change datetime format
CTB_params <- CTB_params %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))
CTB_abs <- CTB_abs %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))

#################################
#### Merge parameter and abs ####
#################################

# Param data first
CTB_merged <- left_join(CTB_params, CTB_abs, by = "datetime")

#########################################
#### Save merged LMP27 file to Drive ####
#########################################

# Make sure it is in datetime format
CTB_merged$datetime <- format(CTB_merged$datetime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(CTB_merged,"data/CTB_absparams.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1llXcmKVhauTAHcnTuXuhhatPtEaMoeW2"

# Upload the file to the specified Google Drive folder
drive_upload(media = "data/CTB_absparams.csv", path = as_id(drive_folder_id))

##==============================================================================
## LMP07
##==============================================================================

#######################################
#### Import abs and parameter data ####
#######################################
# Load data from Google drive, this is the "raw" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1Txv_Q6wLuCzhD-7cWMDWueuv85uC8BIw")
# List all CSV files in the folder
scan_xlsx <- googledrive::drive_ls(path = scan)

# Load only LMP07 files
googledrive::drive_download(file = scan_xlsx$id[scan_csvs$name=="NHLMP07_SCN_24110223.xlsx"], 
                            path = "googledrive/NHLMP07_SCN_24110223.xlsx",
                            overwrite = T)

LMP07_params <- read_excel("googledrive/NHLMP07_SCN_24110223.xlsx", sheet = 1, skip = 1)
LMP07_abs <- read_excel("googledrive/NHLMP07_SCN_24110223.xlsx", sheet = 2, skip = 1)

#############################
#### Tidy both data sets ####
#############################

# Change datetime format
LMP07_params <- LMP07_params %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))
LMP07_abs <- LMP07_abs %>%
  # Rename the column 'Parameter:' to 'datetime'
  rename(datetime = `Parameter:`) %>%
  # Convert the datetime column to POSIXct format
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))

#################################
#### Merge parameter and abs ####
#################################

# Param data first
LMP07_merged <- left_join(LMP07_params, LMP07_abs, by = "datetime")

#########################################
#### Save merged LMP07 file to Drive ####
#########################################

# Make sure it is in datetime format
LMP07_merged$datetime <- format(LMP07_merged$datetime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(LMP07_merged,"data/01_LMP07_absparams.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1llXcmKVhauTAHcnTuXuhhatPtEaMoeW2"

# Upload the file to the specified Google Drive folder
drive_upload(media = "data/LMP07_absparams.csv", path = as_id(drive_folder_id))



