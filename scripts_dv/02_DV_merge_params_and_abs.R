##==============================================================================
## Project: QuEST
## Script to merge parameters and fingerprint s::can data
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(googledrive)
library(dplyr)
library(openxlsx)

##==============================================================================
## DVO
##==============================================================================

#######################################
#### Import abs and parameter data ####
#######################################
# Load data from Google drive, this is the "combined" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1BdR9BtqqmByHOzXtREtmsQoVJbdEntWk")
# List all CSV files in the folder
scan_csvs <- googledrive::drive_ls(path = scan)

# Load only DVO files
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="01_DVO_params.csv"], 
                            path = "googledrive/01_DVO_params.csv",
                            overwrite = T)
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="01_DVO_abs.csv"], 
                            path = "googledrive/01_DVO_abs.csv",
                            overwrite = T)

DVO_params <- read.csv("googledrive/01_DVO_params.csv")
DVO_abs <- read.csv("googledrive/01_DVO_abs.csv")

#############################
#### Tidy both data sets ####
#############################

# Change datetime format
DVO_params <- DVO_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))
DVO_abs <- DVO_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))

#################################
#### Merge parameter and abs ####
#################################

# Param data first
DVO_merged <- left_join(DVO_params, DVO_abs, by = "DateTime")

#########################################
#### Save merged DVO file to Drive ####
#########################################

# Make sure it is in datetime format
DVO_merged$DateTime <- format(DVO_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(DVO_merged,"data/02_DVO_absparams.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1gAfaUZKCoarEaSrPnhdUXy5lQ49Z_xm1"

# Upload the file to the specified Google Drive folder
drive_upload(media = "data/02_DVO_absparams.csv", path = as_id(drive_folder_id))

##==============================================================================
## DVMS1
##==============================================================================

#######################################
#### Import abs and parameter data ####
#######################################

# Load only SSM01 files
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="01_DVMS1_params.csv"], 
                            path = "googledrive/01_DVMS1_params.csv",
                            overwrite = T)
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="01_DVMS1_abs.csv"], 
                            path = "googledrive/01_DVMS1_abs.csv",
                            overwrite = T)

DVMS1_params <- read.csv("googledrive/01_DVMS1_params.csv")
DVMS1_abs <- read.csv("googledrive/01_DVMS1_abs.csv")

#############################
#### Tidy both data sets ####
#############################

# Change datetime format
DVMS1_params <- DVMS1_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))
DVMS1_abs <- DVMS1_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))

#################################
#### Merge parameter and abs ####
#################################

# Param data first
DVMS1_merged <- left_join(DVMS1_params, DVMS1_abs, by = "DateTime")

#########################################
#### Save merged DVMS1 file to Drive ####
#########################################

# Make sure it is in datetime format
DVMS1_merged$DateTime <- format(DVMS1_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(DVMS1_merged,"data/02_DVMS1_absparams.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1gAfaUZKCoarEaSrPnhdUXy5lQ49Z_xm1"

# Upload the file to the specified Google Drive folder
drive_upload(media = "data/02_DVMS1_absparams.csv", path = as_id(drive_folder_id))

##==============================================================================
## DVNWT5
##==============================================================================

#######################################
#### Import abs and parameter data ####
#######################################

# Load only DVNWT5 files
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="01_DVNWT5_params.csv"], 
                            path = "googledrive/01_DVNWT5_params.csv",
                            overwrite = T)
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="01_DVNWT5_abs.csv"], 
                            path = "googledrive/01_DVNWT5_abs.csv",
                            overwrite = T)

DVNWT5_params <- read.csv("googledrive/01_DVNWT5_params.csv")
DVNWT5_abs <- read.csv("googledrive/01_DVNWT5_abs.csv")

#############################
#### Tidy both data sets ####
#############################

# Change datetime format
DVNWT5_params <- DVNWT5_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))
DVNWT5_abs <- DVNWT5_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))

#################################
#### Merge parameter and abs ####
#################################

# Param data first
DVNWT5_merged <- left_join(DVNWT5_params, DVNWT5_abs, by = "DateTime")

#########################################
#### Save merged SSM01 file to Drive ####
#########################################

# Make sure it is in datetime format
DVNWT5_merged$DateTime <- format(DVNWT5_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(DVNWT5_merged,"data/02_DVNWT5_absparams.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1gAfaUZKCoarEaSrPnhdUXy5lQ49Z_xm1"

# Upload the file to the specified Google Drive folder
drive_upload(media = "data/02_DVNWT5_absparams.csv", path = as_id(drive_folder_id))

