##==============================================================================
## Project: QuEST
## Script to merge parameters and fingerprint s::can data
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(googledrive)
library(dplyr)
library(openxlsx)

##==============================================================================
## SSM01
##==============================================================================
#######################################
#### Import abs and parameter data ####
#######################################
# load data from Google drive, this is the "merged" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1qpsqrmcnALNS9OVtoIDICdEuW5LkVuIR")
# list all CSV files in the folder
scan_csvs <- googledrive::drive_ls(path = scan)

# load only SSM01 files
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="01_SSM01_params.csv"], 
                            path = "googledrive/01_SSM01_params.csv",
                            overwrite = T)
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="01_SSM01_abs.csv"], 
                            path = "googledrive/01_SSM01_abs.csv",
                            overwrite = T)

SSM01_params <- read.csv("googledrive/01_SSM01_params.csv")
SSM01_abs <- read.csv("googledrive/01_SSM01_abs.csv")

#############################
#### Tidy both data sets ####
#############################
# change datetime format
SSM01_params <- SSM01_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))
SSM01_abs <- SSM01_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))

#################################
#### Merge parameter and abs ####
#################################
# param data first
SSM01_merged <- left_join(SSM01_params, SSM01_abs, by = "DateTime")

#########################################
#### Save merged SSM01 file to Drive ####
#########################################
# make sure it is in datetime format
SSM01_merged$DateTime <- format(SSM01_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(SSM01_merged,"data/02_SSM01_absparams.csv" , row.names=FALSE, quote=FALSE)

# define the target folder ID in Google Drive
# this is the "merged" folder
drive_folder_id <- "1qpsqrmcnALNS9OVtoIDICdEuW5LkVuIR"

# upload the file to the specified Google Drive folder
drive_upload(media = "data/02_SSM01_absparams.csv", path = as_id(drive_folder_id))

##==============================================================================
## SSM20
##==============================================================================
#######################################
#### Import abs and parameter data ####
#######################################
# load only SSM01 files
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="01_SSM20_params.csv"], 
                            path = "googledrive/01_SSM20_params.csv",
                            overwrite = T)
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="01_SSM20_abs.csv"], 
                            path = "googledrive/01_SSM20_abs.csv",
                            overwrite = T)

SSM20_params <- read.csv("googledrive/01_SSM20_params.csv")
SSM20_abs <- read.csv("googledrive/01_SSM20_abs.csv")

#############################
#### Tidy both data sets ####
#############################
# change datetime format
SSM20_params <- SSM20_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))
SSM20_abs <- SSM20_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))

#################################
#### Merge parameter and abs ####
#################################
# param data first
SSM20_merged <- left_join(SSM20_params, SSM20_abs, by = "DateTime")

#########################################
#### Save merged SSM20 file to Drive ####
#########################################
# make sure it is in datetime format
SSM20_merged$DateTime <- format(SSM20_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(SSM20_merged,"data/02_SSM20_absparams.csv" , row.names=FALSE, quote=FALSE)

# define the target folder ID in Google Drive
# this is the "merged" folder
drive_folder_id <- "1qpsqrmcnALNS9OVtoIDICdEuW5LkVuIR"

# upload the file to the specified Google Drive folder
drive_upload(media = "data/02_SSM20_absparams.csv", path = as_id(drive_folder_id))

##==============================================================================
## SST13
##==============================================================================
#######################################
#### Import abs and parameter data ####
#######################################
# load only SST13 files
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="SST13_params.csv"], 
                            path = "googledrive/01_SST13_params.csv",
                            overwrite = T)
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="SST13_abs.csv"], 
                            path = "googledrive/01_SST13_abs.csv",
                            overwrite = T)

SST13_params <- read.csv("googledrive/01_SST13_params.csv")
SST13_abs <- read.csv("googledrive/01_SST13_abs.csv")

#############################
#### Tidy both data sets ####
#############################
# change datetime format
SST13_params <- SST13_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))
SST13_abs <- SST13_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central"))

#################################
#### Merge parameter and abs ####
#################################
# param data first
SST13_merged <- left_join(SST13_params, SST13_abs, by = "DateTime")

#########################################
#### Save merged SSM01 file to Drive ####
#########################################
# make sure it is in datetime format
SST13_merged$DateTime <- format(SST13_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(SST13_merged,"data/02_SST13_absparams.csv" , row.names=FALSE, quote=FALSE)

# define the target folder ID in Google Drive
# this is the "merged" folder
drive_folder_id <- "1qpsqrmcnALNS9OVtoIDICdEuW5LkVuIR"

# upload the file to the specified Google Drive folder
drive_upload(media = "data/02_SST13_absparams.csv", path = as_id(drive_folder_id))

