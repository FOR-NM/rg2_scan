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
files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

##==============================================================================
## LMP27
##==============================================================================
#######################################
#### Import abs and parameter data ####
#######################################
# Load data from Google drive, this is the "merged" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1llXcmKVhauTAHcnTuXuhhatPtEaMoeW2")
# List all CSV files in the folder
scan_csvs <- googledrive::drive_ls(path = scan)

# Load only LMP27 files
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="NHLMP27_params.csv"], 
                            path = "googledrive/NHLMP27_params.csv",
                            overwrite = T)
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="NHLMP27_abs.csv"], 
                            path = "googledrive/NHLMP27_abs.csv",
                            overwrite = T)

LMP27_params <- read.csv("googledrive/NHLMP27_params.csv")
LMP27_abs <- ("googledrive/NHLMP27_abs.csv")

#############################
#### Tidy both data sets ####
#############################
# Change DateTime format
LMP27_params <- LMP27_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
LMP27_abs <- LMP27_abs %>%
  # Convert the DateTime column to POSIXct format
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

#################################
#### Merge parameter and abs ####
#################################
# Param data first
LMP27_merged <- left_join(LMP27_params, LMP27_abs, by = "DateTime")

# Clean a couple of columns
LMP27_merged <- LMP27_merged[-c(1:7), -c(1, 21)]

#########################################
#### Save merged LMP27 file to Drive ####
#########################################
# Make sure it is in DateTime format
LMP27_merged$DateTime <- format(LMP27_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(LMP27_merged,"data/LMP27_absparams.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "abs and params" folder
drive_folder_id <- "1CeCmX0mGh1wZ3IL4Exu4oPHUuzYOk4T_"

# Upload the file to the specified Google Drive folder
drive_upload(media = "data/LMP27_absparams.csv", path = as_id(drive_folder_id))

##==============================================================================
## LMP72
##==============================================================================
#######################################
#### Import abs and parameter data ####
#######################################
# Load only LMP72 files
googledrive::drive_download(file = scan_xlsx$id[scan_csvs$name=="NHLMP72_params.csv"], 
                            path = "googledrive/NHLMP72_params.csv",
                            overwrite = T)
googledrive::drive_download(file = scan_xlsx$id[scan_csvs$name=="NHLMP72_abs.csv"], 
                            path = "googledrive/NHLMP72_abs.csv",
                            overwrite = T)

LMP72_params <- read.csv("googledrive/NHLMP72_params.csv")
LMP72_abs <- read.csv("googledrive/NHLMP72_abs.csv")

#############################
#### Tidy both data sets ####
#############################
# Change DateTime format
LMP72_params <- LMP72_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
LMP72_abs <- LMP72_abs %>%
  # Convert the DateTime column to POSIXct format
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

#################################
#### Merge parameter and abs ####
#################################
# Param data first
LMP72_merged <- left_join(LMP72_params, LMP72_abs, by = "DateTime")

# Clean a couple of columns
LMP72_merged <- LMP72_merged[-c(1:1053), -c(1, 25)]

#########################################
#### Save merged LMP72 file to Drive ####
#########################################
# Make sure it is in DateTime format
LMP72_merged$DateTime <- as.POSIXct(LMP72_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(LMP72_merged,"data/LMP72_absparams.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1CeCmX0mGh1wZ3IL4Exu4oPHUuzYOk4T_"

# Upload the file to the specified Google Drive folder
drive_upload(media = "data/LMP72_absparams.csv", path = as_id(drive_folder_id))

##==============================================================================
## NCB
##==============================================================================
#######################################
#### Import abs and parameter data ####
#######################################
# Load only NCB files
googledrive::drive_download(file = scan_xlsx$id[scan_csvs$name=="NHNCBd_params.csv"], 
                            path = "googledrive/NHNCBd_params.csv",
                            overwrite = T)
googledrive::drive_download(file = scan_xlsx$id[scan_csvs$name=="NHNCBd_abs.csv"], 
                            path = "googledrive/NHNCBd_abs.csv",
                            overwrite = T)

NCB_params <- read.csv("googledrive/NHNCBd_params.csv")
NCB_abs <- read.csv("googledrive/NHNCBd_abs.csv")

#############################
#### Tidy both data sets ####
#############################
# Change DateTime format
NCB_params <- NCB_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
NCB_abs <- NCB_abs %>%
  # Convert the DateTime column to POSIXct format
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

#################################
#### Merge parameter and abs ####
#################################
# Param data first
NCB_merged <- left_join(NCB_params, NCB_abs, by = "DateTime")

#########################################
#### Save merged LMP27 file to Drive ####
#########################################
# Make sure it is in DateTime format
NCB_merged$DateTime <- as.POSIXct(NCB_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(NCB_merged,"data/NCB_absparams.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1CeCmX0mGh1wZ3IL4Exu4oPHUuzYOk4T_"

# Upload the file to the specified Google Drive folder
drive_upload(media = "data/NCB_absparams.csv", path = as_id(drive_folder_id))

##==============================================================================
## CTB
##==============================================================================
#######################################
#### Import abs and parameter data ####
#######################################
# Load only CTB files
googledrive::drive_download(file = scan_xlsx$id[scan_csvs$name=="NHCTB_params.csv"], 
                            path = "googledrive/NHCTB_params.csv",
                            overwrite = T)
googledrive::drive_download(file = scan_xlsx$id[scan_csvs$name=="NHCTB_abs.csv"], 
                            path = "googledrive/NHCTB_abs.csv",
                            overwrite = T)

CTB_params <- read.csv("googledrive/NHCTB_params.csv")
CTB_abs <- read.csv("googledrive/NHCTB_abs.csv")

#############################
#### Tidy both data sets ####
#############################
# Change DateTime format
CTB_params <- CTB_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
CTB_abs <- CTB_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

#################################
#### Merge parameter and abs ####
#################################
# Param data first
CTB_merged <- left_join(CTB_params, CTB_abs, by = "DateTime")

CTB_merged <- CTB_merged[-c(1:232), -c(1, 22)]

#########################################
#### Save merged LMP27 file to Drive ####
#########################################
# Make sure it is in DateTime format
CTB_merged$DateTime <- format(CTB_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(CTB_merged,"data/CTB_absparams.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1CeCmX0mGh1wZ3IL4Exu4oPHUuzYOk4T_"

# Upload the file to the specified Google Drive folder
drive_upload(media = "data/CTB_absparams.csv", path = as_id(drive_folder_id))

##==============================================================================
## LMP07
##==============================================================================
#######################################
#### Import abs and parameter data ####
#######################################
# Load only LMP07 files
googledrive::drive_download(file = scan_xlsx$id[scan_csvs$name=="NHLMP07_params.csv"], 
                            path = "googledrive/NHLMP07_params.csv",
                            overwrite = T)
googledrive::drive_download(file = scan_xlsx$id[scan_csvs$name=="NHLMP07_abs.csv"], 
                            path = "googledrive/NHLMP07_abs.csv",
                            overwrite = T)

LMP07_params <- read.csv("googledrive/NHLMP07_params.csv")
LMP07_abs <- read.csv("googledrive/NHLMP07_abs.csv")

#############################
#### Tidy both data sets ####
#############################
# Change DateTime format
LMP07_params <- LMP07_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
LMP07_abs <- LMP07_abs %>%
  # Convert the DateTime column to POSIXct format
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

#################################
#### Merge parameter and abs ####
#################################
# Param data first
LMP07_merged <- left_join(LMP07_params, LMP07_abs, by = "DateTime")

LMP07_merged <- LMP07_merged[-c(1:25), -c(1, 22)]

#########################################
#### Save merged LMP07 file to Drive ####
#########################################
# Make sure it is in DateTime format
LMP07_merged$DateTime <- format(LMP07_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(LMP07_merged,"data/LMP07_absparams.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1CeCmX0mGh1wZ3IL4Exu4oPHUuzYOk4T_"

# Upload the file to the specified Google Drive folder
drive_upload(media = "data/LMP07_absparams.csv", path = as_id(drive_folder_id))

##==============================================================================
## SBM
##==============================================================================
#######################################
#### Import abs and parameter data ####
#######################################
# Load only SBM files
googledrive::drive_download(file = scan_xlsx$id[scan_csvs$name=="NHSBM_params.csv"], 
                            path = "googledrive/NHSBM_params.csv",
                            overwrite = T)
googledrive::drive_download(file = scan_xlsx$id[scan_csvs$name=="NHSBM_abs.csv"], 
                            path = "googledrive/NHSBM_abs.csv",
                            overwrite = T)

SBM_params <- read.csv("googledrive/NHSBM_params.csv")
SBM_abs <- read.csv("googledrive/NHSBM_abs.csv")

#############################
#### Tidy both data sets ####
#############################
# Change DateTime format
SBM_params <- SBM_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
SBM_abs <- SBM_abs %>%
  # Convert the DateTime column to POSIXct format
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

#################################
#### Merge parameter and abs ####
#################################
# Param data first
SBM_merged <- left_join(SBM_params, SBM_abs, by = "DateTime")

SBM_merged <- SBM_merged[-c(1:3231), -c(1, 22)]

#########################################
#### Save merged SBM file to Drive ####
#########################################
# Make sure it is in DateTime format
SBM_merged$DateTime <- format(SBM_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(SBM_merged,"data/SBM_absparams.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1CeCmX0mGh1wZ3IL4Exu4oPHUuzYOk4T_"

# Upload the file to the specified Google Drive folder
drive_upload(media = "data/SBM_absparams.csv", path = as_id(drive_folder_id))



