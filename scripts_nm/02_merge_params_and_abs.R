##==============================================================================
## Project: QuEST
## Script to merge parameters and fingerprint s::can data
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(googledrive)
library(dplyr)
library(openxlsx)

##==============================================================================
## USF12
##==============================================================================

#######################################
#### Import abs and parameter data ####
#######################################
# This is the "most_recent" folder
#### Import abs and parameter data ####
# This is the "merged" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1np2B4bSWaNMIYE2FHL3YOnZ20FRudsEy")

# List all the files in the folder
scan_csvs <- googledrive::drive_ls(path = scan, type = "csv")

#USF12 parameters
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="USF12_params.csv"], 
                            path = "googledrive/USF12_params.csv",
                            overwrite = T)
#USF12 abs
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="USF12_abs.csv"], 
                            path = "googledrive/USF12_abs.csv",
                            overwrite = T)


# Load fingerprints and parameter data
USF12_params <- read.csv("googledrive/USF12_params.csv")
USF12_abs <- read.csv("googledrive/USF12_abs.csv")

#############################
#### Tidy both data sets ####
#############################

# Remove extra rows
USF12_params <- USF12_params[-c(1:9), ] 
#USF12_abs <- USF12_abs[-c(1:9), ] 

# Change datetime format
USF12_params <- USF12_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Mountain"))
USF12_abs <- USF12_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Mountain"))

#################################
#### Merge parameter and abs ####
#################################

# Param data first
USF12_merged <- left_join(USF12_params, USF12_abs, by = "DateTime")

#########################################
#### Save merged USF12 file to Drive ####
#########################################

# Make sure it is in datetime format
USF12_merged$DateTime <- format(USF12_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(USF12_merged,"googledrive/USF12_absparams_Buttercup.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1g6aSuGnb--Qeyk-rceX82Y5wSNzCqFg0"

# Upload the file to the specified Google Drive folder
drive_upload(media = "googledrive/USF12_absparams_Buttercup.csv", path = as_id(drive_folder_id))

##==============================================================================
## USF20
##==============================================================================

#######################################
#### Import abs and parameter data ####
#######################################
#USF20 parameters
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="USF20_params.csv"], 
                            path = "googledrive/USF20_params.csv",
                            overwrite = T)
#USF20 abs
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="USF20_abs.csv"], 
                            path = "googledrive/USF20_abs.csv",
                            overwrite = T)

# Load fingerprints and parameter data
USF20_params <- read.csv("googledrive/USF20_params.csv")
USF20_abs <- read.csv("googledrive/USF20_abs.csv")

#############################
#### Tidy both data sets ####
#############################

# Remove extra rows
USF20_params <- USF20_params[-c(1:85), ] 
USF20_abs <- USF20_abs[-c(1:85), ] 

# Change datetime format
USF20_params <- USF20_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Mountain"))
USF20_abs <- USF20_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Mountain"))

#################################
#### Merge parameter and abs ####
#################################

# Param data first
USF20_merged <- left_join(USF20_params, USF20_abs, by = "DateTime")

################################
#### Save merged USF20 file ####
################################

# Make sure it is in datetime format
USF20_merged$DateTime <- format(USF20_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(USF20_merged,"googledrive/USF20_absparams_Blossom.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1g6aSuGnb--Qeyk-rceX82Y5wSNzCqFg0"

# Upload the file to the specified Google Drive folder
drive_upload(media = "googledrive/USF20_absparams_Blossom.csv", path = as_id(drive_folder_id))

##==============================================================================
## USF21
##==============================================================================

#######################################
#### Import abs and parameter data ####
#######################################
#USF21 parameters
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="USF21_params.csv"], 
                            path = "googledrive/USF21_params.csv",
                            overwrite = T)
#USF21 abs
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="USF21_abs.csv"], 
                            path = "googledrive/USF21_abs.csv",
                            overwrite = T)

# Load fingerprints and parameter data
USF21_params <- read.csv("googledrive/USF21_params.csv")
USF21_abs <- read.csv("googledrive/USF21_abs.csv")

#############################
#### Tidy both data sets ####
#############################

# Change DateTime names for easier manipulation

# Remove extra rows
USF21_params <- USF21_params[-c(1:28), ] 
USF21_abs <- USF21_abs[-c(1:28), ] 

# Change datetime format
USF21_params <- USF21_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Mountain"))
USF21_abs <- USF21_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Mountain"))

#################################
#### Merge parameter and abs ####
#################################

# Param data first
USF21_merged <- left_join(USF21_params, USF21_abs, by = "DateTime")

################################
#### Save merged USF21 file ####
################################

# Make sure it is in datetime format
USF21_merged$DateTime <- format(USF21_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(USF21_merged,"googledrive/USF21_absparams_Bubbles.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1g6aSuGnb--Qeyk-rceX82Y5wSNzCqFg0"

# Upload the file to the specified Google Drive folder
drive_upload(media = "googledrive/USF21_absparams_Bubbles.csv", path = as_id(drive_folder_id))

