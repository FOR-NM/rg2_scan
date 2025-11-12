##==============================================================================
## Project: QuEST
## This script will help you if you have to use the moving window approach to do the calibration
## Then you can flag spectra if they are too low or too high 
##==============================================================================

library(dplyr)
library(spectrolab)
library(googledrive) 
library(tidyverse)

##############################################
#### Upload scan dataframe [with spectra] ####
##############################################
# This data is already matched #
# This is the "with grab" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1Wju54VbyACZ_RFtfeInSvBCiVDKFScGj")

# List all xlsx files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")
3

#SSM01
googledrive::drive_download(file = merged$id[merged$name=="SSM01_merged.csv"], 
                            path = "googledrive/SSM01_merged.csv",
                            overwrite = T)
#SSM20
googledrive::drive_download(file = merged$id[merged$name=="SSM20_merged.csv"], 
                            path = "googledrive/SSM20_merged.csv",
                            overwrite = T)
#SST13
googledrive::drive_download(file = merged$id[merged$name=="SST13_merged.csv"], 
                            path = "googledrive/SST13_merged.csv",
                            overwrite = T)


# Load them separately
SSM01 <- read.csv("googledrive/SSM01_merged.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
SSM20 <- read.csv("googledrive/SSM20_merged.csv", na = c("", "NaN", "Na", "NA"))
SST13 <- read.csv("googledrive/SST13_merged.csv", na = c("", "NaN", "Na", "NA"))

# # DateTime at midnight is missing 00:00:00 time, so filling in that time using grep                     
# SSM01$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SSM01$DateTime)] <- paste(
#   SSM01$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SSM01$DateTime)],"00:00:00")
# SSM20$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SSM20$DateTime)] <- paste(
#   SSM20$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SSM20$DateTime)],"00:00:00")
# SST13$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SST13$DateTime)] <- paste(
#   SST13$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SST13$DateTime)],"00:00:00")

# Convert the Datetime column to POSIXct
SSM01$DateTime <- as.POSIXct(SSM01$DateTime, format = "%Y-%m-%d %H:%M:%S")
SSM20$DateTime <- as.POSIXct(SSM20$DateTime, format = "%Y-%m-%d %H:%M:%S")
SST13$DateTime <- as.POSIXct(SST13$DateTime, format = "%Y-%m-%d %H:%M:%S")

# Rename columns for all data frames
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}

# Apply the renaming to each data frame
SSM01 <- rename_columns(SSM01)
SSM20 <- rename_columns(SSM20)
SST13 <- rename_columns(SST13)

#####################################
#### Look at data month by month ####
#####################################
# Remove some NA DateTimes
SSM01 <- SSM01 %>% drop_na(DateTime)
SSM20 <- SSM20 %>% drop_na(DateTime)
SST13 <- SST13 %>% drop_na(DateTime)

# cleaning out some columns
SSM01_test <- SSM01[-c(2,25)]
SSM01 <- SSM01_test %>% 
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",  # Assign "Y" if data exists
    TRUE ~ NA_character_  # Leave as NA otherwise
  ))
SSM20_test <- SSM20[-c(2,26)]
SSM20 <- SSM20_test %>% 
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",
    TRUE ~ NA_character_
  ))
SST13_test <- SST13[-c(2,4)]
SST13 <- SST13_test %>% 
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",
    TRUE ~ NA_character_
  ))

# Separate by month
SSM01_month <- SSM01 %>%
  filter(format(DateTime, "%B") == "August") %>%
  filter(format(DateTime, "%Y") == "2024")
SSM20_month <- SSM20 %>%
  filter(format(DateTime, "%B") == "August") %>%
  filter(format(DateTime, "%Y") == "2024")
SST13_month <- SST13 %>%
  filter(format(DateTime, "%B") == "August")  %>%
  filter(format(DateTime, "%Y") == "2024")

################################################################################
#### Create matrices of ALL spectral data - raw data that needs calibration ####
################################################################################
# 1. Index FULL dataset with columns with absorbances
scan.spec01 = SSM01_month[25:245] # change for SSM01_month to see by month 
scan.spec20 = SSM20_month[26:246]
scan.spec13 = SST13_month[40:260]

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
abs01 = (scan.spec01)
abs20 = (scan.spec20) 
abs13 = (scan.spec13) 

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wl01 = as.numeric(colnames(abs01))
wl20 = as.numeric(colnames(abs20))
wl13 = as.numeric(colnames(abs13))

# 4. Create a vector with sample labels that match the absorbance matrix rows. 
lastrow01 = as.numeric(nrow(abs01))
Num01 = c(1:lastrow01)

lastrow20 = as.numeric(nrow(abs20))
Num20 = c(1:lastrow20)

lastrow13 = as.numeric(nrow(abs13))
Num13 = c(1:lastrow13)

# 5. Create the final matrix 
#SSM01
scan.matrix01 = cbind(abs01)
rownames(scan.matrix01) = as.numeric(Num01)
colnames(scan.matrix01) = as.numeric(wl01)

scan.matrix01 = as.matrix(scan.matrix01)
spec01 = spectra(value = abs01, bands = wl01, names = Num01)
png("scan_figs/SSM01_abs_2408.png", width = 800, height = 600, res = 150)
plot(spec01) # Note = reflectance here = absorbance from the scans
dev.off()

#SSM20
scan.matrix20 = cbind(abs20)
rownames(scan.matrix20) = as.numeric(Num20)
colnames(scan.matrix20) = as.numeric(wl20)

scan.matrix20 = as.matrix(scan.matrix20)
spec20 = spectra(value = abs20, bands = wl20, names = Num20)
png("scan_figs/SSM20_abs_2408.png", width = 800, height = 600, res = 150)
plot(spec20) # Note = reflectance here = absorbance from the scans
dev.off()

#SST13
scan.matrix13 = cbind(abs13)
rownames(scan.matrix13) = as.numeric(Num13)
colnames(scan.matrix13) = as.numeric(wl13)

scan.matrix13 = as.matrix(scan.matrix13)
spec13 = spectra(value = abs13, bands = wl13, names = Num13)
png("scan_figs/SSMT13_abs_2408.png", width = 800, height = 600, res = 150)
plot(spec13) # Note = reflectance here = absorbance from the scans
dev.off()

##############################
#### Flagging absorbances ####
##############################
### SSM01 ###
# First check column numbers, look for spectral columns
data.frame(colnames(SSM01))

SSM01_test <- SSM01 %>%
  mutate(across(24:244,
                list(clean = ~ if_else(. < -10 | . > 200, NA_real_, as.numeric(.))),
                .names = "{.col}_{.fn}"))

SSM01_test <- SSM01_test %>%
  mutate(across(24:244,
                list(flag  = ~ if_else(. < -10 | . > 200, TRUE, FALSE, missing = FALSE)),
                .names = "{.col}_{.fn}")) 

### SSM20 ###
# First check column numbers, look for spectral columns
data.frame(colnames(SSM20))

SSM20_test <- SSM20 %>%
  mutate(across(25:245,
                list(clean = ~ if_else(. < -10 | . > 200, NA_real_, as.numeric(.))),
                .names = "{.col}_{.fn}"))

SSM20_test <- SSM20_test %>%
  mutate(across(25:245,
                list(flag  = ~ if_else(. < -10 | . > 200, TRUE, FALSE, missing = FALSE)),
                .names = "{.col}_{.fn}"))

### SST13 ###
# First check column numbers, look for spectral columns
data.frame(colnames(SST13))
# Check which columns have non-numeric entries
sapply(SST13[39:259], function(x) sum(is.na(as.numeric(as.character(x)))))
# Convert abs to numeric and non-numeric entries to NA.
SST13 <- SST13 %>%
  mutate(across(39:259, ~ suppressWarnings(as.numeric(as.character(.)))))

# first create the clean abs
SST13_test <- SST13 %>%
  mutate(across(39:259,
                list(clean = ~ if_else(. < -10 | . > 200, NA_real_, as.numeric(.))),
                .names = "{.col}_{.fn}"))

# then create the flag for the cleaned abs
SST13_test <- SST13_test %>%
  mutate(across(39:259,
                list(flag  = ~ if_else(. < -10 | . > 200, TRUE, FALSE, missing = FALSE)),
                .names = "{.col}_{.fn}"))

################################
#### Plot clean absorbances ####
################################
data.frame(colnames(SSM01_test))
data.frame(colnames(SSM20_test))
data.frame(colnames(SST13_test))

scan.spec01 = SSM01_test[278:498]
scan.spec20 = SSM20_test[285:499]
scan.spec13 = SST13_test[293:513]

# Rows = wavelength
# Columns = date/time
abs01 = (scan.spec01)
abs20 = (scan.spec20) 
abs13 = (scan.spec13) 

wl01 <- gsub("_clean", "", colnames(abs01))   
wl01 <- as.numeric(wl01)
wl20 <- gsub("_clean", "", colnames(abs20))   
wl20 <- as.numeric(wl20)
wl13 <- gsub("_clean", "", colnames(abs13))   
wl13 <- as.numeric(wl13)

lastrow01 = as.numeric(nrow(abs01))
Num01 = c(1:lastrow01)
lastrow20 = as.numeric(nrow(abs20))
Num20 = c(1:lastrow20)
lastrow13 = as.numeric(nrow(abs13))
Num13 = c(1:lastrow13)

#SSM01
scan.matrix01 = cbind(abs01)
rownames(scan.matrix01) = as.numeric(Num01)
colnames(scan.matrix01) = as.numeric(wl01)

scan.matrix01 = as.matrix(scan.matrix01)
spec01 = spectra(value = abs01, bands = wl01, names = Num01)
plot(spec01) # Note = reflectance here = absorbance from the scans

#SSM20
scan.matrix20 = cbind(abs20)
rownames(scan.matrix20) = as.numeric(Num20)
colnames(scan.matrix20) = as.numeric(wl20)

scan.matrix20 = as.matrix(scan.matrix20)
spec20 = spectra(value = abs20, bands = wl20, names = Num20)
plot(spec20) # Note = reflectance here = absorbance from the scans

#SST13
scan.matrix13 = cbind(abs13)
rownames(scan.matrix13) = as.numeric(Num13)
colnames(scan.matrix13) = as.numeric(wl13)

scan.matrix13 = as.matrix(scan.matrix13)
spec13 = spectra(value = abs13, bands = wl13, names = Num13)
plot(spec13) # Note = reflectance here = absorbance from the scans

############################
#### Save flagged files ####
############################
# Make sure it is in datetime format
SSM01_test$DateTime <- format(SSM01_test$DateTime, "%Y-%m-%d %H:%M:%S")
SSM20_test$DateTime <- format(SSM20_test$DateTime, "%Y-%m-%d %H:%M:%S")
SST13_test$DateTime <- format(SST13_test$DateTime, "%Y-%m-%d %H:%M:%S")

# Save the new data frame to a CSV file
write.csv(SSM01_test,"googledrive/SSM01_flagged_Buttercup.csv" , row.names=FALSE, quote=FALSE)
write.csv(SSM20_test,"googledrive/SSM20_flagged_Blossom.csv" , row.names=FALSE, quote=FALSE)
write.csv(SST13_test,"googledrive/SST13_flagged_Bubbles.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "flagged" folder
drive_folder_id <- "1QsjPCu8AVhePe7DGWBhRha26HXbyshcu"

# Upload the file to the specified Google Drive folder
drive_upload(media = "googledrive/SSM01_flagged_Buttercup.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/SSM20_flagged_Blossom.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/SST13_flagged_Bubbles.csv", path = as_id(drive_folder_id))

