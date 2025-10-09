##==============================================================================
## Project: QuEST
## This script will help you if you have to use the moving window approach to do the calibration
## Then you can flag spectra if they are too low or too high 
##==============================================================================

library(dplyr)
library(spectrolab)
library(googledrive) 

##############################################
#### Upload scan dataframe [with spectra] ####
##############################################
# This data is already matched #
# This is the "merged" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1g6aSuGnb--Qeyk-rceX82Y5wSNzCqFg0")

# List all xlsx files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")
3

#USF12
googledrive::drive_download(file = merged$id[merged$name=="USF12_absparams_Buttercup_clean.csv"], 
                            path = "googledrive/USF12_absparams_Buttercup_clean.csv",
                            overwrite = T)
#USF20
googledrive::drive_download(file = merged$id[merged$name=="USF20_absparams_Blossom_clean.csv"], 
                            path = "googledrive/USF20_absparams_Blossom_clean.csv",
                            overwrite = T)
#USF21
googledrive::drive_download(file = merged$id[merged$name=="USF21_absparams_Bubbles_clean.csv"], 
                            path = "googledrive/USF21_absparams_Bubbles_clean.csv",
                            overwrite = T)

# Let's load them separately first
USF12 <- read.csv("googledrive/USF12_absparams_Buttercup_clean.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
USF20 <- read.csv("googledrive/USF20_absparams_Blossom_clean.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
USF21 <- read.csv("googledrive/USF21_absparams_Bubbles_clean.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)

# DateTime at midnight is missing 00:00:00 time, so filling in that time using grep                     
USF12$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF12$DateTime)] <- paste(
  USF12$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF12$DateTime)],"00:00:00")
USF20$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF20$DateTime)] <- paste(
  USF20$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF20$DateTime)],"00:00:00")
USF21$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF21$DateTime)] <- paste(
  USF21$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF21$DateTime)],"00:00:00")

# Convert the DateTime column to POSIXct
USF12$DateTime <- as.POSIXct(USF12$DateTime, format = "%Y-%m-%d %H:%M:%S")
USF20$DateTime <- as.POSIXct(USF20$DateTime, format = "%Y-%m-%d %H:%M:%S")
USF21$DateTime <- as.POSIXct(USF21$DateTime, format = "%Y-%m-%d %H:%M:%S")

# Rename columns for all data frames
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}

# Apply the renaming to each data frame
USF12 <- rename_columns(USF12)
USF20 <- rename_columns(USF20)
USF21 <- rename_columns(USF21)

################################################
#### Edit data to look at it month by month ####
################################################
USF12_month <- USF12 %>%
  filter(format(DateTime, "%B") == "December")
USF20_month <- USF20 %>%
  filter(format(DateTime, "%B") == "October")
USF21_month <- USF21 %>%
  filter(format(DateTime, "%B") == "October")

################################################################################
#### Create matrices of ALL spectral data - raw data that needs calibration ####
################################################################################
# 1. Index FULL dataset with columns with absorbances
scan.spec12 = USF12[27:236] # change for USF12_month to see by month 
scan.spec20 = USF20[23:234]
scan.spec21 = USF21[27:236]

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
abs12 = (scan.spec12)
abs20 = (scan.spec20) 
abs21 = (scan.spec21) 

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wl12 = as.numeric(colnames(abs12))
wl20 = as.numeric(colnames(abs20))
wl21 = as.numeric(colnames(abs21))

# 4. Create a vector with sample labels that match the absorbance matrix rows. 
lastrow12 = as.numeric(nrow(abs12))
Num12 = c(1:lastrow12)

lastrow20 = as.numeric(nrow(abs20))
Num20 = c(1:lastrow20)

lastrow21 = as.numeric(nrow(abs21))
Num21 = c(1:lastrow21)

# 5. Create the final matrix 
#USF12
scan.matrix12 = cbind(abs12)
rownames(scan.matrix12) = as.numeric(Num12)
colnames(scan.matrix12) = as.numeric(wl12)

scan.matrix12 = as.matrix(scan.matrix12)
spec12 = spectra(value = abs12, bands = wl12, names = Num12)
plot(spec12) # Note = reflectance here = absorbance from the scans

#USF20
scan.matrix20 = cbind(abs20)
rownames(scan.matrix20) = as.numeric(Num20)
colnames(scan.matrix20) = as.numeric(wl20)

scan.matrix20 = as.matrix(scan.matrix20)
spec20 = spectra(value = abs20, bands = wl20, names = Num20)
plot(spec20) # Note = reflectance here = absorbance from the scans

#USF21
scan.matrix21 = cbind(abs21)
rownames(scan.matrix21) = as.numeric(Num21)
colnames(scan.matrix21) = as.numeric(wl21)

scan.matrix21 = as.matrix(scan.matrix21)
spec21 = spectra(value = abs21, bands = wl21, names = Num21)
plot(spec21) # Note = reflectance here = absorbance from the scans

##############################
#### Flagging absorbances ####
##############################
# FLAG works but I can't remember why I was doing this!!!!!!!!!!!!!!!!!!!!!!

### USF12 ###
# First check column numbers, look for spectral columns
data.frame(colnames(USF12))

USF12_test <- USF12 %>%
  mutate(across(27:236,
      list(clean = ~ if_else(. < 0 | . > 100, NA_real_, as.numeric(.))),
      .names = "{.col}_{.fn}"))

USF12_test <- USF12_test %>%
  mutate(across(27:236,
      list(flag  = ~ if_else(. < 0 | . > 100, TRUE, FALSE, missing = FALSE)),
      .names = "{.col}_{.fn}")) 

### USF20 ###
# First check column numbers, look for spectral columns
data.frame(colnames(USF20))

USF20_test <- USF20 %>%
  mutate(across(23:234,
      list(clean = ~ if_else(. < 0 | . > 100, NA_real_, as.numeric(.))),
      .names = "{.col}_{.fn}"))

USF20_test <- USF20_test %>%
  mutate(across(23:234,
           list(flag  = ~ if_else(. < 0 | . > 100, TRUE, FALSE, missing = FALSE)),
      .names = "{.col}_{.fn}"))

### USF21 ###
# First check column numbers, look for spectral columns
data.frame(colnames(USF21))
# Check which columns have non-numeric entries
sapply(USF21[27:236], function(x) sum(is.na(as.numeric(as.character(x)))))
# Convert abs to numeric and non-numeric entries to NA.
USF21 <- USF21 %>%
  mutate(across(27:236, ~ suppressWarnings(as.numeric(as.character(.)))))

# first create the clean abs
USF21_test <- USF21 %>%
  mutate(across(27:236,
      list(clean = ~ if_else(. < 0 | . > 100, NA_real_, as.numeric(.))),
      .names = "{.col}_{.fn}"))

# then create the flag for the cleaned abs
USF21_test <- USF21_test %>%
  mutate(across(27:236,
      list(flag  = ~ if_else(. < 0 | . > 100, TRUE, FALSE, missing = FALSE)),
      .names = "{.col}_{.fn}"))

################################
#### Plot clean absorbances ####
################################
data.frame(colnames(USF12_test))
data.frame(colnames(USF20_test))
data.frame(colnames(USF21_test))

scan.spec12 = USF12_test[243:443]
scan.spec20 = USF20_test[241:441]
scan.spec21 = USF21_test[243:443]

# Rows = wavelength
# Columns = date/time
abs12 = (scan.spec12)
abs20 = (scan.spec20) 
abs21 = (scan.spec21) 

wl12 <- gsub("_clean", "", colnames(abs12))   
wl12 <- as.numeric(wl12)
wl20 <- gsub("_clean", "", colnames(abs20))   
wl20 <- as.numeric(wl20)
wl21 <- gsub("_clean", "", colnames(abs21))   
wl21 <- as.numeric(wl21)


lastrow12 = as.numeric(nrow(abs12))
Num12 = c(1:lastrow12)
lastrow20 = as.numeric(nrow(abs20))
Num20 = c(1:lastrow20)
lastrow21 = as.numeric(nrow(abs21))
Num21 = c(1:lastrow21)

#USF12
scan.matrix12 = cbind(abs12)
rownames(scan.matrix12) = as.numeric(Num12)
colnames(scan.matrix12) = as.numeric(wl12)

scan.matrix12 = as.matrix(scan.matrix12)
spec12 = spectra(value = abs12, bands = wl12, names = Num12)
plot(spec12) # Note = reflectance here = absorbance from the scans

#USF20
scan.matrix20 = cbind(abs20)
rownames(scan.matrix20) = as.numeric(Num20)
colnames(scan.matrix20) = as.numeric(wl20)

scan.matrix20 = as.matrix(scan.matrix20)
spec20 = spectra(value = abs20, bands = wl20, names = Num20)
plot(spec20) # Note = reflectance here = absorbance from the scans

#USF21
scan.matrix21 = cbind(abs21)
rownames(scan.matrix21) = as.numeric(Num21)
colnames(scan.matrix21) = as.numeric(wl21)

scan.matrix21 = as.matrix(scan.matrix21)
spec21 = spectra(value = abs21, bands = wl21, names = Num21)
plot(spec21) # Note = reflectance here = absorbance from the scans

############################
#### Save flagged files ####
############################
# Make sure it is in datetime format
USF12_test$DateTime <- format(USF12_test$DateTime, "%Y-%m-%d %H:%M:%S")
USF20_test$DateTime <- format(USF20_test$DateTime, "%Y-%m-%d %H:%M:%S")
USF21_test$DateTime <- format(USF21_test$DateTime, "%Y-%m-%d %H:%M:%S")

# Save the new data frame to a CSV file
write.csv(USF12_test,"googledrive/USF12_flagged_Buttercup.csv" , row.names=FALSE, quote=FALSE)
write.csv(USF20_test,"googledrive/USF20_flagged_Blossom.csv" , row.names=FALSE, quote=FALSE)
write.csv(USF21_test,"googledrive/USF21_flagged_Bubbles.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "flagged" folder
drive_folder_id <- "1QsjPCu8AVhePe7DGWBhRha26HXbyshcu"

# Upload the file to the specified Google Drive folder
drive_upload(media = "googledrive/USF12_flagged_Buttercup.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/USF20_flagged_Blossom.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/USF21_flagged_Bubbles.csv", path = as_id(drive_folder_id))

