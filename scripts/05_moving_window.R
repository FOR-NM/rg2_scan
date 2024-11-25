##==============================================================================
## Project: QuEST
## This script will help you if you have to use the moving window approach to do the calibration
## Then you can flag spectra if they are too low or too high 
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

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
googledrive::drive_download(file = merged$id[merged$name=="USF12_merged_Buttercup.csv"], 
                            path = "googledrive/USF12_merged_Buttercup.csv",
                            overwrite = T)
#USF20
googledrive::drive_download(file = merged$id[merged$name=="USF20_merged_Blossom.csv"], 
                            path = "googledrive/USF20_merged_Blossom.csv",
                            overwrite = T)
#USF21
googledrive::drive_download(file = merged$id[merged$name=="USF21_merged_Bubbles.csv"], 
                            path = "googledrive/USF21_merged_Bubbles.csv",
                            overwrite = T)

# Let's load them separately first
USF12 <- read.csv("googledrive/USF12_merged_Buttercup.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
USF20 <- read.csv("googledrive/USF20_merged_Blossom.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
USF21 <- read.csv("googledrive/USF21_merged_Bubbles.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)

# Convert the DateTime column to POSIXct
USF12$DateTime <- as.POSIXct(USF12$DateTime, format = "%Y-%m-%d %H:%M:%S")
USF20$DateTime <- as.POSIXct(USF20$DateTime, format = "%Y-%m-%d %H:%M:%S")
USF21$DateTime <- as.POSIXct(USF21$DateTime, format = "%Y-%m-%d %H:%M:%S")

# Rename columns for all data frames (e.g., USF12, USF20, USF21)
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

USF21_month <- USF21 %>%
  filter(format(DateTime, "%B") == "October")

################################################################################
#### Create matrices of ALL spectral data - raw data that needs calibration ####
################################################################################

# 1. Index FULL dataset with columns with absorbances
scan.spec12 = USF12_month[21:230]
scan.spec20 = USF20_month[21:230]
scan.spec21 = USF21_month[21:230]

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

### USF12 ###
# Check if there are negative values
USF12 <- USF12 %>% 
  mutate(flag_negative = ifelse(
    rowSums(across(21:230, ~ . < 0)) > 0,  # Check if any value in the range is < 0
    "Y",                                     # Assign "Y" if any value is < 0
    "N"                                      # Assign "N" otherwise
))

# Check if there are values above 100
USF12 <- USF12 %>% 
  mutate(flag_above100 = ifelse(
    rowSums(across(21:230, ~ . > 100)) > 0,  # Check if any value in the range is > 100
    "Y",                                     # Assign "Y" if any value is > 100
    "N"                                      # Assign "N" otherwise
  ))

### USF20 ###
# Check if there are negative values
USF20 <- USF20 %>% 
  mutate(flag_negative = ifelse(
    rowSums(across(21:230, ~ . < 0)) > 0,  # Check if any value in the range is < 0
    "Y",                                     # Assign "Y" if any value is < 0
    "N"                                      # Assign "N" otherwise
  ))

# Check if there are values above 100
USF20 <- USF20 %>% 
  mutate(flag_above100 = ifelse(
    rowSums(across(21:230, ~ . > 100)) > 0,  # Check if any value in the range is > 100
    "Y",                                     # Assign "Y" if any value is > 100
    "N"                                      # Assign "N" otherwise
  ))

### USF21 ###
# Check if there are negative values
USF21 <- USF21 %>% 
  mutate(flag_negative = ifelse(
    rowSums(across(21:230, ~ . < 0)) > 0,  # Check if any value in the range is < 0
    "Y",                                     # Assign "Y" if any value is < 0
    "N"                                      # Assign "N" otherwise
  ))

# Check if there are values above 100
USF21 <- USF21 %>% 
  mutate(flag_above100 = ifelse(
    rowSums(across(21:230, ~ . > 100)) > 0,  # Check if any value in the range is > 100
    "Y",                                     # Assign "Y" if any value is > 100
    "N"                                      # Assign "N" otherwise
  ))

############################
#### Save flagged files ####
############################

# Make sure it is in datetime format
USF12$DateTime <- format(USF12$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(USF12,"googledrive/USF12_flagged_Buttercup.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
USF20$DateTime <- format(USF20$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(USF20,"googledrive/USF20_flagged_Blossom.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
USF21$DateTime <- format(USF21$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(USF21,"googledrive/USF21_flagged_Bubbles.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1g6aSuGnb--Qeyk-rceX82Y5wSNzCqFg0"

# Upload the file to the specified Google Drive folder
drive_upload(media = "googledrive/USF12_flagged_Buttercup.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/USF20_flagged_Blossom.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/USF21_flagged_Bubbles.csv", path = as_id(drive_folder_id))

