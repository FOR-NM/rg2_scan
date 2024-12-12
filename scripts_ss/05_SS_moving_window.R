##==============================================================================
## Project: QuEST
## This script will help you if you have to use the moving window approach to do the calibration
## Then you can flag spectra if they are too low or too high. This is for South Sandy
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(spectrolab)
library(dplyr)

########################################
#### Clear folders that we will use ####
########################################
# List and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)


##############################################
#### Upload scan dataframe [with spectra] ####
##############################################

# This data is already matched #
# This is the "merged" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1qpsqrmcnALNS9OVtoIDICdEuW5LkVuIR")

# List all CSVs files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")
3

#SSM01
googledrive::drive_download(file = merged$id[merged$name=="05_SSM01_merged.csv"], 
                            path = "googledrive/05_SSM01_merged.csv",
                            overwrite = T)
#SSM20
googledrive::drive_download(file = merged$id[merged$name=="05_SSM20_merged.csv"], 
                            path = "googledrive/05_SSM20_merged.csv",
                            overwrite = T)
#SST13
googledrive::drive_download(file = merged$id[merged$name=="05_SST13_merged.csv"], 
                            path = "googledrive/05_SST13_merged.csv",
                            overwrite = T)

# Let's load them separately first
SSM01 <- read.csv("googledrive/05_SSM01_merged.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
SSM20 <- read.csv("googledrive/05_SSM20_merged.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
SST13 <- read.csv("googledrive/05_SST13_merged.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)

# Convert the DateTime column to POSIXct
SSM01$DateTime <- as.POSIXct(SSM01$DateTime, format = "%Y-%m-%d %H:%M:%S")
SSM20$DateTime <- as.POSIXct(SSM20$DateTime, format = "%Y-%m-%d %H:%M:%S")
SST13$DateTime <- as.POSIXct(SST13$DateTime, format = "%Y-%m-%d %H:%M:%S")

# Rename columns for all data frames (e.g., SSM01, SSM20, SST13)
# This removes the X in front of all the spectra column names
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}

# Apply the renaming to each data frame
SSM01 <- rename_columns(SSM01)
SSM20 <- rename_columns(SSM20)
SST13 <- rename_columns(SST13)

# Drop empty column names
SSM01 <- SSM01[, !(is.na(colnames(SSM01)) | colnames(SSM01) == "")]
SSM20 <- SSM20[, !(is.na(colnames(SSM20)) | colnames(SSM20) == "")]
SST13 <- SST13[, !(is.na(colnames(SST13)) | colnames(SST13) == "")]

################################################
#### Edit data to look at it month by month ####
################################################

SSM01_month <- SSM01 %>%
  dplyr::filter(format(DateTime, "%B") == "November")
SSM20_month <- SSM20 %>%
  dplyr::filter(format(DateTime, "%B") == "November")
SST13_month <- SST13 %>%
  dplyr::filter(format(DateTime, "%B") == "November")

################################################################################
#### Create matrices of ALL spectral data - raw data that needs calibration ####
################################################################################

# 1. Index FULL dataset with columns with absorbances
scan.spec01 = SSM01_month[20:230]
scan.spec20 = SSM20_month[22:231]
scan.spec13 = SST13_month[23:232]

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
plot(spec01) # Note = reflectance here = absorbance from the scans

#SSM20
scan.matrix20 = cbind(abs20)
rownames(scan.matrix20) = as.numeric(Num20)
colnames(scan.matrix20) = as.numeric(wl20)

scan.matrix20 = as.matrix(scan.matrix20)
spec20 = spectra(value = abs20, bands = wl20, names = Num20)
plot(spec20) # Note = reflectance here = absorbance from the scans

#USF13
scan.matrix13 = cbind(abs13)
rownames(scan.matrix13) = as.numeric(Num13)
colnames(scan.matrix13) = as.numeric(wl13)

scan.matrix13 = as.matrix(scan.matrix13)
spec13 = spectra(value = abs13, bands = wl13, names = Num13)
plot(spec13) # Note = reflectance here = absorbance from the scans

##############################
#### Flagging absorbances ####
##############################

 #### HERE I AM HAVING A PROBLEM WHERE A LOT OF THE SPECTRA ARE  NEGATIVE (OR SOMETHING)
    # BECAUSE THIS FLAGGING IS NOT WORKING, BUT I DON'T WANT TO JUST NA A BUNCH OF ROWS
    # DOUBLE CHECK! 

### SSM01 ###
colnames(SSM01)[20:230]

SSM01_test <- SSM01 %>%
  # Create flag columns
  mutate(
    # checks if each value in columns 20 to 230 is negative and adds a flag column
    # and assigns "Y" if there is at least one negative value in the row, otherwise "N"
    flag_negative = ifelse(rowSums(across(20:230, ~ . < -1, .names = "col_{col}")) > 0, "Y", "N"),
    # checks if each value in columns 20 to 230 is greater than 100 and adds a flag column
    # assigns "Y" if there is at least one value greater than 100 in the row, otherwise "N"
    flag_above100 = ifelse(rowSums(across(20:230, ~ . > 100, .names = "col_{col}")) > 0, "Y", "N")
  ) %>%
  # Replace entire row with NA if any value is flagged, except for the flag columns
  # replaces all values in the flagged rows with NA if flags = "Y"
  mutate(across(
    where(is.numeric),
    ~ if_else(flag_negative == "Y" | flag_above100 == "Y", NA_real_, .)
    # NA_real_ ensures numeric NA
  ))

### SSM20 ###
colnames(SSM20)[22:231]

SSM20_test <- SSM20 %>%
  mutate(
    # Flag negative values
    flag_negative = ifelse(rowSums(across(22:231, ~ . < -1)) > 0, "Y", "N"),
    # Flag values above 100
    flag_above100 = ifelse(rowSums(across(22:231, ~ . > 100)) > 0, "Y", "N")
  ) %>%
  mutate(across(
    where(is.numeric),
    ~ if_else(flag_negative == "Y" | flag_above100 == "Y", NA_real_, .)
  ))

### SST13 ###
colnames(SST13)[23:232]

SST13_test <- SST13 %>%
  mutate(
    flag_negative = ifelse(rowSums(across(23:232, ~ . < -1)) > 0, "Y", "N"),
    flag_above100 = ifelse(rowSums(across(23:232, ~ . > 100)) > 0, "Y", "N")
  ) %>%
  mutate(across(
    where(is.numeric),
    ~ if_else(flag_negative == "Y" | flag_above100 == "Y", NA, .)
  ))

############################
#### Save flagged files ####
############################

# Make sure it is in datetime format
SSM01_test$DateTime <- format(SSM01_test$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(SSM01_test,"googledrive/SSM01_flagged.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
SSM20_test$DateTime <- format(SSM20_test$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(SSM20_test,"googledrive/SSM20_flagged.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
SST13_test$DateTime <- format(SST13_test$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(SST13_test,"googledrive/SST13_flagged.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1g6aSuGnb--Qeyk-rceX82Y5wSNzCqFg0"

# Upload the file to the specified Google Drive folder
drive_upload(media = "googledrive/SSM01_flagged.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/SSM20_flagged.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/SST13_flagged.csv", path = as_id(drive_folder_id))

