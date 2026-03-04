##==============================================================================
## Project: QuEST
## Here we will be Calibrating s::can data using Partial Least Squares Regression (PLSR) 
## Following Arial's s::can guide
##==============================================================================

library(googledrive) 
library(data.table)

library(xts)
library(dplyr)
library(pls)
library(merTools)
library(devtools)
#install.packages("devtools")
#install.packages("devtools", repos = "http://cran.us.r-project.org")
library(spectrolab)
#install_github("meireles/spectrolab")
#install_github(repo = "meireles/spectrolab") # Install analysis package
# Make sure to hit "no" for install
library(ggplot2)
library(plotly)

###################################
#### Clear folders we will use ####
###################################
# List and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

######################################
#### STEP 1: Prep grab sample data ###
######################################
# What you need to do here is match the grab samples with the time stamp of the s::can
# This data was matched using previous scripts #
# See scripts merge_params_and_abs and merge_grabsamples_and_scan

#######################################################
#### STEP 2: Upload scan data frame [with spectra] ####
#######################################################
# This is the "with chem" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1PXTqYFSc6yArhhILFGxLF9a7_Rgu9A3q")

# List all xlsx files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")
3

#CTB
googledrive::drive_download(file = merged$id[merged$name=="CTB_merged.csv"], 
                            path = "googledrive/CTB_merged.csv",
                            overwrite = T)
#SMB
googledrive::drive_download(file = merged$id[merged$name=="SMB_merged.csv"], 
                            path = "googledrive/SMB_merged.csv",
                            overwrite = T)
#NCBd
googledrive::drive_download(file = merged$id[merged$name=="NCBd_merged.csv"], 
                            path = "googledrive/NCBd_merged.csv",
                            overwrite = T)
#LMP07
googledrive::drive_download(file = merged$id[merged$name=="LMP07_merged.csv"], 
                            path = "googledrive/LMP07_merged.csv",
                            overwrite = T)
#LMP27
googledrive::drive_download(file = merged$id[merged$name=="LMP27_merged.csv"], 
                            path = "googledrive/LMP27_merged.csv",
                            overwrite = T)

# Let's load them separately first
CTB <- read.csv("googledrive/CTB_merged.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
SMB <- read.csv("googledrive/SMB_merged.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
NCBd <- read.csv("googledrive/NCBd_merged.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
LMP07 <- read.csv("googledrive/LMP07_merged.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
LMP27 <- read.csv("googledrive/LMP27_merged.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)

# DateTime at midnight is missing 00:00:00 time, so filling in using grep
CTB$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",CTB$DateTime)] <- paste(
  CTB$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",CTB$DateTime)],"00:00:00")
SMB$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SMB$DateTime)] <- paste(
  SMB$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SMB$DateTime)],"00:00:00")
NCBd$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",NCBd$DateTime)] <- paste(
  NCBd$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",NCBd$DateTime)],"00:00:00")
LMP07$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",LMP07$DateTime)] <- paste(
  LMP07$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",LMP07$DateTime)],"00:00:00")
LMP27$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",LMP27$DateTime)] <- paste(
  LMP27$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",LMP27$DateTime)],"00:00:00")

# Convert the DateTime column to POSIXct
CTB$DateTime <- as.POSIXct(CTB$DateTime, format = "%Y-%m-%d %H:%M:%S")
SMB$DateTime <- as.POSIXct(SMB$DateTime, format = "%Y-%m-%d %H:%M:%S")
NCBd$DateTime <- as.POSIXct(NCBd$DateTime, format = "%Y-%m-%d %H:%M:%S")
LMP07$DateTime <- as.POSIXct(LMP07$DateTime, format = "%Y-%m-%d %H:%M:%S")
LMP27$DateTime <- as.POSIXct(LMP27$DateTime, format = "%Y-%m-%d %H:%M:%S")

# Remove NAs from DateTime column
CTB <- CTB %>%
  filter(!is.na(DateTime))
SMB <- SMB %>%
  filter(!is.na(DateTime))
NCBd <- NCBd %>%
  filter(!is.na(DateTime))
LMP07 <- LMP07 %>%
  filter(!is.na(DateTime))
LMP27 <- LMP27 %>%
  filter(!is.na(DateTime))

# Rename columns by removing the X in front of the spectra (that brakes the code somehow)
# rename_columns <- function(df) {
#   colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
#   return(df)
# }
# # Apply the renaming to each data frame
# USFCTB <- rename_columns(USFCTB)
# USFSMB <- rename_columns(USFSMB)
# USFNCBd <- rename_columns(USFNCBd)

# Extract NO3.N data as time series objects (xts)
scan_NO3.N_SMB <- xts(SMB$NO3.Neq..mg.l....Measured.value, order.by = SMB$DateTime)
scan_NO3.N_CTB <- xts(CTB$NO3.Neq..mg.l....Measured.value, order.by = CTB$DateTime)
scan_NO3.N_NCBd <- xts(NCBd$NO3.Neq..mg.l....Measured.value, order.by = NCBd$DateTime)
scan_NO3.N_LMP07 <- xts(LMP07$NO3.Neq..mg.l....Measured.value, order.by = LMP07$DateTime)
scan_NO3.N_LMP27 <- xts(LMP27$NO3.Neq..mg.l....Measured.value, order.by = LMP27$DateTime)

# Extract spectral data (assuming spectral columns are in range "SMB0.00.nm" to "4.00.nm")
scan.specSMB= xts(SMB[16:99], as.POSIXct(SMB$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.specCTB = xts(CTB[16:99], as.POSIXct(CTB$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.specNCBd = xts(NCBd[16:115], as.POSIXct(NCBd$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.spec07 = xts(LMP07[16:102], as.POSIXct(LMP07$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.spec27 = xts(LMP27[16:118], as.POSIXct(LMP27$DateTime, format = "%Y-%m-%d %H:%M:%S")) 

# select full spectra
# note here that if there are 0s in your spectra, this code will throw an error
# so only use the wavelengths where you have detectable absorbance

#################################################
####  STEP 3: Compare grab and raw scan data ####
#################################################
# This is just a check to see how well the s::can did relative to your known concentrations 
# I upload this as a new data frame, just because in the previous step I had assigned these XTS values
# Feel free to change this! It's not the most efficient way to do this...
# USFCTB <- USFCTB[,-1]
# USFSMB <- USFSMB[,-1]
# USFNCBd <- USFNCBd[,-1]

# Creating "Grab_sample" column based on values in "Sample.Name"
# Modify the Grab_sample column
SMB <- SMB %>% 
  mutate(Grab_sample = case_when(
    !is.na(NO3..mg.N.L.) & NO3..mg.N.L. != "" ~ "Y",  # Assign "Y" if data exists
    TRUE ~ NA_character_  # Leave as NA otherwise
  ))
CTB <- CTB %>% 
  mutate(Grab_sample = case_when(
    !is.na(NO3..mg.N.L.) & NO3..mg.N.L. != "" ~ "Y",
    TRUE ~ NA_character_
  ))
NCBd <- NCBd %>% 
  mutate(Grab_sample = case_when(
    !is.na(NO3..mg.N.L.) & NO3..mg.N.L. != "" ~ "Y",
    TRUE ~ NA_character_
  ))
LMP07 <- LMP07 %>% 
  mutate(Grab_sample = case_when(
    !is.na(NO3..mg.N.L.) & NO3..mg.N.L. != "" ~ "Y",
    TRUE ~ NA_character_
  ))
LMP27 <- LMP27 %>% 
  mutate(Grab_sample = case_when(
    !is.na(NO3..mg.N.L.) & NO3..mg.N.L. != "" ~ "Y",
    TRUE ~ NA_character_
  ))

# Filter using "Grab_sample" column
grab_SMB = SMB[SMB$Grab_sample == "Y",] # Ony gets data when there is a Y
grab_CTB = CTB[CTB$Grab_sample == "Y",] # Ony gets data when there is a Y
grab_NCBd = NCBd[NCBd$Grab_sample == "Y",] # Ony gets data when there is a Y
grab_LMP07 = LMP07[LMP07$Grab_sample == "Y",] # Ony gets data when there is a Y
grab_LMP27 = LMP27[LMP27$Grab_sample == "Y",] # Ony gets data when there is a Y

grab.NO3.NSMB = grab_SMB$NO3..mg.N.L.
grab.NO3.NCTB = grab_CTB$NO3..mg.N.L.
grab.NO3.NNCBd = grab_NCBd$NO3..mg.N.L.
grab.NO3.N07 = grab_LMP07$NO3..mg.N.L.
grab.NO3.N27 = grab_LMP27$NO3..mg.N.L.

#### remove a couple of problematic samples ####
# grab_USFCTB <- grab_USFCTB %>%
#   mutate(NO3..mg.N.L. = ifelse(DateTime == "SMB25-01-02 CTB:15:00" | is.na(NO3..mg.N.L.),NA,NO3..mg.N.L.))
# grab_USFSMB <- grab_USFSMB %>%
#   mutate(NO3..mg.N.L. = ifelse(DateTime == "SMB24-06-19 14:00:00" | is.na(NO3..mg.N.L.),NA,NO3..mg.N.L.))

# compare grab vs scan NO3.N
plot(grab_CTB$NO3.Neq..mg.l....Measured.value ~ grab_CTB$NO3..mg.N.L.)
ggplot(grab_CTB, aes(x = NO3..mg.N.L., y = NO3.Neq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.NO3.NCTB = lm(grab_CTB$NO3.Neq..mg.l....Measured.value ~ grab_CTB$NO3..mg.N.L.)
summary(calib.mod.NO3.NCTB)

ggplot(grab_SMB, aes(x = NO3..mg.N.L., y = NO3.Neq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.NO3.NSMB = lm(grab_SMB$NO3.Neq..mg.l....Measured.value ~ grab_SMB$NO3..mg.N.L.)
summary(calib.mod.NO3.NSMB)

ggplot(grab_NCBd, aes(x = NO3..mg.N.L., y = NO3.Neq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.NO3.NNCBs = lm(grab_NCBd$NO3.Neq..mg.l....Measured.value ~ grab_NCBd$NO3..mg.N.L.)
summary(calib.mod.NO3.NNCBs)


ggplot(grab_LMP07, aes(x = NO3..mg.N.L., y = NO3.Neq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.NO3.N07 = lm(grab_LMP07$NO3.Neq..mg.l....Measured.value ~ grab_LMP07$NO3..mg.N.L.)
summary(calib.mod.NO3.N07)

ggplot(grab_LMP27, aes(x = NO3..mg.N.L., y = NO3.Neq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.NO3.N27 = lm(grab_LMP27$NO3.Neq..mg.l....Measured.value ~ grab_LMP27$NO3..mg.N.L.)
summary(calib.mod.NO3.N27)

#######################################################################################
#### STEP 4: Create matrices of GRAB spectral data - this is the training data set ####
#######################################################################################
# 1. Index data set with columns with absorbances
# raw spectra
grab.spec.datCTB = grab_CTB[16:99] # Full spectra, with no NAs?
grab.spec.datSMB = grab_SMB[16:99]
grab.spec.datNCBd = grab_NCBd[16:115] 
grab.spec.dat07 = grab_LMP07[16:102]
grab.spec.dat27 = grab_LMP27[16:118]

# Rename columns for all data frames (e.g., USFCTB, USFSMB, USFNCBd)
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}
# Apply the renaming to each data frame
grab.spec.datCTB <- rename_columns(grab.spec.datCTB)
grab.spec.datSMB <- rename_columns(grab.spec.datSMB)
grab.spec.datNCBd <- rename_columns(grab.spec.datNCBd)
grab.spec.dat07 <- rename_columns(grab.spec.dat07)
grab.spec.dat27 <- rename_columns(grab.spec.dat27)

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
absCTB = (grab.spec.datCTB)  # this is not doing anything and just copying grab.spec.datCTB again as absCTB?
absSMB = (grab.spec.datSMB)
absNCBd = (grab.spec.datNCBd)
abs07 = (grab.spec.dat07)
abs27 = (grab.spec.dat27)
#str(abs)

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wlCTB <- gsub("_clean", "", colnames(absCTB))   
wlCTB <- as.numeric(wlCTB)
wlSMB <- gsub("_clean", "", colnames(absSMB))   
wlSMB <- as.numeric(wlSMB)
wlNCBd <- gsub("_clean", "", colnames(absNCBd))   
wlNCBd <- as.numeric(wlNCBd)
wl07 <- gsub("_clean", "", colnames(abs07))   
wl07 <- as.numeric(wl07)
wl27 <- gsub("_clean", "", colnames(abs27))   
wl27 <- as.numeric(wl27)

# 4. Create a vector with sample labels that match the absorbance matrix rows. 
lastrowCTB = as.numeric(nrow(absCTB))
NumCTB = c(1:lastrowCTB)
lastrowSMB = as.numeric(nrow(absSMB))
NumSMB = c(1:lastrowSMB)
lastrowNCBd = as.numeric(nrow(absNCBd))
NumNCBd = c(1:lastrowNCBd)
lastrow07 = as.numeric(nrow(abs07))
Num07 = c(1:lastrow07)
lastrow27 = as.numeric(nrow(abs27))
Num27 = c(1:lastrow27)

# 5. Create the final matrix 
grab.matrixCTB = cbind(absCTB) # this is not binding anything and just copying absCTB again as grab.matrixCTB 
rownames(grab.matrixCTB) = as.numeric(NumCTB)
colnames(grab.matrixCTB) = as.numeric(wlCTB)
grab.matrixCTB = as.matrix(grab.matrixCTB)
str(grab.matrixCTB)
attributes(grab.matrixCTB)

grab.matrixSMB = cbind(absSMB)
rownames(grab.matrixSMB) = as.numeric(NumSMB)
colnames(grab.matrixSMB) = as.numeric(wlSMB)
grab.matrixSMB = as.matrix(grab.matrixSMB)
str(grab.matrixSMB)
attributes(grab.matrixSMB)

grab.matrixNCBd = cbind(absNCBd)
rownames(grab.matrixNCBd) = as.numeric(NumNCBd)
colnames(grab.matrixNCBd) = as.numeric(wlNCBd)
grab.matrixNCBd = as.matrix(grab.matrixNCBd)
str(grab.matrixNCBd)
attributes(grab.matrixNCBd)

grab.matrix07 = cbind(abs07)
rownames(grab.matrix07) = as.numeric(Num07)
colnames(grab.matrix07) = as.numeric(wl07)
grab.matrix07 = as.matrix(grab.matrix07)
str(grab.matrix07)
attributes(grab.matrix07)

grab.matrix27 = cbind(abs27)
rownames(grab.matrix27) = as.numeric(Num27)
colnames(grab.matrix27) = as.numeric(wl27)
grab.matrix27 = as.matrix(grab.matrix27)
str(grab.matrix27)
attributes(grab.matrix27)

# 6. Make this into spectral matrix for model
# Must be in format: grab.spectra = spectra(value = abs, bands = wl, names = Num)
grab.spectraCTB = spectra(value = absCTB, bands = wlCTB, names = NumCTB)
attributes(grab.spectraCTB)
plot(grab.spectraCTB) # Note, bands here = absorbance from the scans

grab.spectraSMB = spectra(value = absSMB, bands = wlSMB, names = NumSMB)
attributes(grab.spectraSMB)
plot(grab.spectraSMB) # Note, bands here = absorbance from the scans

grab.spectraNCBd = spectra(value = absNCBd, bands = wlNCBd, names = NumNCBd)
attributes(grab.spectraNCBd)
plot(grab.spectraNCBd) # Note, bands here = absorbance from the scans

grab.spectra07 = spectra(value = abs07, bands = wl07, names = Num07)
attributes(grab.spectra07)
plot(grab.spectra07)

grab.spectra27 = spectra(value = abs27, bands = wl27, names = Num27)
attributes(grab.spectra27)
plot(grab.spectra27)

#grab.spectra = as_spectra.list(grab.spectra, wave_unit = "wavenumber", measurement_nit = "absorbance")
grab.spectraCTB = as.matrix(grab.spectraCTB)
grab.spectraSMB = as.matrix(grab.spectraSMB)
grab.spectraNCBd = as.matrix(grab.spectraNCBd)
grab.spectra07 = as.matrix(grab.spectra07)
grab.spectra27 = as.matrix(grab.spectra27)
#str(grab.spectra)

# Change attributes so this is correct for scan data
attr(grab.spectraCTB, 'wave_unit') = 'wavelength'
attr(grab.spectraCTB, 'measurement_unit') = 'absorbance'
attributes(grab.spectraCTB)

attr(grab.spectraSMB, 'wave_unit') = 'wavelength'
attr(grab.spectraSMB, 'measurement_unit') = 'absorbance'
attributes(grab.spectraSMB)

attr(grab.spectraNCBd, 'wave_unit') = 'wavelength'
attr(grab.spectraNCBd, 'measurement_unit') = 'absorbance'
attributes(grab.spectraNCBd)

attr(grab.spectra07, 'wave_unit') = 'wavelength'
attr(grab.spectra07, 'measurement_unit') = 'absorbance'
attributes(grab.spectra07)

attr(grab.spectra27, 'wave_unit') = 'wavelength'
attr(grab.spectra27, 'measurement_unit') = 'absorbance'
attributes(grab.spectra27)

########################################################################################
#### STEP 5: Create matrices of ALL spectral data - raw data that needs calibration ####
########################################################################################
# 1. Index FULL dataset with columns with absorbances
# raw spectra
scan.specCTB = CTB[16:99]
scan.specSMB = SMB[16:99] 
scan.specNCBd = NCBd[16:115]
scan.spec07 = LMP07[16:102]
scan.spec27 = LMP27[16:118]

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
absCTB = (scan.specCTB)
absSMB = (scan.specSMB) 
absNCBd = (scan.specNCBd) 
abs07 = (scan.spec07) 
abs27 = (scan.spec27) 

# Rename columns for all data frames (e.g., USFCTB, USFSMB, USFNCBd)
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}
# Apply the renaming to each data frame
absCTB <- rename_columns(absCTB)
absSMB <- rename_columns(absSMB)
absNCBd <- rename_columns(absNCBd)
abs07 <- rename_columns(abs07)
abs27 <- rename_columns(abs27)

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wlCTB <- gsub("_clean", "", colnames(absCTB))   
wlCTB <- as.numeric(wlCTB)
wlSMB <- gsub("_clean", "", colnames(absSMB))   
wlSMB <- as.numeric(wlSMB)
wlNCBd <- gsub("_clean", "", colnames(absNCBd))   
wlNCBd <- as.numeric(wlNCBd)
wl07 <- gsub("_clean", "", colnames(abs07))   
wl07 <- as.numeric(wl07)
wl27 <- gsub("_clean", "", colnames(abs27))   
wl27 <- as.numeric(wl27)

# 4. Create a vector with sample labels that match the absorbance matrix rows. 
lastrowCTB = as.numeric(nrow(absCTB))
NumCTB = c(1:lastrowCTB)

lastrowSMB = as.numeric(nrow(absSMB))
NumSMB = c(1:lastrowSMB)

lastrowNCBd = as.numeric(nrow(absNCBd))
NumNCBd = c(1:lastrowNCBd)

lastrow07 = as.numeric(nrow(abs07))
Num07 = c(1:lastrow07)

lastrow27 = as.numeric(nrow(abs27))
Num27 = c(1:lastrow27)

# 5. Create the final matrix 
#CTB
scan.matrixCTB = cbind(absCTB)
rownames(scan.matrixCTB) = as.numeric(NumCTB)
colnames(scan.matrixCTB) = as.numeric(wlCTB)

scan.matrixCTB = as.matrix(scan.matrixCTB)
specCTB = spectra(value = absCTB, bands = wlCTB, names = NumCTB)
plot(specCTB) # Note = reflectance here = absorbance from the scans

#SMB
scan.matrixSMB = cbind(absSMB)
rownames(scan.matrixSMB) = as.numeric(NumSMB)
colnames(scan.matrixSMB) = as.numeric(wlSMB)

scan.matrixSMB = as.matrix(scan.matrixSMB)
specSMB = spectra(value = absSMB, bands = wlSMB, names = NumSMB)
plot(specSMB) # Note = reflectance here = absorbance from the scans

#NCBd
scan.matrixNCBd = cbind(absNCBd)
rownames(scan.matrixNCBd) = as.numeric(NumNCBd)
colnames(scan.matrixNCBd) = as.numeric(wlNCBd)

scan.matrixNCBd = as.matrix(scan.matrixNCBd)
specNCBd = spectra(value = absNCBd, bands = wlNCBd, names = NumNCBd)
plot(specNCBd) # Note = reflectance here = absorbance from the scans

#LMP07
scan.matrix07 = cbind(abs07)
rownames(scan.matrix07) = as.numeric(Num07)
colnames(scan.matrix07) = as.numeric(wl07)

scan.matrix07 = as.matrix(scan.matrix07)
spec07 = spectra(value = abs07, bands = wl07, names = Num07)
plot(spec07) # Note = reflectance here = absorbance from the scans

#LMP27
scan.matrix27 = cbind(abs27)
rownames(scan.matrix27) = as.numeric(Num27)
colnames(scan.matrix27) = as.numeric(wl27)

scan.matrix27 = as.matrix(scan.matrix27)
spec27 = spectra(value = abs27, bands = wl27, names = Num27)
plot(spec27) # Note = reflectance here = absorbance from the scans

# NOTE: this is where you can identify problem spectra & remove them

# = as.spectra.list(spec)
scan.spectraCTB = as.matrix(specCTB)
str(scan.spectraCTB)
attr(scan.spectraCTB, 'wave_unit') = 'wavelength'
attr(scan.spectraCTB, 'measurement_unit') = 'absorbance'
attributes(scan.spectraCTB)

scan.spectraSMB = as.matrix(specSMB)
str(scan.spectraSMB)
attr(scan.spectraSMB, 'wave_unit') = 'wavelength'
attr(scan.spectraSMB, 'measurement_unit') = 'absorbance'
attributes(scan.spectraSMB)

scan.spectraNCBd = as.matrix(specNCBd)
str(scan.spectraNCBd)
attr(scan.spectraNCBd, 'wave_unit') = 'wavelength'
attr(scan.spectraNCBd, 'measurement_unit') = 'absorbance'
attributes(scan.spectraNCBd)

scan.spectra07 = as.matrix(spec07)
str(scan.spectra07)
attr(scan.spectra07, 'wave_unit') = 'wavelength'
attr(scan.spectra07, 'measurement_unit') = 'absorbance'
attributes(scan.spectra07)

scan.spectra27 = as.matrix(spec27)
str(scan.spectra27)
attr(scan.spectra27, 'wave_unit') = 'wavelength'
attr(scan.spectra27, 'measurement_unit') = 'absorbance'
attributes(scan.spectra27)

####################################################################
#### STEP 6: Create a new data frame with the spectral matrices ####
####################################################################
# This creates a data frame with 
# 1. NO3.N (scan)
# 3. Full s::can spectra (from 2SMB-750nm)

length(scan_NO3.N_CTB)
dim(scan.spectraCTB) 
class(scan.spectraCTB)

# NOTE: We use the I() function to protect the Spectra 
spectralcal.dfCTB = data.frame(NO3.NCTB = scan_NO3.N_CTB, SpectraCTB = I(scan.spectraCTB))
str(spectralcal.dfCTB)

spectralcal.dfSMB = data.frame(NO3.NSMB = scan_NO3.N_SMB, SpectraSMB = I(scan.spectraSMB))
str(spectralcal.dfSMB)

spectralcal.dfNCBd = data.frame(NO3.NNCBd = scan_NO3.N_NCBd, SpectraNCBd = I(scan.spectraNCBd))
str(spectralcal.dfNCBd)

spectralcal.df07 = data.frame(NO3.N07 = scan_NO3.N_LMP07, Spectra07 = I(scan.spectra07))
str(spectralcal.df07)
spectralcal.df27 = data.frame(NO3.N27 = scan_NO3.N_LMP27, Spectra27 = I(scan.spectra27))
str(spectralcal.df27)

# Also do this for the GRAB sample data
grabcal.dfCTB = data.frame(NO3.NCTB = grab.NO3.NCTB, SpectraCTB = I(grab.spectraCTB))
str(grabcal.dfCTB)

grabcal.dfSMB = data.frame(NO3.NSMB = grab.NO3.NSMB, SpectraSMB = I(grab.spectraSMB))
str(grabcal.dfSMB)

grabcal.dfNCBd = data.frame(NO3.NNCBd = grab.NO3.NNCBd, SpectraNCBd = I(grab.spectraNCBd))
str(grabcal.dfNCBd)
grabcal.df07 = data.frame(NO3.N07 = grab.NO3.N07, Spectra07 = I(grab.spectra07))
str(grabcal.df07)
grabcal.df27 = data.frame(NO3.N27 = grab.NO3.N27, Spectra27 = I(grab.spectra27))
str(grabcal.df27)

#####################################################
#### STEP 7 CTB: Develop PLSR training data sets ####
#####################################################
# Create a training and test data set
# Carbon
NTrainCTB = grabcal.dfCTB
NTestCTB = spectralcal.dfCTB

# NO3N
NTrainCTB = grabcal.dfCTB
NTestCTB = spectralcal.dfCTB

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
NmodCTB = plsr(NO3.NCTB ~ SpectraCTB, ncomp = 2, data = NTrainCTB, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(NmodCTB) # optimized for 4 components

# Plot RMSE of the predictions to optimize model
plot(RMSEP(NmodCTB), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error
# NOTE: This plot may be messy, given low number of grab samples 
plot(NmodCTB, ncomp = 1, asp = 1, line = TRUE)

########################################################################
#### STEP 8 CTB: Make predictions based on reduced-error PLSR model #### 
########################################################################
# Predict model!
predictedNCTB = predict(NmodCTB, ncomp = 1, newdata = spectralcal.dfCTB) # use reduced error model
str(predictedNCTB)
plot(predictedNCTB)

write.csv(predictedNCTB, file = "predicted/predictedN_CTB_vclean.csv") # <- this is your newly calibrated dataset!

# Convert predictedNCTB to a data frame
pred_df <- data.frame(
  DateTime = as.POSIXct(dimnames(predictedNCTB)[[1]]),
  Predicted = as.numeric(predictedNCTB))
# Plot
p <- ggplot(pred_df, aes(x = DateTime, y = Predicted)) +
  geom_line(color = "steelblue") +
  labs(
    x = "DateTime",
    y = "Predicted NO3.N (mg/L)",
    title = "Predicted NO3.N over Time (CTB)"
  ) +
  theme_minimal()
ggplotly(p)

## NOTE: If your s::can has significant drift (e.g., which often happens when there is biofouling), 
# You might need to use a moving window approach to the calibraiton (i.e., calibrate 1 month at a time)

#####################################################
#### STEP 7 SMB: Develop PLSR training data sets ####
#####################################################
# Create a training and test dataset
# Carbon
NTrainSMB = grabcal.dfSMB
NTestSMB = spectralcal.dfSMB

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
NmodSMB = plsr(NO3.NSMB ~ SpectraSMB, ncomp = 1, data = NTrainSMB, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(NmodSMB) # optimized for 4 components

# Plot RMSE of the predictions to optimize model
plot(RMSEP(NmodSMB), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error (in this case, x)
# NOTE: This plot may be messy, given low number of grab samples 
plot(NmodSMB, ncomp = 1, asp = 1, line = TRUE)

########################################################################
#### STEP 8 SMB: Make predictions based on reduced-error PLSR model #### 
########################################################################
# Predict model!
predictedNSMB = predict(NmodSMB, ncomp = 1, newdata = spectralcal.dfSMB) # use reduced error model
str(predictedNSMB)
# Plot final predictions
plot(predictedNSMB)

write.csv(predictedNSMB, file = "predicted/predictedN_SMB_vclean.csv") # <- this is your newly calibrated dataset!

## NOTE: If your s::can has significant drift (e.g., which often happens when there is biofouling), 
# You might need to use a moving window approach to the calibraiton (i.e., calibrate 1 month at a time)
# This is a bit more complicated, so start with this simple calibration first. 

# Convert predictedNSMB to a data frame
pred_df <- data.frame(
  DateTime = as.POSIXct(dimnames(predictedNSMB)[[1]]),
  Predicted = as.numeric(predictedNSMB))
# Plot
ggplot(pred_df, aes(x = DateTime, y = Predicted)) +
  geom_point(color = "steelblue") +
  labs(
    x = "DateTime",
    y = "Predicted NO3.N (mg/L)",
    title = "Predicted NO3.N over Time (USFSMB)"
  ) +
  theme_minimal()

######################################################
#### STEP 7 NCBd: Develop PLSR training data sets ####
######################################################
# Create a training and test dataset
# Carbon
NTrainNCBd = grabcal.dfNCBd
NTestNCBd = spectralcal.dfNCBd

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
NmodNCBd = plsr(NO3.NNCBd ~ SpectraNCBd, ncomp = 7, data = NTrainNCBd, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(NmodNCBd) # optimized for 4 components

# Plot RMSE of the predictions to optimize model
plot(RMSEP(NmodNCBd), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error
# NOTE: This plot may be messy, given low number of grab samples 
plot(NmodNCBd, ncomp = 2, asp = 1, line = TRUE)

#########################################################################
#### STEP 8 NCBd: Make predictions based on reduced-error PLSR model #### 
#########################################################################
# Predict model!
predictedNNCBd = predict(NmodNCBd, ncomp = 2, newdata = spectralcal.dfNCBd) # use reduced error model
str(predictedNNCBd)
# Plot
plot(predictedNNCBd)

write.csv(predictedNNCBd, file = "predicted/predictedN_NCBd_vclean.csv") # <- this is your newly calibrated dataset!

# 1. Loadings Plot for USFCTB (Opposite Trend)
# This shows how the wavelengths contribute to each component (ncomp = 1, 2, 3, etc.)
plot(NmodCTB, plottype = "loading",
     comps = 1:2, # Plot the first two components for initial inspection
     main = "CTB NO3-N PLSR Loadings")

# 2. Loadings Plot for USFSMB (Flat Trend)
plot(NmodSMB, plottype = "loading",
     comps = 1:2, # Plot the first two components
     main = "SMB NO3-N PLSR Loadings")

# 3. Loadings Plot for USFNCBd (Flat Trend)
# Examine the first few components for USFNCBd
plot(NmodNCBd, plottype = "loading",
     comps = 1:2, # Plot the first two components
     main = "NCBd NO3-N PLSR Loadings")

# Convert predictedNNCBd to a data frame
pred_df <- data.frame(
  DateTime = as.POSIXct(dimnames(predictedNNCBd)[[1]]),
  Predicted = as.numeric(predictedNNCBd))
# Plot
ggplot(pred_df, aes(x = DateTime, y = Predicted)) +
  geom_point(color = "steelblue") +
  labs(
    x = "DateTime",
    y = "Predicted NO3.N (mg/L)",
    title = "Predicted NO3.N over Time (USFNCBd)"
  ) +
  theme_minimal()

#######################################################
#### STEP 7 LMP07: Develop PLSR training data sets ####
#######################################################
# Create a training and test dataset
# Carbon
NTrain07 = grabcal.df07
NTest07 = spectralcal.df07

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
Nmod07 = plsr(NO3.N07 ~ Spectra07, ncomp = 7, data = NTrain07, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Nmod07) # optimized for 4 components

# Plot RMSE of the predictions to optimize model
plot(RMSEP(Nmod07), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error
# NOTE: This plot may be messy, given low number of grab samples 
plot(Nmod07, ncomp = 1, asp = 1, line = TRUE)

##########################################################################
#### STEP 8 LMP07: Make predictions based on reduced-error PLSR model #### 
##########################################################################
# Predict model!
predictedN07 = predict(Nmod07, ncomp = 1, newdata = spectralcal.df07) # use reduced error model
str(predictedN07)
# Plot
plot(predictedN07)

write.csv(predictedN07, file = "predicted/predictedN_LMP07_vclean.csv") # <- this is your newly calibrated dataset!

#######################################################
#### STEP 7 LMP27: Develop PLSR training data sets ####
#######################################################
# Create a training and test dataset
# Carbon
NTrain27 = grabcal.df27
NTest27 = spectralcal.df27

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
Nmod27 = plsr(NO3.N27 ~ Spectra27, ncomp = 6, data = NTrain27, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Nmod27) # optimized for 4 components

# Plot RMSE of the predictions to optimize model
plot(RMSEP(Nmod27), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error
# NOTE: This plot may be messy, given low number of grab samples 
plot(Nmod27, ncomp = 1, asp = 1, line = TRUE)

##########################################################################
#### STEP 8 LMP27: Make predictions based on reduced-error PLSR model #### 
##########################################################################
# Predict model!
predictedN27 = predict(Nmod27, ncomp = 1, newdata = spectralcal.df27) # use reduced error model
str(predictedN27)
# Plot
plot(predictedN27)

write.csv(predictedN27, file = "predicted/predictedN_LMP27_vclean.csv") # <- this is your newly calibrated dataset!

#######################
#### Save in Drive #### 
#######################
# Define the target folder ID in Google Drive
# This is the "predicted" folder
drive_folder_id <- "1wa1ycqUYv56y3fTn1-VaN2K-NLU3rFeU"

# Upload the file to the specified Google Drive folder
drive_upload(media = "predicted/predictedN_USFCTB_vclean.csv", path = as_id(drive_folder_id))
drive_upload(media = "predicted/predictedN_USFSMB_vclean.csv", path = as_id(drive_folder_id))
drive_upload(media = "predicted/predictedN_USFNCBd_vclean.csv", path = as_id(drive_folder_id))

## NOTE: If your s::can has significant drift (e.g., which often happens when there is biofouling), 
# You might need to use a moving window approach to the calibraiton (i.e., calibrate 1 month at a time)
# This is a bit more complicated, so start with this simple calibration first. 