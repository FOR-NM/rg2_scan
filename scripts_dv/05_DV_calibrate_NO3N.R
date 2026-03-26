##==============================================================================
## Project: QuEST
## Here we will be Calibrating s::can data using Partial Least Squares Regression (PLSR) 
## Following Arial's s::can guide
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================


library(googledrive)
library(data.table)
library(xts)
library(dplyr)
library(pls)
library(spectrolab)
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
scan <- googledrive::as_id("https://drive.google.com/drive/folders/12N6uUxXTttdnadDrn43mL6ilz-Ui2eQX")

# List all xlsx files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")
3

#DVO
googledrive::drive_download(file = merged$id[merged$name=="DVO_chem.csv"], 
                            path = "googledrive/DVO_chem.csv",
                            overwrite = T)
#DVMS1
googledrive::drive_download(file = merged$id[merged$name=="DVMS1_chem.csv"], 
                            path = "googledrive/DVMS1_chem.csv",
                            overwrite = T)
#DVNWT5
googledrive::drive_download(file = merged$id[merged$name=="DVNWT5_chem.csv"], 
                            path = "googledrive/DVNWT5_chem.csv",
                            overwrite = T)

# Let's load them separately first
DVO <- read.csv("googledrive/DVO_chem.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
DVMS1 <- read.csv("googledrive/DVMS1_chem.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
DVNWT5 <- read.csv("googledrive/DVNWT5_chem.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)

# DateTime at midnight is missing 00:00:00 time, so filling in using grep
DVO$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DVO$DateTime)] <- paste(
  DVO$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DVO$DateTime)],"00:00:00")
DVMS1$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DVMS1$DateTime)] <- paste(
  DVMS1$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DVMS1$DateTime)],"00:00:00")
DVNWT5$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DVNWT5$DateTime)] <- paste(
  DVNWT5$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DVNWT5$DateTime)],"00:00:00")

# Convert the DateTime column to POSIXct
DVO$DateTime <- as.POSIXct(DVO$DateTime, format = "%Y-%m-%d %H:%M:%S")
DVMS1$DateTime <- as.POSIXct(DVMS1$DateTime, format = "%Y-%m-%d %H:%M:%S")
DVNWT5$DateTime <- as.POSIXct(DVNWT5$DateTime, format = "%Y-%m-%d %H:%M:%S")

# Remove NAs from DateTime column
DVO <- DVO %>%
  filter(!is.na(DateTime))
DVMS1 <- DVMS1 %>%
  filter(!is.na(DateTime))
DVNWT5 <- DVNWT5 %>%
  filter(!is.na(DateTime))

# Rename columns by removing the X in front of the spectra (that brakes the code somehow)
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}
# Apply the renaming to each data frame
DVO <- rename_columns(DVO)
DVMS1 <- rename_columns(DVMS1)
DVNWT5 <- rename_columns(DVNWT5)

# Extract NO3N data as time series objects (xts)
scan_NO3N_DVO <- xts(DVO$NO3.Neq..mg.l....Measured.value, order.by = DVO$DateTime)
scan_NO3N_DVMS1 <- xts(DVMS1$NO3.Neq..mg.l....Measured.value, order.by = DVMS1$DateTime)
scan_NO3N_DVNWT5 <- xts(DVNWT5$NO3.Neq..mg.l....Measured.value, order.by = DVNWT5$DateTime)

# Extract spectral data (assuming spectral columns are in range "DVNWT55.00.nm" to "250.00.nm")
scan.specDVO = xts(DVO[, c(15:110)], as.POSIXct(DVO$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.specDVMS1 = xts(DVMS1[, c(15:110)], as.POSIXct(DVMS1$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.specDVNWT5 = xts(DVNWT5[, c(15:110)], as.POSIXct(DVNWT5$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
# select full spectra
# note here that if there are 0s in your spectra, this code will throw an error
# so only use the wavelengths where you have detectable absorbance

#################################################
####  STEP 3: Compare grab and raw scan data ####
#################################################
# This is just a check to see how well the s::can did relative to your known concentrations 
# I upload this as a new data frame, just because in the previous step I had assigned these XTS values
# Feel free to change this! It's not the most efficient way to do this...
# Creating "Grab_sample" column based on values in "Sample.Name"
# Modify the Grab_sample column
DVO <- DVO %>% 
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",  # Assign "Y" if data exists
    TRUE ~ NA_character_  # Leave as NA otherwise
  ))
DVMS1 <- DVMS1 %>% 
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",
    TRUE ~ NA_character_
  ))
DVNWT5 <- DVNWT5 %>% 
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",
    TRUE ~ NA_character_
  ))

# Filter using "Grab_sample" column
grab_DVO = DVO[DVO$Grab_sample == "Y",] # Ony gets data when there is a Y
grab_DVMS1 = DVMS1[DVMS1$Grab_sample == "Y",] # Ony gets data when there is a Y
grab_DVNWT5 = DVNWT5[DVNWT5$Grab_sample == "Y",] # Ony gets data when there is a Y

grab.NO3NDVO = grab_DVO$NO3..mg.N.L.
grab.NO3NDVMS1 = grab_DVMS1$NO3..mg.N.L.
grab.NO3NDVNWT5 = grab_DVNWT5$NO3..mg.N.L.

#### remove a couple of problematic samples ####
grab_DVO <- grab_DVO %>%
  mutate(NO3..mg.N.L. = ifelse(Date %in% c("2024-09-25", "2025-05-01", "2025-04-10", "2025-04-03") | is.na(NO3..mg.N.L.), NA, NO3..mg.N.L.))
grab_DVMS1 <- grab_DVMS1 %>%
  mutate(NO3..mg.N.L. = ifelse(Date %in% c("2024-05-23", "2025-09-DVO", "2025-05-01", "2025-10-16", "2025-04-10", "2024-07-30") | is.na(NO3..mg.N.L.),NA,NO3..mg.N.L.))
grab_DVNWT5 <- grab_DVNWT5 %>%
  mutate(NO3..mg.N.L. = ifelse(Date %in% c("2024-09-18", "2025-06-13", "2025-09-DVO") | is.na(NO3..mg.N.L.), NA, NO3..mg.N.L.))

# compare grab vs scan NO3N
plot(grab_DVO$NO3.Neq..mg.l....Measured.value ~ grab_DVO$NO3..mg.N.L.)
ggplot(grab_DVO, aes(x = NO3..mg.N.L., y = NO3.Neq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.NO3NDVO = lm(grab_DVO$NO3.Neq..mg.l....Measured.value ~ grab_DVO$NO3..mg.N.L.)
summary(calib.mod.NO3NDVO)

plot(grab_DVMS1$NO3.Neq..mg.l....Measured.value ~ grab_DVMS1$NO3..mg.N.L.)
ggplot(grab_DVMS1, aes(x = NO3..mg.N.L., y = NO3.Neq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.NO3NDVMS1 = lm(grab_DVMS1$NO3.Neq..mg.l....Measured.value ~ grab_DVMS1$NO3..mg.N.L.)
summary(calib.mod.NO3NDVMS1)

plot(grab_DVNWT5$NO3.Neq..mg.l....Measured.value ~ grab_DVNWT5$NO3..mg.N.L.)
ggplot(grab_DVNWT5, aes(x = NO3..mg.N.L., y = NO3.Neq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.NO3NDVNWT5 = lm(grab_DVNWT5$NO3.Neq..mg.l....Measured.value ~ grab_DVNWT5$NO3..mg.N.L.)
summary(calib.mod.NO3NDVNWT5)

#######################################################################################
#### STEP 4: Create matrices of GRAB spectral data - this is the training data set ####
#######################################################################################
# 1. Index data set with columns with absorbances
# raw spectra
grab.spec.datDVO = grab_DVO[, c(15:110)]
grab.spec.datDVMS1 = grab_DVMS1[, c(15:110)]
grab.spec.datDVNWT5 = grab_DVNWT5[, c(15:110)]

# Rename columns for all data frames (e.g., DVO, DVMS1, DVNWT5)
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}

# Apply the renaming to each data frame
grab.spec.datDVO <- rename_columns(grab.spec.datDVO)
grab.spec.datDVMS1 <- rename_columns(grab.spec.datDVMS1)
grab.spec.datDVNWT5 <- rename_columns(grab.spec.datDVNWT5)

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
absDVO = (grab.spec.datDVO)  # this is not doing anything and just copying grab.spec.datDVO again as absDVO
absDVMS1 = (grab.spec.datDVMS1)
absDVNWT5 = (grab.spec.datDVNWT5)
#str(abs)

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wlDVO <- gsub("_clean", "", colnames(absDVO))   
wlDVO <- as.numeric(wlDVO)
wlDVMS1 <- gsub("_clean", "", colnames(absDVMS1))   
wlDVMS1 <- as.numeric(wlDVMS1)
wlDVNWT5 <- gsub("_clean", "", colnames(absDVNWT5))   
wlDVNWT5 <- as.numeric(wlDVNWT5)
str(wlDVNWT5)

# 4. Create a vector with sample labels that match the absorbance matrix rows. 
lastrowDVO = as.numeric(nrow(absDVO))
NumDVO = c(1:lastrowDVO)

lastrowDVMS1 = as.numeric(nrow(absDVMS1))
NumDVMS1 = c(1:lastrowDVMS1)

lastrowDVNWT5 = as.numeric(nrow(absDVNWT5))
NumDVNWT5 = c(1:lastrowDVNWT5)

# 5. Create the final matrix 
grab.matrixDVO = cbind(absDVO) # this is not binding anything and just copying absDVO again as grab.matrixDVO 
rownames(grab.matrixDVO) = as.numeric(NumDVO)
colnames(grab.matrixDVO) = as.numeric(wlDVO)
grab.matrixDVO = as.matrix(grab.matrixDVO)
str(grab.matrixDVO)
attributes(grab.matrixDVO)

grab.matrixDVMS1 = cbind(absDVMS1)
rownames(grab.matrixDVMS1) = as.numeric(NumDVMS1)
colnames(grab.matrixDVMS1) = as.numeric(wlDVMS1)
grab.matrixDVMS1 = as.matrix(grab.matrixDVMS1)
str(grab.matrixDVMS1)
attributes(grab.matrixDVMS1)

grab.matrixDVNWT5 = cbind(absDVNWT5)
rownames(grab.matrixDVNWT5) = as.numeric(NumDVNWT5)
colnames(grab.matrixDVNWT5) = as.numeric(wlDVNWT5)
grab.matrixDVNWT5 = as.matrix(grab.matrixDVNWT5)
str(grab.matrixDVNWT5)
attributes(grab.matrixDVNWT5)

# 6. Make this into spectral matrix for model
# Must be in format: grab.spectra = spectra(value = abs, bands = wl, names = Num)
grab.spectraDVO = spectra(value = absDVO, bands = wlDVO, names = NumDVO)
attributes(grab.spectraDVO)
plot(grab.spectraDVO) # Note, bands here = absorbance from the scans

grab.spectraDVMS1 = spectra(value = absDVMS1, bands = wlDVMS1, names = NumDVMS1)
attributes(grab.spectraDVMS1)
plot(grab.spectraDVMS1) # Note, bands here = absorbance from the scans

grab.spectraDVNWT5 = spectra(value = absDVNWT5, bands = wlDVNWT5, names = NumDVNWT5)
attributes(grab.spectraDVNWT5)
plot(grab.spectraDVNWT5) # Note, bands here = absorbance from the scans

#grab.spectra = as_spectra.list(grab.spectra, wave_unit = "wavenumber", measurement_nit = "absorbance")
grab.spectraDVO = as.matrix(grab.spectraDVO)
grab.spectraDVMS1 = as.matrix(grab.spectraDVMS1)
grab.spectraDVNWT5 = as.matrix(grab.spectraDVNWT5)
#str(grab.spectra)

# Change attributes so this is correct for scan data
attr(grab.spectraDVO, 'wave_unit') = 'wavelength'
attr(grab.spectraDVO, 'measurement_unit') = 'absorbance'
attributes(grab.spectraDVO)

attr(grab.spectraDVMS1, 'wave_unit') = 'wavelength'
attr(grab.spectraDVMS1, 'measurement_unit') = 'absorbance'
attributes(grab.spectraDVMS1)

attr(grab.spectraDVNWT5, 'wave_unit') = 'wavelength'
attr(grab.spectraDVNWT5, 'measurement_unit') = 'absorbance'
attributes(grab.spectraDVNWT5)

########################################################################################
#### STEP 5: Create matrices of ALL spectral data - raw data that needs calibration ####
########################################################################################
# 1. Index FULL dataset with columns with absorbances
# raw spectra
scan.specDVO = DVO[15:110]
scan.specDVMS1 = DVMS1[15:110] 
scan.specDVNWT5 = DVNWT5[15:110]

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
absDVO = (scan.specDVO)
absDVMS1 = (scan.specDVMS1) 
absDVNWT5 = (scan.specDVNWT5) 

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wlDVO <- gsub("_clean", "", colnames(absDVO))   
wlDVO <- as.numeric(wlDVO)
wlDVMS1 <- gsub("_clean", "", colnames(absDVMS1))   
wlDVMS1 <- as.numeric(wlDVMS1)
wlDVNWT5 <- gsub("_clean", "", colnames(absDVNWT5))   
wlDVNWT5 <- as.numeric(wlDVNWT5)

# 4. Create a vector with sample labels that match the absorbance matrix rows. 
lastrowDVO = as.numeric(nrow(absDVO))
NumDVO = c(1:lastrowDVO)

lastrowDVMS1 = as.numeric(nrow(absDVMS1))
NumDVMS1 = c(1:lastrowDVMS1)

lastrowDVNWT5 = as.numeric(nrow(absDVNWT5))
NumDVNWT5 = c(1:lastrowDVNWT5)

# 5. Create the final matrix 
#DVO
scan.matrixDVO = cbind(absDVO)
rownames(scan.matrixDVO) = as.numeric(NumDVO)
colnames(scan.matrixDVO) = as.numeric(wlDVO)

scan.matrixDVO = as.matrix(scan.matrixDVO)
specDVO = spectra(value = absDVO, bands = wlDVO, names = NumDVO)
plot(specDVO) # Note = reflectance here = absorbance from the scans

#DVMS1
scan.matrixDVMS1 = cbind(absDVMS1)
rownames(scan.matrixDVMS1) = as.numeric(NumDVMS1)
colnames(scan.matrixDVMS1) = as.numeric(wlDVMS1)

scan.matrixDVMS1 = as.matrix(scan.matrixDVMS1)
specDVMS1 = spectra(value = absDVMS1, bands = wlDVMS1, names = NumDVMS1)
plot(specDVMS1) # Note = reflectance here = absorbance from the scans

#DVNWT5
scan.matrixDVNWT5 = cbind(absDVNWT5)
rownames(scan.matrixDVNWT5) = as.numeric(NumDVNWT5)
colnames(scan.matrixDVNWT5) = as.numeric(wlDVNWT5)

scan.matrixDVNWT5 = as.matrix(scan.matrixDVNWT5)
specDVNWT5 = spectra(value = absDVNWT5, bands = wlDVNWT5, names = NumDVNWT5)
plot(specDVNWT5) # Note = reflectance here = absorbance from the scans

# NOTE: this is where you can identify problem spectra & remove them

# = as.spectra.list(spec)
scan.spectraDVO = as.matrix(specDVO)
str(scan.spectraDVO)
attr(scan.spectraDVO, 'wave_unit') = 'wavelength'
attr(scan.spectraDVO, 'measurement_unit') = 'absorbance'
attributes(scan.spectraDVO)

scan.spectraDVMS1 = as.matrix(specDVMS1)
str(scan.spectraDVMS1)
attr(scan.spectraDVMS1, 'wave_unit') = 'wavelength'
attr(scan.spectraDVMS1, 'measurement_unit') = 'absorbance'
attributes(scan.spectraDVMS1)

scan.spectraDVNWT5 = as.matrix(specDVNWT5)
str(scan.spectraDVNWT5)
attr(scan.spectraDVNWT5, 'wave_unit') = 'wavelength'
attr(scan.spectraDVNWT5, 'measurement_unit') = 'absorbance'
attributes(scan.spectraDVNWT5)

####################################################################
#### STEP 6: Create a new data frame with the spectral matrices ####
####################################################################
# This creates a data frame with 
# 1. NO3N (scan)
# 2. TSS (scab)
# 3. Full s::can spectra (from 2DVMS1-750nm)

length(scan_NO3N_DVO)
dim(scan.spectraDVO) 
class(scan.spectraDVO)

# NOTE: We use the I() function to protect the Spectra 
spectralcal.dfDVO = data.frame(NO3NDVO = scan_NO3N_DVO, SpectraDVO = I(scan.spectraDVO))
str(spectralcal.dfDVO)

spectralcal.dfDVMS1 = data.frame(NO3NDVMS1 = scan_NO3N_DVMS1, SpectraDVMS1 = I(scan.spectraDVMS1))
str(spectralcal.dfDVMS1)

spectralcal.dfDVNWT5 = data.frame(NO3NDVNWT5 = scan_NO3N_DVNWT5, SpectraDVNWT5 = I(scan.spectraDVNWT5))
str(spectralcal.dfDVNWT5)

# Also do this for the GRAB sample data
grabcal.dfDVO = data.frame(NO3NDVO = grab.NO3NDVO, SpectraDVO = I(grab.spectraDVO))
str(grabcal.dfDVO)

grabcal.dfDVMS1 = data.frame(NO3NDVMS1 = grab.NO3NDVMS1, SpectraDVMS1 = I(grab.spectraDVMS1))
str(grabcal.dfDVMS1)

grabcal.dfDVNWT5 = data.frame(NO3NDVNWT5 = grab.NO3NDVNWT5, SpectraDVNWT5 = I(grab.spectraDVNWT5))
str(grabcal.dfDVNWT5)

#################################################
#### STEP 7: Develop PLSR training data sets ####
#################################################
# Create a training and test data set
# Carbon
NTrainDVO = grabcal.dfDVO
NTestDVO = spectralcal.dfDVO

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
NmodDVO = plsr(NO3NDVO ~ SpectraDVO, ncomp = 8, data = NTrainDVO, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(NmodDVO) # optimized for 4 components

# Plot RMSE of the predictions to optimize model
plot(RMSEP(NmodDVO), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error
# NOTE: This plot may be messy, given low number of grab samples 
plot(NmodDVO, ncomp = 3, asp = 1, line = TRUE)

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################
# Predict model!
predictedNDVO = predict(NmodDVO, ncomp = 3, newdata = spectralcal.dfDVO) # use reduced error model
str(predictedNDVO)
plot(predictedNDVO)

write.csv(predictedNDVO, file = "predicted/predictedN_DVO.csv") # <- this is your newly calibrated dataset!

## NOTE: If your s::can has significant drift (e.g., which often happens when there is biofouling), 
# You might need to use a moving window approach to the calibraiton (i.e., calibrate 1 month at a time)

#################################################
#### STEP 7: Develop PLSR training data sets ####
#################################################
# Create a training and test dataset
# Carbon
NTrainDVMS1 = grabcal.dfDVMS1
NTestDVMS1 = spectralcal.dfDVMS1

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
NmodDVMS1 = plsr(NO3NDVMS1 ~ SpectraDVMS1, ncomp = 4, data = NTrainDVMS1, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(NmodDVMS1) # optimized for 4 components

# Plot RMSE of the predictions to optimize model
plot(RMSEP(NmodDVMS1), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error (in this case, x)
# NOTE: This plot may be messy, given low number of grab samples 
plot(NmodDVMS1, ncomp = 2, asp = 1, line = TRUE)

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################
# Predict model!
predictedNDVMS1 = predict(NmodDVMS1, ncomp = 2, newdata = spectralcal.dfDVMS1) # use reduced error model
str(predictedNDVMS1)
# Plot final predictions
plot(predictedNDVMS1)

write.csv(predictedNDVMS1, file = "predicted/predictedN_DVMS1.csv") # <- this is your newly calibrated dataset!

#################################################
#### STEP 7: Develop PLSR training data sets ####
#################################################
# Create a training and test dataset
# Carbon
NTrainDVNWT5 = grabcal.dfDVNWT5
NTestDVNWT5 = spectralcal.dfDVNWT5

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
NmodDVNWT5 = plsr(NO3NDVNWT5 ~ SpectraDVNWT5, ncomp = 3, data = NTrainDVNWT5, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(NmodDVNWT5) # optimized for 4 components

# Plot RMSE of the predictions to optimize model
plot(RMSEP(NmodDVNWT5), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error
# NOTE: This plot may be messy, given low number of grab samples 
plot(NmodDVNWT5, ncomp = 1, asp = 1, line = TRUE)

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################
# Predict model!
predictedNDVNWT5 = predict(NmodDVNWT5, ncomp = 1, newdata = spectralcal.dfDVNWT5) # use reduced error model
str(predictedNDVNWT5)
# Plot
plot(predictedNDVNWT5)

write.csv(predictedNDVNWT5, file = "predicted/predictedN_DVNWT5.csv") # <- this is your newly calibrated dataset!

# 1. Loadings Plot for DVO (Opposite Trend)
# This shows how the wavelengths contribute to each component (ncomp = 1, 2, 3, etc.)
plot(NmodDVO, plottype = "loading",
     comps = 1:2, # Plot the first two components for initial inspection
     main = "DVO NO3-N PLSR Loadings")

# 2. Loadings Plot for DVMS1 (Flat Trend)
plot(NmodDVMS1, plottype = "loading",
     comps = 1:2, # Plot the first two components
     main = "DVMS1 NO3-N PLSR Loadings")

# 3. Loadings Plot for DVNWT5 (Flat Trend)
# Examine the first few components for DVNWT5
plot(NmodDVNWT5, plottype = "loading",
     comps = 1:2, # Plot the first two components
     main = "DVNWT5 NO3-N PLSR Loadings")

# Convert predictedNDVNWT5 to a data frame
pred_df <- data.frame(
  DateTime = as.POSIXct(dimnames(predictedNDVNWT5)[[1]]),
  Predicted = as.numeric(predictedNDVNWT5))
# Plot
ggplot(pred_df, aes(x = DateTime, y = Predicted)) +
  geom_point(color = "steelblue") +
  labs(
    x = "DateTime",
    y = "Predicted NO3N (mg/L)",
    title = "Predicted NO3N over Time (DVNWT5)"
  ) +
  theme_minimal()

#######################
#### Save in Drive #### 
#######################
# Define the target folder ID in Google Drive
# This is the "predicted" folder
drive_folder_id <- "13bh64kWtdgknMUqdfDKkJ4JAzvWqLpu8"

# Upload the file to the specified Google Drive folder
drive_put(media = "predicted/predictedN_DVO.csv", path = as_id(drive_folder_id))
drive_put(media = "predicted/predictedN_DVMS1.csv", path = as_id(drive_folder_id))
drive_put(media = "predicted/predictedN_DVNWT5.csv", path = as_id(drive_folder_id))