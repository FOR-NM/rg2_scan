##==============================================================================
## Project: QuEST
## Here we will be Calibrating s::can data using Partial Least Squares Regression (PLSR) 
## Following Arial's s::can guide
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

# Importing data
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
# See scripts 03_merge_params_and_abs and 04_merge_grabsamples_and_scan

######################################################
#### STEP 2: Upload scan data frame [with spectra] ####
######################################################

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

# Drop empty column names
SSM01 <- SSM01[, !(is.na(colnames(SSM01)) | colnames(SSM01) == "")]
SSM20 <- SSM20[, !(is.na(colnames(SSM20)) | colnames(SSM20) == "")]
SST13 <- SST13[, !(is.na(colnames(SST13)) | colnames(SST13) == "")]

# Rename columns by removing the X in front of the spectra (that brakes the core somehow)
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}

# Apply the renaming to each data frame
SSM01 <- rename_columns(SSM01)
SSM20 <- rename_columns(SSM20)
SST13 <- rename_columns(SST13)

# Extract DOC NO3 NO3N and TSS data as time series objects (xts)
scan_DOC_SSM01 <- xts(SSM01$DOC, order.by = SSM01$DateTime)
scan_TSS_SSM01 <- xts(SSM01$TSS, order.by = SSM01$DateTime)
scan_NO3N_SSM01 <- xts(SSM01$NO3N, order.by = SSM01$DateTime)
scan_NO3_SSM01 <- xts(SSM01$NO3, order.by = SSM01$DateTime)

scan_DOC_SSM20 <- xts(SSM20$DOC, order.by = SSM20$DateTime)
scan_TSS_SSM20 <- xts(SSM20$TSS, order.by = SSM20$DateTime)
scan_NO3N_SSM20 <- xts(SSM20$NO3N, order.by = SSM20$DateTime)
scan_NO3_SSM20 <- xts(SSM20$NO3, order.by = SSM20$DateTime)

scan_DOC_SST13 <- xts(SST13$DOC, order.by = SST13$DateTime)
scan_TSS_SST13 <- xts(SST13$TSS, order.by = SST13$DateTime)
scan_NO3N_SST13 <- xts(SST13$NO3N, order.by = SST13$DateTime)
scan_NO3_SST13 <- xts(SST13$NO3, order.by = SST13$DateTime)

# Extract spectral data (assuming spectral columns are in range "X200.00.nm" to "X750.00.nm")
scan.spec01 = xts(SSM01[13:230], as.POSIXct(SSM01$DateTime, format = "%m/%d/%Y %H:%M")) 
scan.spec20 = xts(SSM20[13:231], as.POSIXct(SSM20$DateTime, format = "%m/%d/%Y %H:%M")) 
scan.spec13 = xts(SST13[25:234], as.POSIXct(SST13$DateTime, format = "%m/%d/%Y %H:%M")) 
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
SSM01 <- SSM01 %>% 
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",  # Assign "Y" if data exists
    TRUE ~ NA_character_  # Leave as NA otherwise
  ))

SSM20 <- SSM20 %>% 
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",
    TRUE ~ NA_character_
  ))

SST13 <- SST13 %>% 
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",
    TRUE ~ NA_character_
  ))

# Filter using "Grab_sample" column
grab_SSM01 = SSM01[SSM01$Grab_sample == "Y",] # Ony gets data when there is a Y
grab_SSM20 = SSM20[SSM20$Grab_sample == "Y",] # Ony gets data when there is a Y
grab_SST13 = SST13[SST13$Grab_sample == "Y",] # Ony gets data when there is a Y

grab.DOC01 = grab_SSM01$NPOC..mg.C.L.
grab.NO301 = grab_SSM01$NO3..mg.N.L.

grab.DOC20 = grab_SSM20$NPOC..mg.C.L.
grab.NO320 = grab_SSM20$NO3..mg.N.L.

grab.DOC13 = grab_SST13$NPOC..mg.C.L.
grab.NO313 = grab_SST13$NO3..mg.N.L.

# Compare grab vs scan DOC
plot(grab_SSM01$DOC_mg.l_clean ~ grab_SSM01$NPOC..mg.C.L.)
calib.mod.DOC01 = lm(grab_SSM01$DOC_mg.l_clean ~ grab_SSM01$NPOC..mg.C.L.)
summary(calib.mod.DOC01)

plot(grab_SSM20$DOC_mg.l_clean ~ grab_SSM20$NPOC..mg.C.L.)
calib.mod.DOC20 = lm(grab_SSM20$DOC_mg.l_clean ~ grab_SSM20$NPOC..mg.C.L.)
summary(calib.mod.DOC20)

plot(grab_SST13$DOC_mg.l_clean ~ grab_SST13$NPOC..mg.C.L.)
calib.mod.DOC13 = lm(grab_SST13$DOC_mg.l_clean ~ grab_SST13$NPOC..mg.C.L.)
summary(calib.mod.DOC13)

# Compare grab vs scan NO3
plot(grab_SSM01$NO3_mg.l_clean ~ grab_SSM01$NO3..mg.N.L.)
calib.mod.DOC01 = lm(grab_SSM01$NO3_mg.l_clean ~ grab_SSM01$NO3..mg.N.L.)
summary(calib.mod.DOC01)

plot(grab_SSM20$NO3_mg.l_clean ~ grab_SSM20$NO3..mg.N.L.)
calib.mod.DOC20 = lm(grab_SSM20$NO3_mg.l_clean ~ grab_SSM20$NO3..mg.N.L.)
summary(calib.mod.DOC20)

plot(grab_SST13$NO3_mg.l_clean ~ grab_SST13$NO3..mg.N.L.)
calib.mod.DOC13 = lm(grab_SST13$NO3_mg.l_clean ~ grab_SST13$NO3..mg.N.L.)
summary(calib.mod.DOC13)

#######################################################################################
#### STEP 4: Create matrices of GRAB spectral data - this is the training data set ####
#######################################################################################
# 1. Index data set with columns with absorbances
grab.spec.dat01 = grab_SSM01[20:230] # Full spectra, with no NAs
grab.spec.dat20 = grab_SSM20[22:231] # Full spectra, with no NAs
grab.spec.dat13 = grab_SST13[23:232] # Full spectra, with no NAs

# Rename columns for all data frames (e.g., SSM01, SSM20, SST13)
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}

# Apply the renaming to each data frame
grab.spec.dat01 <- rename_columns(grab.spec.dat01)
grab.spec.dat20 <- rename_columns(grab.spec.dat20)
grab.spec.dat13 <- rename_columns(grab.spec.dat13)

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
abs01 = (grab.spec.dat01)  # this is not doing anything and just copying grab.spec.dat01 again as abs01
abs20 = (grab.spec.dat20)
abs13 = (grab.spec.dat13)
#str(abs)

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wl01 = as.numeric(colnames(abs01))
wl20 = as.numeric(colnames(abs20))
wl13 = as.numeric(colnames(abs13))
str(wl13)

# 4. Create a vector with sample labels that match the absorbance matrix rows. 
lastrow01 = as.numeric(nrow(abs01))
Num01 = c(1:lastrow01)

lastrow20 = as.numeric(nrow(abs20))
Num20 = c(1:lastrow20)

lastrow13 = as.numeric(nrow(abs13))
Num13 = c(1:lastrow13)

# 5. Create the final matrix 
grab.matrix01 = cbind(abs01) # this is not binding anything and just copying abs01 again as grab.matrix01 
rownames(grab.matrix01) = as.numeric(Num01)
colnames(grab.matrix01) = as.numeric(wl01)
grab.matrix01 = as.matrix(grab.matrix01)
str(grab.matrix01)
attributes(grab.matrix01)

grab.matrix20 = cbind(abs20)
rownames(grab.matrix20) = as.numeric(Num20)
colnames(grab.matrix20) = as.numeric(wl20)
grab.matrix20 = as.matrix(grab.matrix20)
str(grab.matrix20)
attributes(grab.matrix20)

grab.matrix13 = cbind(abs13)
rownames(grab.matrix13) = as.numeric(Num13)
colnames(grab.matrix13) = as.numeric(wl13)
grab.matrix13 = as.matrix(grab.matrix13)
str(grab.matrix13)
attributes(grab.matrix13)

# 6. Make this into spectral matrix for model
# Must be in format: grab.spectra = spectra(value = abs, bands = wl, names = Num)
grab.spectra01 = spectra(value = abs01, bands = wl01, names = Num01)
attributes(grab.spectra01)
plot(grab.spectra01) # Note, bands here = absorbance from the scans

grab.spectra20 = spectra(value = abs20, bands = wl20, names = Num20)
attributes(grab.spectra20)
plot(grab.spectra20) # Note, bands here = absorbance from the scans

grab.spectra13 = spectra(value = abs13, bands = wl13, names = Num13)
attributes(grab.spectra13)
plot(grab.spectra13) # Note, bands here = absorbance from the scans

#grab.spectra = as_spectra.list(grab.spectra, wave_unit = "wavenumber", measurement_nit = "absorbance")

grab.spectra01 = as.matrix(grab.spectra01)
grab.spectra20 = as.matrix(grab.spectra20)
grab.spectra13 = as.matrix(grab.spectra13)
#str(grab.spectra)

# Change attributes so this is correct for scan data
attr(grab.spectra01, 'wave_unit') = 'wavelength'
attr(grab.spectra01, 'measurement_unit') = 'absorbance'
attributes(grab.spectra01)

attr(grab.spectra20, 'wave_unit') = 'wavelength'
attr(grab.spectra20, 'measurement_unit') = 'absorbance'
attributes(grab.spectra20)

attr(grab.spectra13, 'wave_unit') = 'wavelength'
attr(grab.spectra13, 'measurement_unit') = 'absorbance'
attributes(grab.spectra13)

########################################################################################
#### STEP 5: Create matrices of ALL spectral data - raw data that needs calibration ####
########################################################################################

# 1. Index FULL dataset with columns with absorbances
scan.spec01 = SSM01[13:230]
scan.spec20 = SSM20[13:231]
scan.spec13 = SST13[25:234]

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

#SST13
scan.matrix13 = cbind(abs13)
rownames(scan.matrix13) = as.numeric(Num13)
colnames(scan.matrix13) = as.numeric(wl13)

scan.matrix13 = as.matrix(scan.matrix13)
spec13 = spectra(value = abs13, bands = wl13, names = Num13)
plot(spec13) # Note = reflectance here = absorbance from the scans

# NOTE: this is where you can identify problem spectra & remove them

# = as.spectra.list(spec)
scan.spectra01 = as.matrix(spec01)
str(scan.spectra01)
attr(scan.spectra01, 'wave_unit') = 'wavelength'
attr(scan.spectra01, 'measurement_unit') = 'absorbance'
attributes(scan.spectra01)

scan.spectra20 = as.matrix(spec20)
str(scan.spectra20)
attr(scan.spectra20, 'wave_unit') = 'wavelength'
attr(scan.spectra20, 'measurement_unit') = 'absorbance'
attributes(scan.spectra20)

scan.spectra13 = as.matrix(spec13)
str(scan.spectra13)
attr(scan.spectra13, 'wave_unit') = 'wavelength'
attr(scan.spectra13, 'measurement_unit') = 'absorbance'
attributes(scan.spectra13)

####################################################################
#### STEP 6: Create a new data frame with the spectral matrices ####
####################################################################
# This creates a data frame with 
# 1. DOC (scan)
# 2. TSS (scab)
# 3. Full s::can spectra (from 220-750nm)

length(scan_DOC_SSM01)
length(scan_NO3_SSM01)
dim(scan.spectra01) 
class(scan.spectra01)

# NOTE: We use the I() function to protect the Spectra 
spectralcal.df01 = data.frame(DOC01 = scan_DOC_SSM01, NO301 = scan_NO3_SSM01, Spectra01 = I(scan.spectra01))
str(spectralcal.df01)

spectralcal.df20 = data.frame(DOC20 = scan_DOC_SSM20, NO320 = scan_NO3_SSM20, Spectra20 = I(scan.spectra20))
str(spectralcal.df20)

spectralcal.df13 = data.frame(DOC13 = scan_DOC_SST13, NO313 = scan_NO3_SST13, Spectra13 = I(scan.spectra13))
str(spectralcal.df13)

# Also do this for the GRAB sample data
grabcal.df01 = data.frame(DOC01 = grab.DOC01, NO301 = grab.NO301, Spectra01 = I(grab.spectra01))
str(grabcal.df01)

grabcal.df20 = data.frame(DOC20 = grab.DOC20, NO320 = grab.NO320, Spectra20 = I(grab.spectra20))
str(grabcal.df20)

grabcal.df13 = data.frame(DOC13 = grab.DOC13, NO313 = grab.NO313, Spectra13 = I(grab.spectra13))
str(grabcal.df13)

#################################################
#### STEP 7: Develop PLSR training data sets ####
#################################################

# Create a training and test dataset
# Carbon
CTrain01 = grabcal.df01
CTest01 = spectralcal.df01

# NO3
NTrain01 = grabcal.df01
NTest01 = spectralcal.df01

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
Cmod01 = plsr(DOC01 ~ Spectra01, ncomp = 3, data = CTrain01, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod01) # optimized for 4 components

Nmod01 = plsr(NO301 ~ Spectra01, ncomp = 2, data = NTrain01, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod01)

# Plot RMSE of the predictions to optimize model
plot(RMSEP(Cmod01), legendpos = "topright")
plot(RMSEP(Nmod01), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error (in this case, 1)
# NOTE: This plot may be messy, given low number of grab samples 
plot(Cmod01, ncomp = 2, asp = 1, line = TRUE)
plot(Nmod01, ncomp = 1, asp = 1, line = TRUE)

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################

# Predict model!
predictedC01 = predict(Cmod01, ncomp = 2, newdata = spectralcal.df01) # use reduced error model
str(predictedC01)
plot(predictedC01)

predictedN01 = predict(Nmod01, ncomp = 1, newdata = spectralcal.df01) # use reduced error model
str(predictedN01)
# Plot final predictions
plot(predictedN01)

write.csv(predictedC01, file = "PredictedC_SSM01.csv") # <- this is your newly calibrated dataset!
write.csv(predictedN01, file = "PredictedN_SSM01.csv") # <- this is your newly calibrated dataset!


## NOTE: If your s::can has significant drift (e.g., which often happens when there is biofouling), 
# You might need to use a moving window approach to the calibraiton (i.e., calibrate 1 month at a time)

#################################################
#### STEP 7: Develop PLSR training data sets ####
#################################################

# Create a training and test dataset
# Carbon
CTrain20 = grabcal.df20
CTest20 = spectralcal.df20

# NO3
NTrain20 = grabcal.df20
NTest20 = spectralcal.df20

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
Cmod20 = plsr(DOC20 ~ Spectra20, ncomp = 3, data = CTrain20, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod20) # optimized for 4 components

Nmod20 = plsr(NO320 ~ Spectra20, ncomp = 3, data = NTrain20, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod20)

# Plot RMSE of the predictions to optimize model
plot(RMSEP(Cmod20), legendpos = "topright")
plot(RMSEP(Nmod20), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error (in this case, 1)
# NOTE: This plot may be messy, given low number of grab samples 
plot(Cmod20, ncomp = 2, asp = 1, line = TRUE)
plot(Nmod20, ncomp = 1, asp = 1, line = TRUE)

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################

# Predict model!
predictedC20 = predict(Cmod20, ncomp = 1, newdata = spectralcal.df20) # use reduced error model
str(predictedC20)
plot(predictedC20)

predictedN20 = predict(Nmod20, ncomp = 1, newdata = spectralcal.df20) # use reduced error model
str(predictedN20)
# Plot final predictions
plot(predictedN20)

write.csv(predictedC20, file = "PredictedC_SSM20.csv") # <- this is your newly calibrated dataset!
write.csv(predictedN20, file = "PredictedN_SSM20.csv") # <- this is your newly calibrated dataset!

# Invert it?
#predictedC20 <- read.csv("predicted/PredictedC_SSM20.csv")
#predictedC20_rev <- predictedC20

#predictedC20_rev$DOC20_rev <- -predictedC20_rev$DOC.compensated

# Check the result
#head(predictedC20_rev)
#write.csv(predictedC20_rev, file = "PredictedC_SSM20_inv.csv") 

## NOTE: If your s::can has significant drift (e.g., which often happens when there is biofouling), 
# You might need to use a moving window approach to the calibraiton (i.e., calibrate 1 month at a time)
# This is a bit more complicated, so start with this simple calibration first. 

#################################################
#### STEP 7: Develop PLSR training data sets ####
#################################################

# Create a training and test dataset
# Carbon
CTrain13 = grabcal.df13
CTest13 = spectralcal.df13

# NO3
NTrain13 = grabcal.df13
NTest13 = spectralcal.df13

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
Cmod13 = plsr(DOC13 ~ Spectra13, ncomp = 2, data = CTrain13, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod13) # optimized for 4 components

Nmod13 = plsr(NO313 ~ Spectra13, ncomp = 3, data = NTrain13, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod13)

# Plot RMSE of the predictions to optimize model
plot(RMSEP(Cmod13), legendpos = "topright")
plot(RMSEP(Nmod13), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error (in this case, 1)
# NOTE: This plot may be messy, given low number of grab samples 
plot(Cmod13, ncomp = 2, asp = 1, line = TRUE)
plot(Nmod13, ncomp = 1, asp = 1, line = TRUE)

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################

# Predict model!
predictedC13 = predict(Cmod13, ncomp = 2, newdata = spectralcal.df13) # use reduced error model
str(predictedC13)
plot(predictedC13)

head(predictedC20)
predictedT13 = predict(Nmod13, ncomp = 1, newdata = spectralcal.df13) # use reduced error model
str(predictedN13)
# Plot final predictions
plot(predictedN13)

write.csv(predictedC, file = "PredictedC_SST13.csv") # <- this is your newly calibrated dataset!
write.csv(predictedN, file = "PredictedN_SST13.csv") # <- this is your newly calibrated dataset!


## NOTE: If your s::can has significant drift (e.g., which often happens when there is biofouling), 
# You might need to use a moving window approach to the calibraiton (i.e., calibrate 1 month at a time)
# This is a bit more complicated, so start with this simple calibration first. 