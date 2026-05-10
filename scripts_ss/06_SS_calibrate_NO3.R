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
library(prospectr) #to preprocess spectroscopic data 

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

# Let's load them separately first
SSM01 <- read.csv("googledrive/SSM01_merged.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
SSM20 <- read.csv("googledrive/SSM20_merged.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
SST13 <- read.csv("googledrive/SST13_merged.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)

# DateTime at midnight is missing 00:00:00 time, so filling in using grep
SSM01$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SSM01$DateTime)] <- paste(
  SSM01$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SSM01$DateTime)],"00:00:00")
SSM20$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SSM20$DateTime)] <- paste(
  SSM20$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SSM20$DateTime)],"00:00:00")
SST13$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SST13$DateTime)] <- paste(
  SST13$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SST13$DateTime)],"00:00:00")

# Convert the DateTime column to POSIXct
SSM01$DateTime <- as.POSIXct(SSM01$DateTime, format = "%Y-%m-%d %H:%M:%S")
SSM20$DateTime <- as.POSIXct(SSM20$DateTime, format = "%Y-%m-%d %H:%M:%S")
SST13$DateTime <- as.POSIXct(SST13$DateTime, format = "%Y-%m-%d %H:%M:%S")

# Remove NAs from DateTime column
SSM01 <- SSM01 %>%
  filter(!is.na(DateTime))
SSM20 <- SSM20 %>%
  filter(!is.na(DateTime))
SST13 <- SST13 %>%
  filter(!is.na(DateTime))

# Rename columns by removing the X in front of the spectra (that brakes the code somehow)
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}
# Apply the renaming to each data frame
SSM01 <- rename_columns(SSM01)
SSM20 <- rename_columns(SSM20)
SST13 <- rename_columns(SST13)

# Extract NO3N data as time series objects (xts)
scan_NO3N_SSM01 <- xts(SSM01$NO3.N_mg.l, order.by = SSM01$DateTime)
scan_NO3N_SSM20 <- xts(SSM20$NO3.N_mg.l, order.by = SSM20$DateTime)
scan_NO3N_SST13 <- xts(SST13$NO3.N_mg.l, order.by = SST13$DateTime)

# Extract spectral data (assuming spectral columns are in range "135.00.nm" to "400.00.nm")
scan.specSSM01 = xts(SSM01[18:127], as.POSIXct(SSM01$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.specSSM20 = xts(SSM20[18:127], as.POSIXct(SSM20$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.specSST13 = xts(SST13[18:130], as.POSIXct(SST13$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
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

grab.NO3N01 = grab_SSM01$NO3..mg.N.L.
grab.NO3N20 = grab_SSM20$NO3..mg.N.L.
grab.NO3N13 = grab_SST13$NO3..mg.N.L.

#### remove a couple of problematic samples ####
# grab_SSM01 <- grab_SSM01 %>%
#   mutate(NO3..mg.N.L. = ifelse(Date %in% c("2024-09-25", "2025-05-01", "2025-04-10", "2025-04-03") | is.na(NO3..mg.N.L.), NA, NO3..mg.N.L.))
# grab_SSM20 <- grab_SSM20 %>%
#   mutate(NO3..mg.N.L. = ifelse(Date %in% c("2024-05-23", "2025-09-01", "2025-05-01", "2024-10-16", "2025-04-10", "2024-07-30") | is.na(NO3..mg.N.L.),NA,NO3..mg.N.L.))
# grab_SST13 <- grab_SST13 %>%
#   mutate(NO3..mg.N.L. = ifelse(Date %in% c("2024-09-18", "2025-06-13", "2025-09-01") | is.na(NO3..mg.N.L.), NA, NO3..mg.N.L.))

# compare grab vs scan NO3N
plot(grab_SSM01$NO3.N_mg.l ~ grab_SSM01$NO3..mg.N.L.)
ggplot(grab_SSM01, aes(x = NO3..mg.N.L., y = NO3.N_mg.l)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.NO3N01 = lm(grab_SSM01$NO3.N_mg.l ~ grab_SSM01$NO3..mg.N.L.)
summary(calib.mod.NO3N01)

plot(grab_SSM20$NO3.N_mg.l ~ grab_SSM20$NO3..mg.N.L.)
ggplot(grab_SSM20, aes(x = NO3..mg.N.L., y = NO3.N_mg.l)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.NO3N20 = lm(grab_SSM20$NO3.N_mg.l ~ grab_SSM20$NO3..mg.N.L.)
summary(calib.mod.NO3N20)

plot(grab_SST13$NO3.N_mg.l ~ grab_SST13$NO3..mg.N.L.)
ggplot(grab_SST13, aes(x = NO3..mg.N.L., y = NO3.N_mg.l)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.NO3N13 = lm(grab_SST13$NO3.N_mg.l ~ grab_SST13$NO3..mg.N.L.)
summary(calib.mod.NO3N13)

#######################################################################################
#### STEP 4: Create matrices of GRAB spectral data - this is the training data set ####
#######################################################################################
# 1. Index data set with columns with absorbances
# raw spectra
grab.spec.dat01 = grab_SSM01[18:127]
grab.spec.dat20 = grab_SSM20[18:127]
grab.spec.dat13 = grab_SST13[18:130] 

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
abs13 = (grab.spec.datSST13)
#str(abs)

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wl01 <- gsub("_clean", "", colnames(abs01))   
wl01 <- as.numeric(wl01)
wl20 <- gsub("_clean", "", colnames(abs20))   
wl20 <- as.numeric(wl20)
wl13 <- gsub("_clean", "", colnames(abs13))   
wl13 <- as.numeric(wl13)
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
# raw spectra
scan.spec01 = SSM01[18:127]
scan.spec20 = SSM20[18:127] 
scan.spec13 = SST13[18:130]

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
abs01 = (scan.spec01)
abs20 = (scan.spec20) 
abs13 = (scan.spec13) 

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wl01 <- gsub("_clean", "", colnames(abs01))   
wl01 <- as.numeric(wl01)
wl20 <- gsub("_clean", "", colnames(abs20))   
wl20 <- as.numeric(wl20)
wl13 <- gsub("_clean", "", colnames(abs13))   
wl13 <- as.numeric(wl13)

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
length(scan_NO3N_SSM01)
dim(scan.spectra01) 
class(scan.spectra01)

# NOTE: We use the I() function to protect the Spectra 
spectralcal.df01 = data.frame(NO3N01 = scan_NO3N_SSM01, Spectra01 = I(scan.spectra01))
str(spectralcal.df01)

spectralcal.df20 = data.frame(NO3N20 = scan_NO3N_SSM20, Spectra20 = I(scan.spectra20))
str(spectralcal.df20)

spectralcal.df13 = data.frame(NO3N13 = scan_NO3N_SST13, Spectra13 = I(scan.spectra13))
str(spectralcal.df13)

# Also do this for the GRAB sample data
grabcal.df01 = data.frame(NO3N01 = grab.NO3N01, Spectra01 = I(grab.spectra01))
str(grabcal.df01)

grabcal.df20 = data.frame(NO3N20 = grab.NO3N20, Spectra20 = I(grab.spectra20))
str(grabcal.df20)

grabcal.df13 = data.frame(NO3N13 = grab.NO3N13, Spectra13 = I(grab.spectra13))
str(grabcal.df13)

#################################################
#### STEP 7: Develop PLSR training data sets ####
#################################################
# Create a training and test data set
# NO3N
NTrain01 = grabcal.df01
NTest01 = spectralcal.df01

##########
NTrain01 = grabcal.df01
NTest01 = spectralcal.df01

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
Nmod01 = plsr(NO3N01 ~ Spectra01, ncomp = 7, data = NTrain01, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Nmod01)

# Plot RMSE of the predictions to optimize model
plot(RMSEP(Nmod01), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error
plot(Nmod01, ncomp = 2, asp = 1, line = TRUE)

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################
# predictedN01 = predict(Nmod01, ncomp = 2, newdata = spectralcal.df01) # use reduced error model
# str(predictedN01)
# # Plot final predictions
# plot(predictedN01)

write.csv(predictedN01_SNV, file = "predicted/PredictedN_SSM01_SNV.csv") # <- this is your newly calibrated dataset!

## NOTE: If your s::can has significant drift (e.g., which often happens when there is biofouling), 
# You might need to use a moving window approach to the calibraiton (i.e., calibrate 1 month at a time)

#################################################
#### STEP 7: Develop PLSR training data sets ####
#################################################
# NO3N
NTrain20 = grabcal.df20
NTest20 = spectralcal.df20

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
Nmod20 = plsr(NO3N20 ~ Spectra20, ncomp = 2, data = NTrain20, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod20)

# Plot RMSE of the predictions to optimize model
plot(RMSEP(Nmod20), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error (in this case, x)
# NOTE: This plot may be messy, given low number of grab samples 
plot(Nmod20, ncomp = 2, asp = 1, line = TRUE)

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################
# Predict model!
predictedN20 = predict(Nmod20, ncomp = 2, newdata = spectralcal.df20) # use reduced error model
str(predictedN20)
# Plot final predictions
plot(predictedN20)

write.csv(predictedN20, file = "predicted/PredictedN_SSM20_vclean.csv") # <- this is your newly calibrated dataset!

## NOTE: If your s::can has significant drift (e.g., which often happens when there is biofouling), 
# You might need to use a moving window approach to the calibraiton (i.e., calibrate 1 month at a time)
# This is a bit more complicated, so start with this simple calibration first. 

# Convert predictedN20 to a data frame
pred_df <- data.frame(
  DateTime = as.POSIXct(dimnames(predictedN20)[[1]]),
  Predicted = as.numeric(predictedN20))
# Plot
ggplot(pred_df, aes(x = DateTime, y = Predicted)) +
  geom_point(color = "steelblue") +
  labs(
    x = "DateTime",
    y = "Predicted NO3N (mg/L)",
    title = "Predicted NO3N over Time (SSM20)"
  ) +
  theme_minimal()

#################################################
#### STEP 7: Develop PLSR training data sets ####
#################################################
# Create a training and test dataset
# NO3N
NTrain13 = grabcal.df13
NTest13 = spectralcal.df13

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
Nmod13 = plsr(NO3N13 ~ Spectra13, ncomp = 3, data = NTrain13, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Nmod13)

# Plot RMSE of the predictions to optimize model
plot(RMSEP(Nmod13), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error
# NOTE: This plot may be messy, given low number of grab samples 
plot(Nmod13, ncomp = 2, asp = 1, line = TRUE)

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################
predictedN13 = predict(Nmod13, ncomp = 2, newdata = spectralcal.df13) # use reduced error model
str(predictedN13)
# Plot
plot(predictedN13)

write.csv(predictedN13, file = "predicted/PredictedN_SST13_vclean.csv") # <- this is your newly calibrated dataset!

# 1. Loadings Plot for SSM01 (Opposite Trend)
# This shows how the wavelengths contribute to each component (ncomp = 1, 2, 3, etc.)
plot(Nmod01, plottype = "loading",
     comps = 1:2, # Plot the first two components for initial inspection
     main = "SSM01 NO3-N PLSR Loadings")

# 2. Loadings Plot for SSM20 (Flat Trend)
plot(Nmod20, plottype = "loading",
     comps = 1:2, # Plot the first two components
     main = "SSM20 NO3-N PLSR Loadings")

# 3. Loadings Plot for SST13 (Flat Trend)
# Examine the first few components for SST13
plot(Nmod13, plottype = "loading",
     comps = 1:2, # Plot the first two components
     main = "SST13 NO3-N PLSR Loadings")

# Convert predictedN13 to a data frame
pred_df <- data.frame(
  DateTime = as.POSIXct(dimnames(predictedN13)[[1]]),
  Predicted = as.numeric(predictedN13))
# Plot
p <- ggplot(pred_df, aes(x = DateTime, y = Predicted)) +
  geom_point(color = "steelblue") +
  labs(
    x = "DateTime",
    y = "Predicted NO3N (mg/L)",
    title = "Predicted NO3N over Time (SST13)"
  ) +
  theme_minimal()
ggplotly(p)

#######################
#### Save in Drive #### 
#######################
# Define the target folder ID in Google Drive
# This is the "predicted" folder
drive_folder_id <- "1wa1ycqUYv56y3fTn1-VaN2K-NLU3rFeU"

drive_upload(media = "predicted/PredictedN_SSM01_vclean.csv", path = as_id(drive_folder_id))
drive_upload(media = "predicted/PredictedN_SSM20_vclean.csv", path = as_id(drive_folder_id))
drive_upload(media = "predicted/PredictedN_SST13_vclean.csv", path = as_id(drive_folder_id))

drive_upload(media = "predicted/PredictedN_SSM01_SNV.csv", path = as_id(drive_folder_id))



## NOTE: If your s::can has significant drift (e.g., which often happens when there is biofouling), 
# You might need to use a moving window approach to the calibraiton (i.e., calibrate 1 month at a time)
# This is a bit more complicated, so start with this simple calibration first. 