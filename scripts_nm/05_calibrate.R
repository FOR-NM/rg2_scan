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
library(ggplot2)

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

######################################################
#### STEP 2: Upload scan data frame [with spectra] ####
######################################################
# This is the "with chem" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1qjM3Zze-I5ycFCHNcd997UG6gYXBUoX8")

# List all xlsx files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")
3

#USF12
googledrive::drive_download(file = merged$id[merged$name=="USF12_chem_Buttercup.csv"], 
                            path = "googledrive/USF12_chem_Buttercup.csv",
                            overwrite = T)
#USF20
googledrive::drive_download(file = merged$id[merged$name=="USF20_chem_Blossom.csv"], 
                            path = "googledrive/USF20_chem_Blossom.csv",
                            overwrite = T)
#USF21
googledrive::drive_download(file = merged$id[merged$name=="USF21_chem_Bubbles.csv"], 
                            path = "googledrive/USF21_chem_Bubbles.csv",
                            overwrite = T)

# Let's load them separately first
USF12 <- read.csv("googledrive/USF12_chem_Buttercup.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
USF20 <- read.csv("googledrive/USF20_chem_Blossom.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
USF21 <- read.csv("googledrive/USF21_chem_Bubbles.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)

# # DateTime at midnight is missing 00:00:00 time, so filling in that time using grep                     
# USF12$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF12$DateTime)] <- paste(
#   USF12$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF12$DateTime)],"00:00:00")
# USF20$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF20$DateTime)] <- paste(
#   USF20$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF20$DateTime)],"00:00:00")
# USF21$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF21$DateTime)] <- paste(
#   USF21$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF21$DateTime)],"00:00:00")

# Convert the DateTime column to POSIXct
USF12$DateTime <- as.POSIXct(USF12$DateTime, format = "%Y-%m-%d %H:%M:%S")
USF20$DateTime <- as.POSIXct(USF20$DateTime, format = "%Y-%m-%d %H:%M:%S")
USF21$DateTime <- as.POSIXct(USF21$DateTime, format = "%Y-%m-%d %H:%M:%S")

# Rename columns by removing the X in front of the spectra (that brakes the code somehow)
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}
# Apply the renaming to each data frame
USF12 <- rename_columns(USF12)
USF20 <- rename_columns(USF20)
USF21 <- rename_columns(USF21)

# USF12 <- USF12[-c(45201:45235),]
# USF20 <- USF20[-c(33137:33367),]
USF21 <- USF21[-c(19675:19771),]

# Extract DOC NO3 NO3N and TSS data as time series objects (xts)
scan_DOC_USF12 <- xts(USF12$DOC_mg.l , order.by = USF12$DateTime)
scan_TSS_USF12 <- xts(USF12$TSS_clean, order.by = USF12$DateTime)
scan_NO3N_USF12 <- xts(USF12$NO3..mg.N.L., order.by = USF12$DateTime)
scan_NO3_USF12 <- xts(USF12$NO3_mg.l, order.by = USF12$DateTime)

scan_DOC_USF20 <- xts(USF20$DOC_mg.l, order.by = USF20$DateTime)
scan_TSS_USF20 <- xts(USF20$TSS_clean, order.by = USF20$DateTime)
scan_NO3N_USF20 <- xts(USF20$NO3..mg.N.L., order.by = USF20$DateTime)
scan_NO3_USF20 <- xts(USF20$NO3_mg.l, order.by = USF20$DateTime)

scan_DOC_USF21 <- xts(USF21$DOC_mg.l, order.by = USF21$DateTime)
scan_TSS_USF21 <- xts(USF21$TSS_clean, order.by = USF21$DateTime)
scan_NO3N_USF21 <- xts(USF21$NO3..mg.N.L., order.by = USF21$DateTime)
scan_NO3_USF21 <- xts(USF21$NO3_mg.l, order.by = USF21$DateTime)

# Extract spectral data (assuming spectral columns are in range "200.00.nm" to "750.00.nm")
scan.spec12 = xts(USF12[27:227], as.POSIXct(USF12$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.spec20 = xts(USF20[23:223], as.POSIXct(USF20$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.spec21 = xts(USF21[27:227], as.POSIXct(USF21$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
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
USF12 <- USF12 %>% 
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",  # Assign "Y" if data exists
    TRUE ~ NA_character_  # Leave as NA otherwise
  ))

USF20 <- USF20 %>% 
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",
    TRUE ~ NA_character_
  ))

USF21 <- USF21 %>% 
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",
    TRUE ~ NA_character_
  ))

# Filter using "Grab_sample" column
grab_USF12 = USF12[USF12$Grab_sample == "Y",] # Ony gets data when there is a Y
grab_USF20 = USF20[USF20$Grab_sample == "Y",] # Ony gets data when there is a Y
grab_USF21 = USF21[USF21$Grab_sample == "Y",] # Ony gets data when there is a Y

grab.DOC12 = grab_USF12$NPOC..mg.C.L.
grab.NO312 = grab_USF12$NO3..mg.N.L.

grab.DOC20 = grab_USF20$NPOC..mg.C.L.
grab.NO320 = grab_USF20$NO3..mg.N.L.

grab.DOC21 = grab_USF21$NPOC..mg.C.L.
grab.NO321 = grab_USF21$NO3..mg.N.L.

# Compare grab vs scan DOC
plot(grab_USF12$DOC_mg.l ~ grab_USF12$NPOC..mg.C.L.)
ggplot(grab_USF12, aes(x = NPOC..mg.C.L., y = DOC_mg.l)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOC12 = lm(grab_USF12$DOC_mg.l ~ grab_USF12$NPOC..mg.C.L.)
summary(calib.mod.DOC12)

ggplot(grab_USF20, aes(x = NPOC..mg.C.L., y = DOC_mg.l)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOC20 = lm(grab_USF20$DOC_mg.l ~ grab_USF20$NPOC..mg.C.L.)
summary(calib.mod.DOC20)

ggplot(grab_USF21, aes(x = NPOC..mg.C.L., y = DOC_mg.l)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOC21 = lm(grab_USF21$DOC_mg.l ~ grab_USF21$NPOC..mg.C.L.)
summary(calib.mod.DOC21)

# Compare grab vs scan NO3
plot(grab_USF12$NO3_mg.l ~ grab_USF12$NO3..mg.N.L.)
ggplot(grab_USF12, aes(x = NO3..mg.N.L., y = NO3_mg.l)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOC12 = lm(grab_USF12$NO3_mg.l ~ grab_USF12$NO3..mg.N.L.)
summary(calib.mod.DOC12)

plot(grab_USF20$NO3_mg.l ~ grab_USF20$NO3..mg.N.L.)
ggplot(grab_USF20, aes(x = NO3..mg.N.L., y = NO3_mg.l)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOC20 = lm(grab_USF20$NO3_mg.l ~ grab_USF20$NO3..mg.N.L.)
summary(calib.mod.DOC20)

plot(grab_USF21$NO3_mg.l ~ grab_USF21$NO3..mg.N.L.)
ggplot(grab_USF21, aes(x = NO3..mg.N.L., y = NO3_mg.l)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOC21 = lm(grab_USF21$NO3_mg.l ~ grab_USF21$NO3..mg.N.L.)
summary(calib.mod.DOC21)

#######################################################################################
#### STEP 4: Create matrices of GRAB spectral data - this is the training data set ####
#######################################################################################
# 1. Index data set with columns with absorbances
# raw spectra
grab.spec.dat12 = grab_USF12[27:227] # Full spectra, with no NAs?
grab.spec.dat20 = grab_USF20[23:223]
grab.spec.dat21 = grab_USF21[27:227] 
# clean spectra
# grab.spec.dat12 = grab_USF12[243:443] # Full spectra, with no NAs?
# grab.spec.dat20 = grab_USF20[241:441]
# grab.spec.dat21 = grab_USF21[243:443] 

# Rename columns for all data frames (e.g., USF12, USF20, USF21)
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}

# Apply the renaming to each data frame
grab.spec.dat12 <- rename_columns(grab.spec.dat12)
grab.spec.dat20 <- rename_columns(grab.spec.dat20)
grab.spec.dat21 <- rename_columns(grab.spec.dat21)

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
abs12 = (grab.spec.dat12)  # this is not doing anything and just copying grab.spec.dat12 again as abs12
abs20 = (grab.spec.dat20)
abs21 = (grab.spec.dat21)
#str(abs)

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wl12 <- gsub("_clean", "", colnames(abs12))   
wl12 <- as.numeric(wl12)
wl20 <- gsub("_clean", "", colnames(abs20))   
wl20 <- as.numeric(wl20)
wl21 <- gsub("_clean", "", colnames(abs21))   
wl21 <- as.numeric(wl21)
str(wl21)

# 4. Create a vector with sample labels that match the absorbance matrix rows. 
lastrow12 = as.numeric(nrow(abs12))
Num12 = c(1:lastrow12)

lastrow20 = as.numeric(nrow(abs20))
Num20 = c(1:lastrow20)

lastrow21 = as.numeric(nrow(abs21))
Num21 = c(1:lastrow21)

# 5. Create the final matrix 
grab.matrix12 = cbind(abs12) # this is not binding anything and just copying abs12 again as grab.matrix12 
rownames(grab.matrix12) = as.numeric(Num12)
colnames(grab.matrix12) = as.numeric(wl12)
grab.matrix12 = as.matrix(grab.matrix12)
str(grab.matrix12)
attributes(grab.matrix12)

grab.matrix20 = cbind(abs20)
rownames(grab.matrix20) = as.numeric(Num20)
colnames(grab.matrix20) = as.numeric(wl20)
grab.matrix20 = as.matrix(grab.matrix20)
str(grab.matrix20)
attributes(grab.matrix20)

grab.matrix21 = cbind(abs21)
rownames(grab.matrix21) = as.numeric(Num21)
colnames(grab.matrix21) = as.numeric(wl21)
grab.matrix21 = as.matrix(grab.matrix21)
str(grab.matrix21)
attributes(grab.matrix21)

# 6. Make this into spectral matrix for model
# Must be in format: grab.spectra = spectra(value = abs, bands = wl, names = Num)
grab.spectra12 = spectra(value = abs12, bands = wl12, names = Num12)
attributes(grab.spectra12)
plot(grab.spectra12) # Note, bands here = absorbance from the scans

grab.spectra20 = spectra(value = abs20, bands = wl20, names = Num20)
attributes(grab.spectra20)
plot(grab.spectra20) # Note, bands here = absorbance from the scans

grab.spectra21 = spectra(value = abs21, bands = wl21, names = Num21)
attributes(grab.spectra21)
plot(grab.spectra21) # Note, bands here = absorbance from the scans

#grab.spectra = as_spectra.list(grab.spectra, wave_unit = "wavenumber", measurement_nit = "absorbance")
grab.spectra12 = as.matrix(grab.spectra12)
grab.spectra20 = as.matrix(grab.spectra20)
grab.spectra21 = as.matrix(grab.spectra21)
#str(grab.spectra)

# Change attributes so this is correct for scan data
attr(grab.spectra12, 'wave_unit') = 'wavelength'
attr(grab.spectra12, 'measurement_unit') = 'absorbance'
attributes(grab.spectra12)

attr(grab.spectra20, 'wave_unit') = 'wavelength'
attr(grab.spectra20, 'measurement_unit') = 'absorbance'
attributes(grab.spectra20)

attr(grab.spectra21, 'wave_unit') = 'wavelength'
attr(grab.spectra21, 'measurement_unit') = 'absorbance'
attributes(grab.spectra21)

########################################################################################
#### STEP 5: Create matrices of ALL spectral data - raw data that needs calibration ####
########################################################################################
# 1. Index FULL dataset with columns with absorbances
# raw spectra
scan.spec12 = USF12[27:227]
scan.spec20 = USF20[23:223] 
scan.spec21 = USF21[27:227]
# clean spectra
# scan.spec12 = USF12[243:443]
# scan.spec20 = USF20[241:441] 
# scan.spec21 = USF21[243:443]

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
abs12 = (scan.spec12)
abs20 = (scan.spec20) 
abs21 = (scan.spec21) 

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wl12 <- gsub("_clean", "", colnames(abs12))   
wl12 <- as.numeric(wl12)
wl20 <- gsub("_clean", "", colnames(abs20))   
wl20 <- as.numeric(wl20)
wl21 <- gsub("_clean", "", colnames(abs21))   
wl21 <- as.numeric(wl21)

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

# NOTE: this is where you can identify problem spectra & remove them

# = as.spectra.list(spec)
scan.spectra12 = as.matrix(spec12)
str(scan.spectra12)
attr(scan.spectra12, 'wave_unit') = 'wavelength'
attr(scan.spectra12, 'measurement_unit') = 'absorbance'
attributes(scan.spectra12)

scan.spectra20 = as.matrix(spec20)
str(scan.spectra20)
attr(scan.spectra20, 'wave_unit') = 'wavelength'
attr(scan.spectra20, 'measurement_unit') = 'absorbance'
attributes(scan.spectra20)

scan.spectra21 = as.matrix(spec21)
str(scan.spectra21)
attr(scan.spectra21, 'wave_unit') = 'wavelength'
attr(scan.spectra21, 'measurement_unit') = 'absorbance'
attributes(scan.spectra21)

####################################################################
#### STEP 6: Create a new data frame with the spectral matrices ####
####################################################################
# This creates a data frame with 
# 1. DOC (scan)
# 2. TSS (scab)
# 3. Full s::can spectra (from 220-750nm)

length(scan_DOC_USF12)
length(scan_NO3_USF12)
dim(scan.spectra12) 
class(scan.spectra12)

# NOTE: We use the I() function to protect the Spectra 
spectralcal.df12 = data.frame(DOC12 = scan_DOC_USF12, NO312 = scan_NO3_USF12, Spectra12 = I(scan.spectra12))
str(spectralcal.df12)

spectralcal.df20 = data.frame(DOC20 = scan_DOC_USF20, NO320 = scan_NO3_USF20, Spectra20 = I(scan.spectra20))
str(spectralcal.df20)

spectralcal.df21 = data.frame(DOC21 = scan_DOC_USF21, NO321 = scan_NO3_USF21, Spectra21 = I(scan.spectra21))
str(spectralcal.df21)

# Also do this for the GRAB sample data
grabcal.df12 = data.frame(DOC12 = grab.DOC12, NO312 = grab.NO312, Spectra12 = I(grab.spectra12))
str(grabcal.df12)

grabcal.df20 = data.frame(DOC20 = grab.DOC20, NO320 = grab.NO320, Spectra20 = I(grab.spectra20))
str(grabcal.df20)

grabcal.df21 = data.frame(DOC21 = grab.DOC21, NO321 = grab.NO321, Spectra21 = I(grab.spectra21))
str(grabcal.df21)

#################################################
#### STEP 7: Develop PLSR training data sets ####
#################################################
# Create a training and test data set
# Carbon
CTrain12 = grabcal.df12
CTest12 = spectralcal.df12

# NO3
NTrain12 = grabcal.df12
NTest12 = spectralcal.df12

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
Cmod12 = plsr(DOC12 ~ Spectra12, ncomp = 10, data = CTrain12, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod12) # optimized for 4 components

Nmod12 = plsr(NO312 ~ Spectra12, ncomp = 10, data = NTrain12, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod12)

# Plot RMSE of the predictions to optimize model
plot(RMSEP(Cmod12), legendpos = "topright")
plot(RMSEP(Nmod12), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error (in this case, 2)
# NOTE: This plot may be messy, given low number of grab samples 
plot(Cmod12, ncomp = 2, asp = 1, line = TRUE)
plot(Nmod12, ncomp = 2, asp = 1, line = TRUE)

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################
# Predict model!
predictedC12 = predict(Cmod12, ncomp = 2, newdata = spectralcal.df12) # use reduced error model
str(predictedC12)
plot(predictedC12)

predictedN12 = predict(Nmod12, ncomp = 2, newdata = spectralcal.df12) # use reduced error model
str(predictedN12)
# Plot final predictions
plot(predictedN12)

write.csv(predictedC12, file = "predicted/PredictedC_USF12.csv") # <- this is your newly calibrated dataset!
write.csv(predictedN12, file = "predicted/PredictedN_USF12.csv") # <- this is your newly calibrated dataset!

# Convert predictedC12 to a data frame
pred_df <- data.frame(
  DateTime = as.POSIXct(dimnames(predictedC12)[[1]]),
  Predicted = as.numeric(predictedC12)
)

# Plot
ggplot(pred_df, aes(x = DateTime, y = Predicted)) +
  geom_point(color = "steelblue") +
  labs(
    x = "DateTime",
    y = "Predicted DOC (mg/L)",
    title = "Predicted DOC over Time (USF12)"
  ) +
  theme_minimal()

# Define your date range
start_date <- as.POSIXct("2025-06-01")
end_date   <- as.POSIXct("2025-09-05")

# Filter the predictions
pred_zoom <- pred_df %>%
  filter(DateTime >= start_date & DateTime <= end_date)

# Plot
ggplot(pred_zoom, aes(x = DateTime, y = Predicted)) +
  geom_line(color = "steelblue") +
  labs(
    x = "DateTime",
    y = "Predicted DOC (mg/L)",
    title = "Predicted DOC (USF12): June – Sep 2025"
  ) +
  theme_minimal()
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
Cmod20 = plsr(DOC20 ~ Spectra20, ncomp = 10, data = CTrain20, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod20) # optimized for 4 components

Nmod20 = plsr(NO320 ~ Spectra20, ncomp = 10, data = NTrain20, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod20)

# Plot RMSE of the predictions to optimize model
plot(RMSEP(Cmod20), legendpos = "topright")
plot(RMSEP(Nmod20), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error (in this case, x)
# NOTE: This plot may be messy, given low number of grab samples 
plot(Cmod20, ncomp = 3, asp = 1, line = TRUE)
plot(Nmod20, ncomp = 2, asp = 1, line = TRUE)

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################
# Predict model!
predictedC20 = predict(Cmod20, ncomp = 3, newdata = spectralcal.df20) # use reduced error model
str(predictedC20)
plot(predictedC20)

predictedN20 = predict(Nmod20, ncomp = 1, newdata = spectralcal.df20) # use reduced error model
str(predictedN20)
# Plot final predictions
plot(predictedN20)

write.csv(predictedC20, file = "predicted/PredictedC_USF20.csv") # <- this is your newly calibrated dataset!
write.csv(predictedN20, file = "predicted/PredictedN_USF20.csv") # <- this is your newly calibrated dataset!

# Invert it?
#predictedC20 <- read.csv("predicted/PredictedC_USF20.csv")
#predictedC20_rev <- predictedC20

#predictedC20_rev$DOC20_rev <- -predictedC20_rev$DOC.compensated

# Check the result
#head(predictedC20_rev)
#write.csv(predictedC20_rev, file = "PredictedC_USF20_inv.csv") 

## NOTE: If your s::can has significant drift (e.g., which often happens when there is biofouling), 
# You might need to use a moving window approach to the calibraiton (i.e., calibrate 1 month at a time)
# This is a bit more complicated, so start with this simple calibration first. 

#################################################
#### STEP 7: Develop PLSR training data sets ####
#################################################
# Create a training and test dataset
# Carbon
CTrain21 = grabcal.df21
CTest21 = spectralcal.df21

# NO3
NTrain21 = grabcal.df21
NTest21 = spectralcal.df21

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
Cmod21 = plsr(DOC21 ~ Spectra21, ncomp = 5, data = CTrain21, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod21) # optimized for 4 components

Nmod21 = plsr(NO321 ~ Spectra21, ncomp = 7, data = NTrain21, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod21)

# Plot RMSE of the predictions to optimize model
plot(RMSEP(Cmod21), legendpos = "topright")
plot(RMSEP(Nmod21), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error (in this case, 1)
# NOTE: This plot may be messy, given low number of grab samples 
plot(Cmod21, ncomp = 4, asp = 1, line = TRUE)
plot(Nmod21, ncomp = 2, asp = 1, line = TRUE)

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################
# Predict model!
predictedC21 = predict(Cmod21, ncomp = 4, newdata = spectralcal.df21) # use reduced error model
str(predictedC21)
plot(predictedC21)

head(predictedC21)
predictedN21 = predict(Nmod21, ncomp = 2, newdata = spectralcal.df21) # use reduced error model
str(predictedN21)
# Plot final predictions
plot(predictedN21)

write.csv(predictedC21, file = "predicted/PredictedC_USF21.csv") # <- this is your newly calibrated dataset!
write.csv(predictedN21, file = "predicted/PredictedN_USF21.csv") # <- this is your newly calibrated dataset!

#######################
#### Save in Drive #### 
#######################
# Define the target folder ID in Google Drive
# This is the "predicted" folder
drive_folder_id <- "1wa1ycqUYv56y3fTn1-VaN2K-NLU3rFeU"

# Upload the file to the specified Google Drive folder
drive_upload(media = "predicted/PredictedC_USF12.csv", path = as_id(drive_folder_id))
drive_upload(media = "predicted/PredictedC_USF20.csv", path = as_id(drive_folder_id))
drive_upload(media = "predicted/PredictedC_USF21.csv", path = as_id(drive_folder_id))

## NOTE: If your s::can has significant drift (e.g., which often happens when there is biofouling), 
# You might need to use a moving window approach to the calibraiton (i.e., calibrate 1 month at a time)
# This is a bit more complicated, so start with this simple calibration first. 