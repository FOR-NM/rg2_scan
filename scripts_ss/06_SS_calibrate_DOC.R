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
# This data is already matched #
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1Wju54VbyACZ_RFtfeInSvBCiVDKFScGj")

# List all CSVs files in the folder
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

# Extract DOC data as time series objects (xts)
scan_DOC_SSM01 <- xts(SSM01$DOC_mg.l , order.by = SSM01$DateTime)
scan_DOC_SSM20 <- xts(SSM20$DOC_mg.l, order.by = SSM20$DateTime)
scan_DOC_SST13 <- xts(SST13$DOC_mg.l, order.by = SST13$DateTime)

# Extract spectral data (assuming spectral columns are in range "SSM200.00.nm" to "4.00.nm")
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
# SSM01 <- SSM01[,-1]
# SSM20 <- SSM20[,-1]
# SST13 <- SST13[,-1]

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

grab.DOCSSM01 = grab_SSM01$NPOC..mg.C.L.
grab.DOCSSM20 = grab_SSM20$NPOC..mg.C.L.
grab.DOCSST13 = grab_SST13$NPOC..mg.C.L.

#### remove a couple of problematic samples ####
# grab_SSM01 <- grab_SSM01 %>%
#   mutate(NPOC..mg.C.L. = ifelse(DateTime == "2025-01-02 12:15:00" | is.na(NPOC..mg.C.L.),NA,NPOC..mg.C.L.))
# grab_SSM20 <- grab_SSM20 %>%
#   mutate(NPOC..mg.C.L. = ifelse(DateTime == "2024-06-19 14:00:00" | is.na(NPOC..mg.C.L.),NA,NPOC..mg.C.L.))

# compare grab vs scan DOC 
plot(grab_SSM01$DOC_mg.l ~ grab_SSM01$NPOC..mg.C.L.)
ggplot(grab_SSM01, aes(x = NPOC..mg.C.L., y = DOC_mg.l)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOCSSM01 = lm(grab_SSM01$DOC_mg.l ~ grab_SSM01$NPOC..mg.C.L.)
summary(calib.mod.DOCSSM01)

ggplot(grab_SSM20, aes(x = NPOC..mg.C.L., y = DOC_mg.l)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOCSSM20 = lm(grab_SSM20$DOC_mg.l ~ grab_SSM20$NPOC..mg.C.L.)
summary(calib.mod.DOCSSM20)

ggplot(grab_SST13, aes(x = NPOC..mg.C.L., y = DOC_mg.l)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOCSST13 = lm(grab_SST13$DOC_mg.l ~ grab_SST13$NPOC..mg.C.L.)
summary(calib.mod.DOCSST13)

#########################################
####  CLEANING FOR CRAZY WAVELENGTHS ####
#########################################
# filter Corrupted_No_Match FULL dataset
# 1. Identify spectral columns
spec_cols <- grep("^[0-9]", colnames(SSM01), value = TRUE)

# 2. Function to mask out SPECTRA for non-grab samples
# This keeps all 30,000 rows but turns the scan data to NA if Grab_sample isn't "Y"
mask_non_grabs <- function(df, site_name) {
  spec_cols <- grep("^[0-9]", colnames(df), value = TRUE)
  
  df_masked <- df %>%
    mutate(across(all_of(spec_cols), 
                  ~ifelse(Grab_sample == "Y", ., NA_real_))) %>%
    # Also ensure NPOC is only present when Grab_sample is Y
    mutate(NPOC_clean = ifelse(Grab_sample == "Y", NPOC..mg.C.L., NA_real_))
  
  message(paste("Site", site_name, ": Masked all rows except Grab Samples."))
  return(df_masked)
}

# 3. Apply the mask
SSM01_clean <- mask_non_grabs(SSM01, "SSM01")
SSM20_clean <- mask_non_grabs(SSM20, "SSM20")
SST13_clean <- mask_non_grabs(SST13, "SST13")

# 4. Verify the result
# Total rows should still be ~30,000
nrow(grab_SSM01_full)

# Number of rows with actual spectral data should be your grab sample count (e.g., 17)
sum(!is.na(grab_SSM01_full[[spec_cols[1]]]))

#######################################################################################
#### STEP 4: Create matrices of GRAB spectral data - this is the training data set ####
#######################################################################################
# 1. Index data set with columns with absorbances
# raw spectra
grab.spec.datSSM01 = SSM01_clean[18:127]
grab.spec.datSSM20 = SSM20_clean[18:127]
grab.spec.datSST13 = SST13_clean[18:130] 

# Rename columns for all data frames (e.g., SSM01, SSM20, SST13)
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}

# Apply the renaming to each data frame
grab.spec.datSSM01 <- rename_columns(grab.spec.datSSM01)
grab.spec.datSSM20 <- rename_columns(grab.spec.datSSM20)
grab.spec.datSST13 <- rename_columns(grab.spec.datSST13)

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
absSSM01 = (grab.spec.datSSM01)  # this is not doing anything and just copying grab.spec.datSSM01 again as absSSM01
absSSM20 = (grab.spec.datSSM20)
absSST13 = (grab.spec.datSST13)
#str(abs)

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wlSSM01 <- gsub("_clean", "", colnames(absSSM01))   
wlSSM01 <- as.numeric(wlSSM01)
wlSSM20 <- gsub("_clean", "", colnames(absSSM20))   
wlSSM20 <- as.numeric(wlSSM20)
wlSST13 <- gsub("_clean", "", colnames(absSST13))   
wlSST13 <- as.numeric(wlSST13)
str(wlSST13)

# 4. Create a vector with sample labels that match the absorbance matrix rows. 
lastrowSSM01 = as.numeric(nrow(absSSM01))
NumSSM01 = c(1:lastrowSSM01)

lastrowSSM20 = as.numeric(nrow(absSSM20))
NumSSM20 = c(1:lastrowSSM20)

lastrowSST13 = as.numeric(nrow(absSST13))
NumSST13 = c(1:lastrowSST13)

# 5. Create the final matrix 
grab.matrixSSM01 = cbind(absSSM01) # this is not binding anything and just copying absSSM01 again as grab.matrixSSM01?
rownames(grab.matrixSSM01) = as.numeric(NumSSM01)
colnames(grab.matrixSSM01) = as.numeric(wlSSM01)
grab.matrixSSM01 = as.matrix(grab.matrixSSM01)
str(grab.matrixSSM01)
attributes(grab.matrixSSM01)

grab.matrixSSM20 = cbind(absSSM20)
rownames(grab.matrixSSM20) = as.numeric(NumSSM20)
colnames(grab.matrixSSM20) = as.numeric(wlSSM20)
grab.matrixSSM20 = as.matrix(grab.matrixSSM20)
str(grab.matrixSSM20)
attributes(grab.matrixSSM20)

grab.matrixSST13 = cbind(absSST13)
rownames(grab.matrixSST13) = as.numeric(NumSST13)
colnames(grab.matrixSST13) = as.numeric(wlSST13)
grab.matrixSST13 = as.matrix(grab.matrixSST13)
str(grab.matrixSST13)
attributes(grab.matrixSST13)

# 6. Make this into spectral matrix for model
# Must be in format: grab.spectra = spectra(value = abs, bands = wl, names = Num)
grab.spectraSSM01 = spectra(value = absSSM01, bands = wlSSM01, names = NumSSM01)
attributes(grab.spectraSSM01)
plot(grab.spectraSSM01) # Note, bands here = absorbance from the scans

grab.spectraSSM20 = spectra(value = absSSM20, bands = wlSSM20, names = NumSSM20)
attributes(grab.spectraSSM20)
plot(grab.spectraSSM20) # Note, bands here = absorbance from the scans

grab.spectraSST13 = spectra(value = absSST13, bands = wlSST13, names = NumSST13)
attributes(grab.spectraSST13)
plot(grab.spectraSST13) # Note, bands here = absorbance from the scans

#grab.spectra = as_spectra.list(grab.spectra, wave_unit = "wavenumber", measurement_nit = "absorbance")
grab.spectraSSM01 = as.matrix(grab.spectraSSM01)
grab.spectraSSM20 = as.matrix(grab.spectraSSM20)
grab.spectraSST13 = as.matrix(grab.spectraSST13)
#str(grab.spectra)

# Change attributes so this is correct for scan data
attr(grab.spectraSSM01, 'wave_unit') = 'wavelength'
attr(grab.spectraSSM01, 'measurement_unit') = 'absorbance'
attributes(grab.spectraSSM01)

attr(grab.spectraSSM20, 'wave_unit') = 'wavelength'
attr(grab.spectraSSM20, 'measurement_unit') = 'absorbance'
attributes(grab.spectraSSM20)

attr(grab.spectraSST13, 'wave_unit') = 'wavelength'
attr(grab.spectraSST13, 'measurement_unit') = 'absorbance'
attributes(grab.spectraSST13)

########################################################################################
#### STEP 5: Create matrices of ALL spectral data - raw data that needs calibration ####
########################################################################################
# 1. Index FULL dataset with columns with absorbances
# raw spectra
scan.specSSM01 = SSM01[18:127]
scan.specSSM20 = SSM20[18:127] 
scan.specSST13 = SST13[18:130]

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
absSSM01 = (scan.specSSM01)
absSSM20 = (scan.specSSM20) 
absSST13 = (scan.specSST13) 

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wlSSM01 <- gsub("_clean", "", colnames(absSSM01))   
wlSSM01 <- as.numeric(wlSSM01)
wlSSM20 <- gsub("_clean", "", colnames(absSSM20))   
wlSSM20 <- as.numeric(wlSSM20)
wlSST13 <- gsub("_clean", "", colnames(absSST13))   
wlSST13 <- as.numeric(wlSST13)

# 4. Create a vector with sample labels that match the absorbance matrix rows. 
lastrowSSM01 = as.numeric(nrow(absSSM01))
NumSSM01 = c(1:lastrowSSM01)

lastrowSSM20 = as.numeric(nrow(absSSM20))
NumSSM20 = c(1:lastrowSSM20)

lastrowSST13 = as.numeric(nrow(absSST13))
NumSST13 = c(1:lastrowSST13)

# 5. Create the final matrix 
#SSM01
scan.matrixSSM01 = cbind(absSSM01)
rownames(scan.matrixSSM01) = as.numeric(NumSSM01)
colnames(scan.matrixSSM01) = as.numeric(wlSSM01)

scan.matrixSSM01 = as.matrix(scan.matrixSSM01)
specSSM01 = spectra(value = absSSM01, bands = wlSSM01, names = NumSSM01)
plot(specSSM01) # Note = reflectance here = absorbance from the scans

#SSM20
scan.matrixSSM20 = cbind(absSSM20)
rownames(scan.matrixSSM20) = as.numeric(NumSSM20)
colnames(scan.matrixSSM20) = as.numeric(wlSSM20)

scan.matrixSSM20 = as.matrix(scan.matrixSSM20)
specSSM20 = spectra(value = absSSM20, bands = wlSSM20, names = NumSSM20)
plot(specSSM20) # Note = reflectance here = absorbance from the scans

#SST13
scan.matrixSST13 = cbind(absSST13)
rownames(scan.matrixSST13) = as.numeric(NumSST13)
colnames(scan.matrixSST13) = as.numeric(wlSST13)

scan.matrixSST13 = as.matrix(scan.matrixSST13)
specSST13 = spectra(value = absSST13, bands = wlSST13, names = NumSST13)
plot(specSST13) # Note = reflectance here = absorbance from the scans

# NOTE: this is where you can identify problem spectra & remove them

# = as.spectra.list(spec)
scan.spectraSSM01 = as.matrix(specSSM01)
str(scan.spectraSSM01)
attr(scan.spectraSSM01, 'wave_unit') = 'wavelength'
attr(scan.spectraSSM01, 'measurement_unit') = 'absorbance'
attributes(scan.spectraSSM01)

scan.spectraSSM20 = as.matrix(specSSM20)
str(scan.spectraSSM20)
attr(scan.spectraSSM20, 'wave_unit') = 'wavelength'
attr(scan.spectraSSM20, 'measurement_unit') = 'absorbance'
attributes(scan.spectraSSM20)

scan.spectraSST13 = as.matrix(specSST13)
str(scan.spectraSST13)
attr(scan.spectraSST13, 'wave_unit') = 'wavelength'
attr(scan.spectraSST13, 'measurement_unit') = 'absorbance'
attributes(scan.spectraSST13)

####################################################################
#### STEP 6: Create a new data frame with the spectral matrices ####
####################################################################
# This creates a data frame with 
# 1. DOC (scan)
# 3. Full s::can spectra (from 2SSM20-750nm)
length(scan_DOC_SSM01)
dim(scan.spectraSSM01) 
class(scan.spectraSSM01)

# NOTE: We use the I() function to protect the Spectra 
spectralcal.dfSSM01 = data.frame(DOCSSM01 = scan_DOC_SSM01, SpectraSSM01 = I(scan.spectraSSM01))
str(spectralcal.dfSSM01)

spectralcal.dfSSM20 = data.frame(DOCSSM20 = scan_DOC_SSM20, SpectraSSM20 = I(scan.spectraSSM20))
str(spectralcal.dfSSM20)

spectralcal.dfSST13 = data.frame(DOCSST13 = scan_DOC_SST13, SpectraSST13 = I(scan.spectraSST13))
str(spectralcal.dfSST13)

# Also do this for the GRAB sample data
grabcal.dfSSM01 = data.frame(DOCSSM01 = grab.DOCSSM01, SpectraSSM01 = I(grab.spectraSSM01))
str(grabcal.dfSSM01)

grabcal.dfSSM20 = data.frame(DOCSSM20 = grab.DOCSSM20, SpectraSSM20 = I(grab.spectraSSM20))
str(grabcal.dfSSM20)

grabcal.dfSST13 = data.frame(DOCSST13 = grab.DOCSST13, SpectraSST13 = I(grab.spectraSST13))
str(grabcal.dfSST13)

#################################################
#### STEP 7: Develop PLSR training data sets ####
#################################################
# Create a training and test data set
# Carbon
CTrainSSM01 = grabcal.dfSSM01
CTestSSM01 = spectralcal.dfSSM01

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
CmodSSM01 = plsr(DOCSSM01 ~ SpectraSSM01, ncomp = 9, data = CTrainSSM01, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(CmodSSM01) # optimized for 4 components

# Plot RMSE of the predictions to optimize model
plot(RMSEP(CmodSSM01), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error
# NOTE: This plot may be messy, given low number of grab samples 
plot(CmodSSM01, ncomp = 3, asp = 1, line = TRUE)

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################
# Predict model!
predictedCSSM01 = predict(CmodSSM01, ncomp = 3, newdata = spectralcal.dfSSM01) # use reduced error model
str(predictedCSSM01)
plot(predictedCSSM01)

write.csv(predictedCSSM01, file = "predicted/PredictedC_SSM01.csv") # <- this is your newly calibrated dataset!

## NOTE: If your s::can has significant drift (e.g., which often happens when there is biofouling), 
# You might need to use a moving window approach to the calibraiton (i.e., calibrate 1 month at a time)

#################################################
#### STEP 7: Develop PLSR training data sets ####
#################################################
# Create a training and test dataset
# Carbon
CTrainSSM20 = grabcal.dfSSM20
CTestSSM20 = spectralcal.dfSSM20

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
CmodSSM20 = plsr(DOCSSM20 ~ SpectraSSM20, ncomp = 2, data = CTrainSSM20, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(CmodSSM20) # optimized for 4 components

# Plot RMSE of the predictions to optimize model
plot(RMSEP(CmodSSM20), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error (in this case, x)
# NOTE: This plot may be messy, given low number of grab samples 
plot(CmodSSM20, ncomp = 2, asp = 1, line = TRUE)

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################
# Predict model!
predictedCSSM20 = predict(CmodSSM20, ncomp = 2, newdata = spectralcal.dfSSM20) # use reduced error model
str(predictedCSSM20)
# Plot final predictions
plot(predictedCSSM20)

write.csv(predictedCSSM20, file = "predicted/PredictedC_SSM20.csv") # <- this is your newly calibrated dataset!

#################################################
#### STEP 7: Develop PLSR training data sets ####
#################################################
# Create a training and test dataset
# Carbon
CTrainSST13 = grabcal.dfSST13
CTestSST13 = spectralcal.dfSST13

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
CmodSST13 = plsr(DOCSST13 ~ SpectraSST13, ncomp = 3, data = CTrainSST13, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(CmodSST13) # optimized for 4 components

# Plot RMSE of the predictions to optimize model
plot(RMSEP(CmodSST13), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error
# NOTE: This plot may be messy, given low number of grab samples 
plot(CmodSST13, ncomp = 1, asp = 1, line = TRUE)

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################
# Predict model!
predictedCSST13 = predict(CmodSST13, ncomp = 3, newdata = spectralcal.dfSST13) # use reduced error model
str(predictedCSST13)
# Plot
plot(predictedCSST13)

write.csv(predictedCSST13, file = "predicted/PredictedC_SST13.csv") # <- this is your newly calibrated dataset!

# 1. Loadings Plot for SSM01 (Opposite Trend)
# This shows how the wavelengths contribute to each component (ncomp = 1, 2, 3, etc.)
plot(CmodSSM01, plottype = "loading",
     comps = 1:2, # Plot the first two components for initial inspection
     main = "SSM01 NO3-N PLSR Loadings")

# 2. Loadings Plot for SSM20 (Flat Trend)
plot(CmodSSM20, plottype = "loading",
     comps = 1:2, # Plot the first two components
     main = "SSM20 NO3-N PLSR Loadings")

# 3. Loadings Plot for SST13 (Flat Trend)
# Examine the first few components for SST13
plot(CmodSST13, plottype = "loading",
     comps = 1:2, # Plot the first two components
     main = "SST13 NO3-N PLSR Loadings")

# Convert predictedCSST13 to a data frame
pred_df <- data.frame(
  DateTime = as.POSIXct(dimnames(predictedCSST13)[[1]]),
  Predicted = as.numeric(predictedCSST13))
# Plot
ggplot(pred_df, aes(x = DateTime, y = Predicted)) +
  geom_point(color = "steelblue") +
  labs(
    x = "DateTime",
    y = "Predicted DOC (mg/L)",
    title = "Predicted DOC over Time (SST13)"
  ) +
  theme_minimal()

#######################
#### Save in Drive #### 
#######################
# Define the target folder ID in Google Drive
# This is the "predicted" folder
drive_folder_id <- "13bh64kWtdgknMUqdfDKkJ4JAzvWqLpu8"

# Upload the file to the specified Google Drive folder
drive_put(media = "predicted/PredictedC_SSM01.csv", path = as_id(drive_folder_id))
drive_put(media = "predicted/PredictedC_SSM20.csv", path = as_id(drive_folder_id))
drive_put(media = "predicted/PredictedC_SST13.csv", path = as_id(drive_folder_id))
