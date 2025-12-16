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
# This is the "with chem" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/12N6uUxXTttdnadDrn43mL6ilz-Ui2eQX")

# List all CSVs files in the folder
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

# # DateTime at midnight is missing 00:00:00 time, so filling in that time using grep                     
# DVO$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DVO$DateTime)] <- paste(
#   DVO$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DVO$DateTime)],"00:00:00")
# DVMS1$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DVMS1$DateTime)] <- paste(
#   DVMS1$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DVMS1$DateTime)],"00:00:00")
# DVNWT5$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DVNWT5$DateTime)] <- paste(
#   DVNWT5$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DVNWT5$DateTime)],"00:00:00")

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

# Extract DOC NO3 NO3N and TSS data as time series objects (xts)
scan_DOC_DVO <- xts(DVO$DOC_mg.l , order.by = DVO$DateTime)
scan_TSS_DVO <- xts(DVO$TSS_clean, order.by = DVO$DateTime)
scan_NO3N_DVO <- xts(DVO$NO3..mg.N.L., order.by = DVO$DateTime)

scan_DOC_DVMS1 <- xts(DVMS1$DOC_mg.l, order.by = DVMS1$DateTime)
scan_TSS_DVMS1 <- xts(DVMS1$TSS_clean, order.by = DVMS1$DateTime)
scan_NO3N_DVMS1 <- xts(DVMS1$NO3..mg.N.L., order.by = DVMS1$DateTime)

scan_DOC_DVNWT5 <- xts(DVNWT5$DOC_mg.l, order.by = DVNWT5$DateTime)
scan_TSS_DVNWT5 <- xts(DVNWT5$TSS_clean, order.by = DVNWT5$DateTime)
scan_NO3N_DVNWT5 <- xts(DVNWT5$NO3..mg.N.L., order.by = DVNWT5$DateTime)

# Extract spectral data (assuming spectral columns are in range "200.00.nm" to "750.00.nm")
scan.spec12 = xts(DVO[15:224], as.POSIXct(DVO$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.spec20 = xts(DVMS1[15:224], as.POSIXct(DVMS1$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.spec21 = xts(DVNWT5[15:224], as.POSIXct(DVNWT5$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
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

grab.DOC12 = grab_DVO$NPOC..mg.C.L.
grab.NO3N12 = grab_DVO$NO3..mg.N.L.

grab.DOC20 = grab_DVMS1$NPOC..mg.C.L.
grab.NO3N20 = grab_DVMS1$NO3..mg.N.L.

grab.DOC21 = grab_DVNWT5$NPOC..mg.C.L.
grab.NO3N21 = grab_DVNWT5$NO3..mg.N.L.

# #### remove a couple of problematic samples ####
# grab_DVO <- grab_DVO %>%
#   mutate(NPOC..mg.C.L. = ifelse(Date == "2025-01-02" | is.na(NPOC..mg.C.L.),
#                                 NA,
#                                 NPOC..mg.C.L.))
# grab_DVMS1 <- grab_DVMS1 %>%
#   mutate(NPOC..mg.C.L. = ifelse(Date == "2024-10-24" | is.na(NPOC..mg.C.L.),
#                                 NA,
#                                 NPOC..mg.C.L.))
# grab_DVMS1 <- grab_DVMS1 %>%
#   mutate(NO3..mg.N.L. = ifelse(Date == "2024-09-11" | is.na(NO3..mg.N.L.),
#                                NA,
#                                NO3..mg.N.L.))
# 
# grab_DVNWT5 <- grab_DVNWT5 %>%
#   mutate(NPOC..mg.C.L. = ifelse(Date == "2024-08-30" | is.na(NPOC..mg.C.L.),
#                                 NA,
#                                 NPOC..mg.C.L.))
# grab_DVNWT5 <- grab_DVNWT5 %>%
#   mutate(NO3..mg.N.L. = ifelse(Date %in% c("2024-06-27", "2024-07-17", "2024-09-18", "2025-06-13") | is.na(NO3..mg.N.L.),
#                                NA,
#                                NO3..mg.N.L.))
# compare grab vs scan DOC
plot(grab_DVO$DOCeq..mg.l....Measured.value ~ grab_DVO$NPOC..mg.C.L.)
ggplot(grab_DVO, aes(x = NPOC..mg.C.L., y = DOCeq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOC12 = lm(grab_DVO$DOCeq..mg.l....Measured.value ~ grab_DVO$NPOC..mg.C.L.)
summary(calib.mod.DOC12)

ggplot(grab_DVMS1, aes(x = NPOC..mg.C.L., y = DOCeq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOC20 = lm(grab_DVMS1$DOCeq..mg.l....Measured.value ~ grab_DVMS1$NPOC..mg.C.L.)
summary(calib.mod.DOC20)

ggplot(grab_DVNWT5, aes(x = NPOC..mg.C.L., y = DOCeq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOC21 = lm(grab_DVNWT5$DOCeq..mg.l....Measured.value ~ grab_DVNWT5$NPOC..mg.C.L.)
summary(calib.mod.DOC21)

# compare grab vs scan NO3
plot(grab_DVO$NO3.Neq..mg.l....Measured.value ~ grab_DVO$NO3..mg.N.L.)
ggplot(grab_DVO, aes(x = NO3..mg.N.L., y = NO3.Neq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.NO3N12 = lm(grab_DVO$NO3.Neq..mg.l....Measured.value ~ grab_DVO$NO3..mg.N.L.)
summary(calib.mod.NO3N12)

plot(grab_DVMS1$NO3.Neq..mg.l....Measured.value ~ grab_DVMS1$NO3..mg.N.L.)
ggplot(grab_DVMS1, aes(x = NO3..mg.N.L., y = NO3.Neq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.NO3N20 = lm(grab_DVMS1$NO3.Neq..mg.l....Measured.value ~ grab_DVMS1$NO3..mg.N.L.)
summary(calib.mod.NO3N20)

plot(grab_DVNWT5$NO3.Neq..mg.l....Measured.value ~ grab_DVNWT5$NO3..mg.N.L.)
ggplot(grab_DVNWT5, aes(x = NO3..mg.N.L., y = NO3.Neq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.NO3N21 = lm(grab_DVNWT5$NO3.Neq..mg.l....Measured.value ~ grab_DVNWT5$NO3..mg.N.L.)
summary(calib.mod.NO3N21)

#######################################################################################
#### STEP 4: Create matrices of GRAB spectral data - this is the training data set ####
#######################################################################################
# 1. Index data set with columns with absorbances
# raw spectra
grab.spec.dat12 = grab_DVO[19:228] # Full spectra, with no NAs?
grab.spec.dat20 = grab_DVMS1[19:228]
grab.spec.dat21 = grab_DVNWT5[19:228] 
# clean spectra
# grab.spec.dat12 = grab_DVO[243:443] # Full spectra, with no NAs?
# grab.spec.dat20 = grab_DVMS1[241:441]
# grab.spec.dat21 = grab_DVNWT5[243:443] 

# Rename columns for all data frames (e.g., DVO, DVMS1, DVNWT5)
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
scan.spec12 = DVO[19:228]
scan.spec20 = DVMS1[19:228] 
scan.spec21 = DVNWT5[19:228]
# clean spectra
# scan.spec12 = DVO[243:443]
# scan.spec20 = DVMS1[241:441] 
# scan.spec21 = DVNWT5[243:443]

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
#DVO
scan.matrix12 = cbind(abs12)
rownames(scan.matrix12) = as.numeric(Num12)
colnames(scan.matrix12) = as.numeric(wl12)

scan.matrix12 = as.matrix(scan.matrix12)
spec12 = spectra(value = abs12, bands = wl12, names = Num12)
plot(spec12) # Note = reflectance here = absorbance from the scans

#DVMS1
scan.matrix20 = cbind(abs20)
rownames(scan.matrix20) = as.numeric(Num20)
colnames(scan.matrix20) = as.numeric(wl20)

scan.matrix20 = as.matrix(scan.matrix20)
spec20 = spectra(value = abs20, bands = wl20, names = Num20)
plot(spec20) # Note = reflectance here = absorbance from the scans

#DVNWT5
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

length(scan_DOC_DVO)
length(scan_NO3N_DVO)
dim(scan.spectra12) 
class(scan.spectra12)

# NOTE: We use the I() function to protect the Spectra 
spectralcal.df12 = data.frame(DOC12 = scan_DOC_DVO, NO3N12 = scan_NO3N_DVO, Spectra12 = I(scan.spectra12))
str(spectralcal.df12)

spectralcal.df20 = data.frame(DOC20 = scan_DOC_DVMS1, NO3N20 = scan_NO3N_DVMS1, Spectra20 = I(scan.spectra20))
str(spectralcal.df20)

spectralcal.df21 = data.frame(DOC21 = scan_DOC_DVNWT5, NO3N21 = scan_NO3N_DVNWT5, Spectra21 = I(scan.spectra21))
str(spectralcal.df21)

# Also do this for the GRAB sample data
grabcal.df12 = data.frame(DOC12 = grab.DOC12, NO3N12 = grab.NO3N12, Spectra12 = I(grab.spectra12))
str(grabcal.df12)

grabcal.df20 = data.frame(DOC20 = grab.DOC20, NO3N20 = grab.NO3N20, Spectra20 = I(grab.spectra20))
str(grabcal.df20)

grabcal.df21 = data.frame(DOC21 = grab.DOC21, NO3N21 = grab.NO3N21, Spectra21 = I(grab.spectra21))
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
Cmod12 = plsr(DOC12 ~ Spectra12, ncomp = 15, data = CTrain12, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod12) # optimized for 4 components

Nmod12 = plsr(NO3N12 ~ Spectra12, ncomp = 15, data = NTrain12, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod12)

# Plot RMSE of the predictions to optimize model
plot(RMSEP(Cmod12), legendpos = "topright")
plot(RMSEP(Nmod12), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error (in this case, 2)
# NOTE: This plot may be messy, given low number of grab samples 
plot(Cmod12, ncomp = 3, asp = 1, line = TRUE)
plot(Nmod12, ncomp = 2, asp = 1, line = TRUE)

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################
# Predict model!
predictedC12 = predict(Cmod12, ncomp = 4, newdata = spectralcal.df12) # use reduced error model
str(predictedC12)
plot(predictedC12)

predictedN12 = predict(Nmod12, ncomp = 6, newdata = spectralcal.df12) # use reduced error model
str(predictedN12)
# Plot final predictions
plot(predictedN12)

write.csv(predictedC12, file = "predicted/PredictedC_DVO.csv") # <- this is your newly calibrated dataset!
write.csv(predictedN12, file = "predicted/PredictedN_DVO.csv") # <- this is your newly calibrated dataset!

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
    title = "Predicted DOC over Time (DVO)"
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
    title = "Predicted DOC (DVO): June – Sep 2025"
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
Cmod20 = plsr(DOC20 ~ Spectra20, ncomp = 8, data = CTrain20, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod20) # optimized for 4 components

Nmod20 = plsr(NO3N20 ~ Spectra20, ncomp = 8, data = NTrain20, validation = "LOO") # usually ncomp is N-1 grab samples you have
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
# Plot final predictions
plot(predictedC20)

predictedN20 = predict(Nmod20, ncomp = 2, newdata = spectralcal.df20) # use reduced error model
str(predictedN20)
# Plot final predictions
plot(predictedN20)

write.csv(predictedC20, file = "predicted/PredictedC_DVMS1.csv") # <- this is your newly calibrated dataset!
write.csv(predictedN20, file = "predicted/PredictedN_DVMS1.csv") # <- this is your newly calibrated dataset!

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
Cmod21 = plsr(DOC21 ~ Spectra21, ncomp = 7, data = CTrain21, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod21) # optimized for 4 components

Nmod21 = plsr(NO3N21 ~ Spectra21, ncomp = 7, data = NTrain21, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod21)

# Plot RMSE of the predictions to optimize model
plot(RMSEP(Cmod21), legendpos = "topright")
plot(RMSEP(Nmod21), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error (in this case, 1)
# NOTE: This plot may be messy, given low number of grab samples 
plot(Cmod21, ncomp = 4, asp = 1, line = TRUE)
plot(Nmod21, ncomp = 4, asp = 1, line = TRUE)

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################
# Predict model!
predictedC21 = predict(Cmod21, ncomp = 4, newdata = spectralcal.df21) # use reduced error model
str(predictedC21)
# Plot
plot(predictedC21)

head(predictedC21)
predictedN21 = predict(Nmod21, ncomp = 2, newdata = spectralcal.df21) # use reduced error model
str(predictedN21)
# Plot
plot(predictedN21)

write.csv(predictedC21, file = "predicted/PredictedC_DVNWT5.csv") # <- this is your newly calibrated dataset!
write.csv(predictedN21, file = "predicted/PredictedN_DVNWT5.csv") # <- this is your newly calibrated dataset!

# # 1. Loadings Plot for DVO (Opposite Trend)
# # This shows how the wavelengths contribute to each component (ncomp = 1, 2, 3, etc.)
# plot(Nmod12, plottype = "loading",
#      comps = 1:2, # Plot the first two components for initial inspection
#      main = "DVO NO3-N PLSR Loadings")
# 
# # 2. Loadings Plot for DVNWT5 (Flat Trend)
# # Examine the first few components for DVNWT5
# plot(Nmod21, plottype = "loading",
#      comps = 1:2, # Plot the first two components
#      main = "DVNWT5 NO3-N PLSR Loadings")

#######################
#### Save in Drive #### 
#######################
# Define the target folder ID in Google Drive
# This is the "predicted" folder
drive_folder_id <- "1wa1ycqUYv56y3fTn1-VaN2K-NLU3rFeU"

# Upload the file to the specified Google Drive folder
drive_upload(media = "predicted/PredictedC_DVO.csv", path = as_id(drive_folder_id))
drive_upload(media = "predicted/PredictedC_DVMS1.csv", path = as_id(drive_folder_id))
drive_upload(media = "predicted/PredictedC_DVNWT5.csv", path = as_id(drive_folder_id))

## NOTE: If your s::can has significant drift (e.g., which often happens when there is biofouling), 
# You might need to use a moving window approach to the calibraiton (i.e., calibrate 1 month at a time)
# This is a bit more complicated, so start with this simple calibration first. 