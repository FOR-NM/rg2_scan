## NAME: PLSR Calibration Code 
## CODER: Arial Shogren
## FILE DATE: 01.13.2024
## PURPOSE: Calibration for s::can spectral data using grab samples
## SITE: Talladega

# ---- Talladega TLP Site (Downstream)  ----
#### ---- Download necessary packages ---- 

#install.packages("spectrolab") # No 
library(spectrolab)
#install.packages("RcppArmadillo")
#install.packages("dev.tools")
#install.packages("pls")
library(devtools)
library(pls)
#install_github(repo = "meireles/spectrolab") # Install analysis package
# Make sure to hit "no" for install
#install.packages("devtools")
#install.packages("devtools", repos = "http://cran.us.r-project.org")
#install.packages("merTools")
#require(RcppArmadillo)



#install_github(repo = "griffithdan/plantspec") # Install analysis package
#library(plantspec)
#install_github(repo = "griffithdan/plantspecDB") # Install data package 
#library(plantspecDB)

#library(spectrolab) # no longer available with this verison of R
library(data.table)
library(xts)
library(pls)
library(readxl)
library(devtools)
#library(plantspec)
#library(plantspecDB)
#library(merTools)



#### ---- STEP 1: Prep grab sample data ---- 

# What you need to do here is match the grab samples with the time stamp of the s::can
# I usually do this by hand in excel because we have so few grab samples / s::cans
# But this could feasibly be done using a merge function in R

#### ---- STEP 2: Upload scan dataframe [with spectra] ---- 
# Upload s::can data - including the reference samples

scandat = read_excel("calibrating_exmpl/TAL_scan compiled data_2023.01.06 to 2024.01.03.xlsx",
                     sheet = "TLM01_2023", na = c("", "NaN", "Na")) # make sure this matches your non-detects
head(scandat) # Just to check
ncol(scandat) # Just to check 

# Upload the raw scan-generated concentration data, spectra, and your grab samples
# This makes them a time-stamped object
scan.DOC = xts(scandat$DOC_mg_L_scan, as.POSIXct(scandat$DateTime, format = "%m/%d/%Y %H:%M"))
scan.TSS = xts(scandat$TSS_mg_L_scan, as.POSIXct(scandat$DateTime, format = "%m/%d/%Y %H:%M"))

scan.spec = xts(scandat[10:220], as.POSIXct(scandat$DateTime, format = "%m/%d/%Y %H:%M")) 
  # select full spectra
  # note here that if there are 0s in your spectra, this code will throw an error
  # so only use the wavelengths where you have detectable absorbance
#grab.DOC = xts(scandat$DOC_mg_L, as.POSIXct(scandat$DateTime, format = "%m/%d/%Y %H:%M")) 


#### ---- STEP 3: Compare grab and raw scan data ---- 
# This is just a check to see how well the s::can did relative to your known concentraitons 
# I upload this as a new dataframe, jsut because in the previous step I had assigned these XTS values
# Feel free to change this! It's not the most efficient way to do this...

# DOC
# ---- Lining up scan and grab sample data 

grabdat = read_excel("calibrating_exmpl/TAL_scan compiled data_2023.01.06 to 2024.01.03.xlsx",
                     sheet = "TLM01_2023", na = c("", "NaN", "Na")) 
grabdat = as.data.frame(grabdat)
#str(grabdat)
grab.spec = grabdat[grabdat$Grab_sample == "Y",] # Ony gets data when there is a Y
grab.DOC = grab.spec$DOC_mg_L
grab.TSS = grab.spec$TSS_mg_L
spec.DOC = grab.spec$DOC_mg_L_scan
spec.TSS = grab.spec$TSS_mg_L_scan
grab.spec.dat = grab.spec[10:220] # Full spectra
grab.spec.dat = na.omit(grab.spec.dat) # Omits rows with NA absorbance values
#head(grab.spec.dat)

plot(grab.DOC ~ spec.DOC)
calib.mod.DOC = lm(grab.DOC ~ spec.DOC)
summary(calib.mod.DOC)

plot(grab.TSS ~ spec.TSS)
calib.mod.TSS = lm(grab.TSS ~ spec.TSS)
summary(calib.mod.TSS)


# So in this case, the s::can does a pretty good job! But we will still use PLSR 
# for more precision

#### ---- STEP 4: Create matrices of GRAB spectral data - this is the training dataset ----
# 1. Index dataset with columns with absorbances
grab.spec.dat = grab.spec[10:220] # Full spectra, with no NAs

scan.spec

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
abs = (grab.spec.dat)
#str(abs)

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wl = as.numeric(colnames(abs))
#str(wl)

# 4. Create a vector with sample labels that match the absorbance matrix rows. 
lastrow = as.numeric(nrow(abs))
Num = c(1:lastrow)

# 5. Create the final matrix 
grab.matrix = cbind(abs)
rownames(grab.matrix) = as.numeric(Num)
colnames(grab.matrix) = as.numeric(wl)

grab.matrix = as.matrix(grab.matrix)
str(grab.matrix)
attributes(grab.matrix)

# 6. Make this into spectral matrix for model
# Must be in format: grab.spectra = spectra(value = abs, bands = wl, names = Num)
grab.spectra = spectra(value
                  = abs, bands = wl, names = Num)
attributes(grab.spectra)
plot(grab.spectra) # Note = bands here = absorbance from the scans

#grab.spectra = as_spectra.list(grab.spectra, wave_unit = "wavenumber", measurement_nit = "absorbance")

grab.spectra = as.matrix(grab.spectra)
#str(grab.spectra)

# Change attributes so this is correct for scan data
attr(grab.spectra, 'wave_unit') = 'wavelength'
attr(grab.spectra, 'measurement_unit') = 'absorbance'
attributes(grab.spectra)

#### ---- STEP 5: Create matrices of ALL spectral data - raw data that needs calibration ----

# 1. Index FULL dataset with columns with absorbances
scan.spec = scandat[10:220]

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
abs = (scan.spec)

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wl = as.numeric(colnames(abs))

# 4. Create a vector with sample labels that match the absorbance matrix rows. 
lastrow = as.numeric(nrow(abs))
Num = c(1:lastrow)

# 5. Create the final matrix 
scan.matrix = cbind(abs)
rownames(scan.matrix) = as.numeric(Num)
colnames(scan.matrix) = as.numeric(wl)

scan.matrix = as.matrix(scan.matrix)
spec = spectra(value = abs, bands = wl, names = Num)
#head(spec)
plot(spec) # Note = reflectance here = absorbance from the scans

# NOTE: this is where you can identify problem spectra & remove them

# = as.spectra.list(spec)
scan.spectra = as.matrix(spec)
str(scan.spectra)
attr(scan.spectra, 'wave_unit') = 'wavelength'
attr(scan.spectra, 'measurement_unit') = 'absorbance'
attributes(scan.spectra)

#### ---- STEP 6: Create a new dataframe with the spectral matrices ----

# This creates a dataframe with 
# 1. DOC (scan)
# 2. TSS (scab)
# 3. Full s::can spectra (from 220-750nm)

# NOTE: We use the I() function to protect the Spectra 
spectralcal.df = data.frame(DOC = scan.DOC, TSS = scan.TSS, Spectra = I(scan.spectra))
str(spectralcal.df)

# Also do this for the GRAB sample data
grabcal.df = data.frame(DOC = grab.DOC, TSS = grab.TSS, Spectra = I(grab.spectra))
str(grabcal.df)
#### ---- STEP 7: Develop PLSR training datasets  ----

# Create a training and test dataset
# Carbon
CTrain = grabcal.df
CTest = spectralcal.df

# TSS
TTrain = grabcal.df
TTest = spectralcal.df

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
Cmod = plsr(DOC ~ Spectra, ncomp = 8, data = CTrain, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod) # optimized for 4 components

Tmod = plsr(TSS ~ Spectra, ncomp = 8, data = TTrain, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod)

# Plot RMSE of the predictions to optimize model
plot(RMSEP(Cmod), legendpos = "topright")
plot(RMSEP(Tmod), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error (in this case, 1)
# NOTE: This plot may be messy, given low number of grab samples 
plot(Cmod, ncomp = 2, asp = 1, line = TRUE)
plot(Tmod, ncomp = 1, asp = 1, line = TRUE)

#### ---- STEP 8: Make predictions based on reduced-error PLSR model ---- 

# Predict model!
predictedC = predict(Cmod, ncomp = 2, newdata = spectralcal.df) # use reduced error model
str(predictedC)
plot(predictedC)

predictedT = predict(Tmod, ncomp = 1, newdata = spectralcal.df) # use reduced error model
str(predictedT)
# Plot final predictions
plot(predictedT)

write.csv(predictedC, file = "TAL_TLM_2023_DOCpredicted.csv") # <- this is your newly calibrated dataset!
write.csv(predictedT, file = "TAL_TLM_2023_TSSpredicted.csv") # <- this is your newly calibrated dataset!


## NOTE: If your s::can has significant drift (e.g., which often happens when there is biofouling), 
# You might need to use a moving window approach to the calibraiton (i.e., calibrate 1 month at a time)
# This is a bit more complicated, so start with this simple calibration first. 
