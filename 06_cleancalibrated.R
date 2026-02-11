##==============================================================================
## Project: QuEST
## After Calibrating s::can data using Partial Least Squares Regression (PLSR), 
## there are some extra cleaning steps we had to do, here they are
##==============================================================================

library(spectrolab)
library(ggplot2)
library(plotly)

####################################
#### Upload predicted scan data ####
####################################
# This is the "predicted" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1wa1ycqUYv56y3fTn1-VaN2K-NLU3rFeU")
# List all xlsx files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")
3

# NO3
#USF12
googledrive::drive_download(file = merged$id[merged$name=="PredictedN_USF12.csv"], 
                            path = "googledrive/PredictedN_USF12.csv",
                            overwrite = T)
#USF20
googledrive::drive_download(file = merged$id[merged$name=="PredictedN_USF20.csv"], 
                            path = "googledrive/PredictedN_USF20.csv",
                            overwrite = T)
#USF21
googledrive::drive_download(file = merged$id[merged$name=="PredictedN_USF21.csv"], 
                            path = "googledrive/PredictedN_USF21.csv",
                            overwrite = T)

# Let's load them separately first
NO312 <- read.csv("googledrive/PredictedN_USF12.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
NO320 <- read.csv("googledrive/PredictedN_USF20.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
NO321 <- read.csv("googledrive/PredictedN_USF21.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)

# DateTime at midnight is missing 00:00:00 time, so filling in that time using grep
NO312$X[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",NO312$X)] <- paste(
  NO312$X[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",NO312$X)],"00:00:00")
NO320$X[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",NO320$X)] <- paste(
  NO320$X[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",NO320$X)],"00:00:00")
NO321$X[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",NO321$X)] <- paste(
  NO321$X[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",NO321$X)],"00:00:00")

# Convert the DateTime column to POSIXct
NO312$DateTime <- as.POSIXct(NO312$X, format = "%Y-%m-%d %H:%M:%S")
NO320$DateTime <- as.POSIXct(NO320$X, format = "%Y-%m-%d %H:%M:%S")
NO321$DateTime <- as.POSIXct(NO321$X, format = "%Y-%m-%d %H:%M:%S")

# DOC
#USF12
googledrive::drive_download(file = merged$id[merged$name=="PredictedC_USF12.csv"], 
                            path = "googledrive/PredictedC_USF12.csv",
                            overwrite = T)
#USF20
googledrive::drive_download(file = merged$id[merged$name=="PredictedC_USF20.csv"], 
                            path = "googledrive/PredictedC_USF20.csv",
                            overwrite = T)
#USF21
googledrive::drive_download(file = merged$id[merged$name=="PredictedC_USF21.csv"], 
                            path = "googledrive/PredictedC_USF21.csv",
                            overwrite = T)

# Let's load them separately first
DOC12 <- read.csv("googledrive/PredictedC_USF12.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
DOC20 <- read.csv("googledrive/PredictedC_USF20.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
DOC21 <- read.csv("googledrive/PredictedC_USF21.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)

# DateTime at midnight is missing 00:00:00 time, so filling in that time using grep
DOC12$X[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DOC12$X)] <- paste(
  DOC12$X[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DOC12$X)],"00:00:00")
DOC20$X[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DOC20$X)] <- paste(
  DOC20$X[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DOC20$X)],"00:00:00")
DOC21$X[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DOC21$X)] <- paste(
  DOC21$X[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DOC21$X)],"00:00:00")

# Convert the DateTime column to POSIXct
DOC12$DateTime <- as.POSIXct(NO312$X, format = "%Y-%m-%d %H:%M:%S")
DOC20$DateTime <- as.POSIXct(NO320$X, format = "%Y-%m-%d %H:%M:%S")
DOC21$DateTime <- as.POSIXct(NO321$X, format = "%Y-%m-%d %H:%M:%S")

##############################
#### Upload raw scan data ####
##############################
# This is the "with chem" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1qjM3Zze-I5ycFCHNcd997UG6gYXBUoX8")
# List all xlsx files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")

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

# Convert the DateTime column to POSIXct
USF12$DateTime <- as.POSIXct(USF12$DateTime, format = "%Y-%m-%d %H:%M:%S")
USF20$DateTime <- as.POSIXct(USF20$DateTime, format = "%Y-%m-%d %H:%M:%S")
USF21$DateTime <- as.POSIXct(USF21$DateTime, format = "%Y-%m-%d %H:%M:%S")

#################################
#### Clean for extra columns ####
#################################
USF12 <- USF12[,-c(4, 5, 10, 11)]
USF20 <- USF20[,-c(4, 5, 10, 11)]
USF21 <- USF21[,-c(4, 5, 10, 11)]

#############################
#### Merge the data sets ####
#############################
dat12 <- merge(USF12, NO312, by = "DateTime")
dat20 <- merge(USF20, NO320, by = "DateTime")
dat21 <- merge(USF21, NO321, by = "DateTime")

dat12 <- merge(dat12, DOC12, by = "DateTime")
dat20 <- merge(dat20, DOC20, by = "DateTime")
dat21 <- merge(dat21, DOC21, by = "DateTime")

# remove some columns
dat12 <- dat12[,-c(134:135, 136, 138)]
dat20 <- dat20[,-c(134:135, 136, 138)]
dat21 <- dat21[,-c(134:135, 136, 138)]

###########################################
#### Clean negative Nitrate excursions ####
###########################################
# In rows where column NO3N is 0, set predicted NO3N column to 0
dat12_clean <- dat12 %>%
  mutate(NO3N12.2.comps = if_else(NO3.N_mg.l == 0, 0, NO3N12.2.comps))
dat20_clean <- dat20 %>%
  mutate(NO3N20.2.comps = if_else(NO3.N_mg.l == 0, 0, NO3N20.2.comps))
dat21_clean <- dat21 %>%
  mutate(NO3N21.2.comps = if_else(NO3.N_mg.l == 0, 0, NO3N21.2.comps))

####################################################
#### Plot spectra for negative predicted values ####
####################################################
# 1. Index data set with columns with absorbances
# raw spectra
grab.spec.dat12 = dat12[15:114] # Full spectra, with no NAs
grab.spec.dat20 = dat20[15:114]
grab.spec.dat21 = dat21[15:114] 

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

# 1. Identify which rows have negative predictions
# Column 133 is the predicted NO3N
neg_rows12 <- which(dat12[[134]] < 0)
neg_rows20 <- which(dat20[[134]] < 0)
neg_rows21 <- which(dat21[[134]] < 0)

neg12 <- dat12 %>% filter(dat12[[134]] > 0)
neg20 <- dat20 %>% filter(dat20[[134]] > 0)
neg21 <- dat21 %>% filter(dat21[[134]] > 0)

# 2. Filter your absorbance data and your sample numbers
# We use the index [neg_rows12, ] to pick specific rows and all columns
abs12_neg <- abs12[neg_rows12, ]
Num12_neg <- Num12[neg_rows12]

abs20_neg <- abs20[neg_rows20, ]
Num20_neg <- Num20[neg_rows20]

abs21_neg <- abs21[neg_rows21, ]
Num21_neg <- Num21[neg_rows21]

# 3. Create the spectral matrix for only those negative points
# Note: wl12 remains the same because wavelengths don't change
if(length(neg_rows12) > 0) {
  grab.spectra12_neg <- spectra(value = abs12_neg, bands = wl12, names = Num12_neg)
  
  # Plotting 12
  plot(grab.spectra12_neg, main = "Spectra for Negative Predictions (USF12)")
} else {
  print("No negative values found for USF12.")
}

if(length(neg_rows20) > 0) {
  grab.spectra20_neg <- spectra(value = abs20_neg, bands = wl20, names = Num20_neg)
  
  # Plotting 20
  plot(grab.spectra20_neg, main = "Spectra for Negative Predictions (USF20)")
} else {
  print("No negative values found for USF20.")
}

if(length(neg_rows21) > 0) {
  grab.spectra21_neg <- spectra(value = abs21_neg, bands = wl21, names = Num21_neg)
  
  # Plotting 21
  plot(grab.spectra21_neg, main = "Spectra for Negative Predictions (USF21)")
} else {
  print("No negative values found for USF21.")
}

##############
#### Plot ####
##############
p <- ggplot(neg12, aes(x = DateTime, y = NO3N12.2.comps)) +
  geom_line(color = "steelblue") +
  labs(
    x = "DateTime",
    y = "Predicted DOC (mg/L)",
    title = "Predicted NO3N over Time (USF12)"
  ) +
  theme_minimal()
ggplotly(p)

p <- ggplot(neg20, aes(x = DateTime, y = NO3N20.2.comps)) +
  geom_line(color = "steelblue") +
  labs(
    x = "DateTime",
    y = "Predicted DOC (mg/L)",
    title = "Predicted NO3N over Time (USF20)"
  ) +
  theme_minimal()
ggplotly(p)

p <- ggplot(neg21, aes(x = DateTime, y = NO3N21.2.comps)) +
  geom_line(color = "steelblue") +
  labs(
    x = "DateTime",
    y = "Predicted DOC (mg/L)",
    title = "Predicted NO3N over Time (USF21)"
  ) +
  theme_minimal()
ggplotly(p)

##############
#### Save ####
##############
write.csv(dat12_clean, file = "predicted/USF12.csv")
write.csv(dat20_clean, file = "predicted/USF20.csv")
write.csv(dat21_clean, file = "predicted/USF21.csv")

write.csv(neg12, file = "predicted/USF12.csv")
write.csv(neg20, file = "predicted/USF20.csv")
write.csv(neg21, file = "predicted/USF21.csv")

