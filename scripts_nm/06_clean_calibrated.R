##############################################
#### Upload scan dataframe [with spectra] ####
##############################################
# This data is already matched #
# This is the "merged" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1wa1ycqUYv56y3fTn1-VaN2K-NLU3rFeU")

# List all xlsx files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")
3

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
USF12 <- read.csv("googledrive/PredictedC_USF12.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
USF20 <- read.csv("googledrive/PredictedC_USF20.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
USF21 <- read.csv("googledrive/PredictedC_USF21.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)

# rename columns 
USF12 <- USF12 %>%
  rename(DateTime = X)
USF20 <- USF20 %>%
  rename(DateTime = X)
USF21 <- USF21 %>%
  rename(DateTime = X)

# DateTime at midnight is missing 00:00:00 time, so filling in that time using grep                     
USF12$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF12$DateTime)] <- paste(
  USF12$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF12$DateTime)],"00:00:00")
USF20$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF20$DateTime)] <- paste(
  USF20$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF20$DateTime)],"00:00:00")
USF21$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF21$DateTime)] <- paste(
  USF21$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF21$DateTime)],"00:00:00")

# Convert the DateTime column to POSIXct
USF12$DateTime <- as.POSIXct(USF12$DateTime, format = "%Y-%m-%d %H:%M:%S")
USF20$DateTime <- as.POSIXct(USF20$DateTime, format = "%Y-%m-%d %H:%M:%S")
USF21$DateTime <- as.POSIXct(USF21$DateTime, format = "%Y-%m-%d %H:%M:%S")

##############################################
#### Upload scan dataframe [with spectra] ####
##############################################
# This data is already matched #
# This is the "merged" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1g6aSuGnb--Qeyk-rceX82Y5wSNzCqFg0")

# List all xlsx files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")
3

#USF12
googledrive::drive_download(file = merged$id[merged$name=="USF12_absparams_Buttercup_clean.csv"], 
                            path = "googledrive/USF12_absparams_Buttercup_clean.csv",
                            overwrite = T)
#USF20
googledrive::drive_download(file = merged$id[merged$name=="USF20_absparams_Blossom_clean.csv"], 
                            path = "googledrive/USF20_absparams_Blossom_clean.csv",
                            overwrite = T)
#USF21
googledrive::drive_download(file = merged$id[merged$name=="USF21_absparams_Bubbles_clean.csv"], 
                            path = "googledrive/USF21_absparams_Bubbles_clean.csv",
                            overwrite = T)

# Let's load them separately first
USF12_spec <- read.csv("googledrive/USF12_absparams_Buttercup_clean.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
USF20_spec <- read.csv("googledrive/USF20_absparams_Blossom_clean.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
USF21_spec <- read.csv("googledrive/USF21_absparams_Bubbles_clean.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)

# DateTime at midnight is missing 00:00:00 time, so filling in that time using grep                     
USF12_spec$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF12_spec$DateTime)] <- paste(
  USF12_spec$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF12_spec$DateTime)],"00:00:00")
USF20_spec$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF20_spec$DateTime)] <- paste(
  USF20_spec$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF20_spec$DateTime)],"00:00:00")
USF21_spec$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF21_spec$DateTime)] <- paste(
  USF21_spec$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF21_spec$DateTime)],"00:00:00")

# Convert the DateTime column to POSIXct
USF12_spec$DateTime <- as.POSIXct(USF12_spec$DateTime, format = "%Y-%m-%d %H:%M:%S")
USF20_spec$DateTime <- as.POSIXct(USF20_spec$DateTime, format = "%Y-%m-%d %H:%M:%S")
USF21_spec$DateTime <- as.POSIXct(USF21_spec$DateTime, format = "%Y-%m-%d %H:%M:%S")

########################################################
#### Merge full spectral data with predicted values ####
########################################################
# wqual data first
USF12_pred <- merge(USF12_spec, USF12, by = c("DateTime"))
USF20_pred <- merge(USF20_spec, USF20, by = c("DateTime"))
USF21_pred <- merge(USF21_spec, USF21, by = c("DateTime"))

##############################
#### Flagging absorbances ####
##############################
### USF12 ###
# First check column numbers, look for spectral columns
data.frame(colnames(USF12_pred))

USF12_test <- USF12_pred %>%
  mutate(across(243, #add 27:227 to do that for spectral values
                list(clean = ~ if_else(. < 0 | . > 100, NA_real_, as.numeric(.))),
                .names = "{.col}_{.fn}"))

USF12_test <- USF12_test %>%
  mutate(across(243,
                list(flag  = ~ if_else(. < 0 | . > 100, TRUE, FALSE, missing = FALSE)),
                .names = "{.col}_{.fn}")) 

### USF20 ###
# First check column numbers, look for spectral columns
data.frame(colnames(USF20_pred))

USF20_test <- USF20_pred %>%
  mutate(across(241,
                list(clean = ~ if_else(. < 0 | . > 100, NA_real_, as.numeric(.))),
                .names = "{.col}_{.fn}"))

USF20_test <- USF20_test %>%
  mutate(across(241,
                list(flag  = ~ if_else(. < 0 | . > 100, TRUE, FALSE, missing = FALSE)),
                .names = "{.col}_{.fn}"))

### USF21 ###
# First check column numbers, look for spectral columns
data.frame(colnames(USF21_pred))
# Check which columns have non-numeric entries
sapply(USF21_pred[27:227], function(x) sum(is.na(as.numeric(as.character(x)))))

# Convert abs to numeric and non-numeric entries to NA.
USF21_pred <- USF21_pred %>%
  mutate(across(27:227, ~ suppressWarnings(as.numeric(as.character(.)))))

# first create the clean abs
USF21_test <- USF21_pred %>%
  mutate(across(243,
                list(clean = ~ if_else(. < 0 | . > 100, NA_real_, as.numeric(.))),
                .names = "{.col}_{.fn}"))

# then create the flag for the cleaned abs
USF21_test <- USF21_test %>%
  mutate(across(243,
                list(flag  = ~ if_else(. < 0 | . > 100, TRUE, FALSE, missing = FALSE)),
                .names = "{.col}_{.fn}"))

# ################################
# #### Plot clean absorbances ####
# ################################
# # Rename columns for all data frames
# rename_columns <- function(df) {
#   colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
#   return(df)
# }
# # Apply the renaming to each data frame
# USF12_test <- rename_columns(USF12_test)
# USF20_test <- rename_columns(USF20_test)
# USF21_test <- rename_columns(USF21_test)
# 
# # find spebtral data in df 
# data.frame(colnames(USF12_test))
# data.frame(colnames(USF20_test))
# data.frame(colnames(USF21_test))
# 
# scan.spec12 = USF12_test[244:444]
# scan.spec20 = USF20_test[242:442]
# scan.spec21 = USF21_test[244:444]
# 
# # Rows = wavelength
# # Columns = date/time
# abs12 = (scan.spec12)
# abs20 = (scan.spec20) 
# abs21 = (scan.spec21) 
# 
# wl12 <- gsub(".nm_clean", "", colnames(abs12))   
# wl12 <- as.numeric(wl12)
# wl20 <- gsub(".nm_clean", "", colnames(abs20))   
# wl20 <- as.numeric(wl20)
# wl21 <- gsub(".nm_clean", "", colnames(abs21))   
# wl21 <- as.numeric(wl21)
# 
# lastrow12 = as.numeric(nrow(abs12))
# Num12 = c(1:lastrow12)
# lastrow20 = as.numeric(nrow(abs20))
# Num20 = c(1:lastrow20)
# lastrow21 = as.numeric(nrow(abs21))
# Num21 = c(1:lastrow21)
# 
# #USF12
# scan.matrix12 = cbind(abs12)
# rownames(scan.matrix12) = as.numeric(Num12)
# colnames(scan.matrix12) = as.numeric(wl12)
# 
# scan.matrix12 = as.matrix(scan.matrix12)
# spec12 = spectra(value = abs12, bands = wl12, names = Num12)
# plot(spec12) # Note = reflectance here = absorbance from the scans
# 
# #USF20
# scan.matrix20 = cbind(abs20)
# rownames(scan.matrix20) = as.numeric(Num20)
# colnames(scan.matrix20) = as.numeric(wl20)
# 
# scan.matrix20 = as.matrix(scan.matrix20)
# spec20 = spectra(value = abs20, bands = wl20, names = Num20)
# plot(spec20) # Note = reflectance here = absorbance from the scans
# 
# #USF21
# scan.matrix21 = cbind(abs21)
# rownames(scan.matrix21) = as.numeric(Num21)
# colnames(scan.matrix21) = as.numeric(wl21)
# 
# scan.matrix21 = as.matrix(scan.matrix21)
# spec21 = spectra(value = abs21, bands = wl21, names = Num21)
# plot(spec21) # Note = reflectance here = absorbance from the scans

##########################
#### Clean a bit more ####
##########################
USF12_test <- USF12_test %>%
  mutate(DOC12.2.comps_clean = if_else(DOC12.2.comps_clean < 1.7, NA_real_, DOC12.2.comps_clean))
USF20_test <- USF20_test %>%
  mutate(DOC20.3.comps_clean = if_else(DOC20.3.comps_clean < 1.9, NA_real_, DOC20.3.comps_clean))
USF21_test <- USF21_test %>%
  mutate(DOC21.4.comps_clean = if_else(DOC21.4.comps_clean < 2, NA_real_, DOC21.4.comps_clean))

#### remove error section from USF20 ###
USF20_test <- USF20_test %>%
  mutate(across(
    c("DOC20.3.comps_clean", 242),
    ~ ifelse(between(DateTime, as.Date("2024-09-25"), as.Date("2024-10-17")), NA, .)
  ))

#### remove error section from USF20 ###
USF21_test <- USF21_test %>%
  mutate(across(
    c("DOC21.4.comps_clean", 244),
    ~ ifelse(between(DateTime, as.Date("2025-05-10"), as.Date("2025-05-20")), NA, .)
  ))

##################################
#### Plot clean predicted DOC ####
##################################
# Plot
ggplot(USF12_test, aes(x = DateTime, y = DOC12.2.comps_clean)) +
  geom_line(color = "steelblue") +
  labs(
    x = "DateTime",
    y = "Predicted DOC (mg/L)",
    title = "Predicted DOC over Time (USF12)"
  ) +
  theme_minimal()

ggplot(USF20_test, aes(x = DateTime, y = DOC20.3.comps_clean)) +
  geom_line(color = "steelblue") +
  labs(
    x = "DateTime",
    y = "Predicted DOC (mg/L)",
    title = "Predicted DOC over Time (USF20)"
  ) +
  theme_minimal()

ggplot(USF21_test, aes(x = DateTime, y = DOC21.4.comps_clean)) +
  geom_line(color = "steelblue") +
  labs(
    x = "DateTime",
    y = "Predicted DOC (mg/L)",
    title = "Predicted DOC over Time (USF21)"
  ) +
  theme_minimal()

#########################################
#### Remove some rows for others use ####
#########################################
USF12_ready <- USF12_test[,-c(2:16, 18:242)]
USF20_ready <- USF20_test[,-c(2:16, 18:240)]
USF21_ready <- USF21_test[,-c(2:16, 18:242)]

#######################
#### Save in Drive #### 
#######################
write.csv(USF12_ready, file = "predicted/PredictedC_USF12_clean.csv")
write.csv(USF20_ready, file = "predicted/PredictedC_USF20_clean.csv")
write.csv(USF21_ready, file = "predicted/PredictedC_USF21_clean.csv")

# Define the target folder ID in Google Drive
# This is the "predicted" folder
drive_folder_id <- "1wa1ycqUYv56y3fTn1-VaN2K-NLU3rFeU"

# Upload the file to the specified Google Drive folder
drive_put(media = "predicted/PredictedC_USF12_clean.csv", path = as_id(drive_folder_id))
drive_put(media = "predicted/PredictedC_USF20_clean.csv", path = as_id(drive_folder_id))
drive_put(media = "predicted/PredictedC_USF21_clean.csv", path = as_id(drive_folder_id))

