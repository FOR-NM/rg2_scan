##==============================================================================
## Project: QuEST
## This script will help you if you have to use the moving window approach to do the calibration
## Then you can flag spectra if they are too low or too high 
##==============================================================================

library(dplyr)
library(spectrolab)
library(googledrive) 

##############################################
#### Upload scan dataframe [with spectra] ####
##############################################
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

# Rename columns for all data frames
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}

# Apply the renaming to each data frame
DVO <- rename_columns(DVO)
DVMS1 <- rename_columns(DVMS1)
DVNWT5 <- rename_columns(DVNWT5)

################################################
#### Edit data to look at it month by month ####
################################################
DVO_month <- DVO %>%
  filter(format(DateTime, "%B") == "July")
DVMS1_month <- DVMS1 %>%
  filter(format(DateTime, "%B") == "July")
DVNWT5_month <- DVNWT5 %>%
  filter(format(DateTime, "%B") == "July")

################################################################################
#### Create matrices of ALL spectral data - raw data that needs calibration ####
################################################################################
# 1. Index FULL dataset with columns with absorbances
scan.spec12 = DVO_month[15:224] # change for DVO_month to see by month 
scan.spec20 = DVMS1_month[15:224]
scan.spec21 = DVNWT5_month[15:224]

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
abs12 = (scan.spec12)
abs20 = (scan.spec20) 
abs21 = (scan.spec21) 

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wl12 = as.numeric(colnames(abs12))
wl20 = as.numeric(colnames(abs20))
wl21 = as.numeric(colnames(abs21))

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

####----------------------------------------------------------------------####
# ----------------------------------------------------------------------
# 2. Extract Unique Month-Year Combinations
# ----------------------------------------------------------------------
# Combine month name and year to get a unique identifier (e.g., "July 2024", "August 2024")
# We use DVO to determine the months, assuming DVMS1 and DVNWT5 cover the same period.

DVO$MonthYear <- format(DVO$DateTime, "%B %Y")
unique_months <- unique(DVO$MonthYear)

# ----------------------------------------------------------------------
# 3. Core Function to Process and Plot Data for a Single Month
# ----------------------------------------------------------------------

process_and_plot_month <- function(data_frame, df_name, month_year_label) {
  
  # a. Filter data for the specific month
  data_month <- data_frame %>%
    dplyr::filter(format(DateTime, "%B %Y") == month_year_label)
  
  # Check if there is any data for this month
  if (nrow(data_month) == 0) {
    message(paste("Skipping", df_name, "for", month_year_label, "- No data found."))
    return(NULL)
  }
  
  # b. Index spectral data (columns 19 to 228, as in your original script)
  scan.spec <- data_month[15:224]
  
  # c. Create absorbance object
  abs_data <- scan.spec
  
  # d. Create wavelength vector
  wl <- as.numeric(colnames(abs_data))
  
  # e. Create sample vector
  last_row <- as.numeric(nrow(abs_data))
  num_samples <- c(1:last_row)
  
  # f. Create the spectra object and plot
  # NOTE: The 'spectra' object is required for the 'plot' function from 'spectrolab'
  spec_object <- spectra(value = abs_data, bands = wl, names = num_samples)
  
  # g. Create the plot object
  # We use the base plot function from your original code, which works well for spectra objects.
  p <- plot(spec_object, main = paste("Absorbance Spectra for", df_name, "-", month_year_label))
  
  # h. Save the plot using ggsave (if you use ggplot) or pdf/png devices (for base plot)
  # Since you are using a custom 'plot' function (likely base R/spectrolab),
  # we must use a device function like png() or pdf() to save the plot.
  
  filename <- paste0(df_name, "_Absorbance_", gsub(" ", "_", month_year_label), ".png")
  
  png(filename = filename, width = 800, height = 600) # Opens a PNG graphics device
  plot(spec_object, main = paste("Absorbance Spectra for", df_name, "-", month_year_label))
  dev.off() # Closes the device and saves the file
  
  message(paste("Successfully saved plot:", filename))
}

# ----------------------------------------------------------------------
# 4. Loop Through All Months and Datasets
# ----------------------------------------------------------------------

all_data_frames <- list(DVO = DVO, DVMS1 = DVMS1, DVNWT5 = DVNWT5)

for (month in unique_months) {
  for (df_name in names(all_data_frames)) {
    
    # Check if the data frame for this name exists in the environment
    if (exists(df_name)) {
      # Use get() to retrieve the data frame object from the environment
      df_object <- get(df_name) 
      
      # Execute the function
      process_and_plot_month(data_frame = df_object, 
                             df_name = df_name, 
                             month_year_label = month)
    } else {
      warning(paste("Data frame", df_name, "not found in the environment. Skipping."))
    }
  }
}

# ----------------------------------------------------------------------
# Cleanup (optional)
# ----------------------------------------------------------------------
DVO$MonthYear <- NULL # Remove the temporary column

####----------------------------------------------------------------------####

