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
# This is the "clean" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1BNCKA7LdysjDH5_REI4WhH_P0Z4FIe0r")

# List all xlsx files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")
3

#SSM01
googledrive::drive_download(file = merged$id[merged$name=="SSM01_merged_clean.csv"], 
                            path = "googledrive/SSM01_merged_clean.csv",
                            overwrite = T)
#SSM20
googledrive::drive_download(file = merged$id[merged$name=="SSM20_merged_clean.csv"], 
                            path = "googledrive/SSM20_merged_clean.csv",
                            overwrite = T)
#SST13
googledrive::drive_download(file = merged$id[merged$name=="SST13_merged_clean.csv"], 
                            path = "googledrive/SST13_merged_clean.csv",
                            overwrite = T)

# Let's load them separately first
SSM01 <- read.csv("googledrive/SSM01_merged_clean.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
SSM20 <- read.csv("googledrive/SSM20_merged_clean.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
SST13 <- read.csv("googledrive/SST13_merged_clean.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)

# DateTime at midnight is missing 00:00:00 time, so filling in that time using grep                     
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

# Rename columns for all data frames
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}

# Apply the renaming to each data frame
SSM01 <- rename_columns(SSM01)
SSM20 <- rename_columns(SSM20)
SST13 <- rename_columns(SST13)

####----------------------------------------------------------------------####
# ----------------------------------------------------------------------
# 2. Extract Unique Month-Year Combinations
# ----------------------------------------------------------------------
# Combine month name and year to get a unique identifier (e.g., "July 2024", "August 2024")
# We use SSM01 to determine the months, assuming SSM20 and SST13 cover the same period.

SSM01$MonthYear <- format(SSM01$DateTime, "%B %Y")
unique_months <- unique(SSM01$MonthYear)

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
  scan.spec <- data_month[23:243]
  
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

all_data_frames <- list(SSM01 = SSM01, SSM20 = SSM20, SST13 = SST13)

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
SSM01$MonthYear <- NULL # Remove the temporary column

####----------------------------------------------------------------------####

