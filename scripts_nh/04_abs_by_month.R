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
# This is the "abs and params" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1CeCmX0mGh1wZ3IL4Exu4oPHUuzYOk4T_")

# List all the files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")

#CTB
googledrive::drive_download(file = merged$id[merged$name=="CTB_absparams.csv"], 
                            path = "googledrive/CTB_absparams.csv",
                            overwrite = T)
#SBM
googledrive::drive_download(file = merged$id[merged$name=="SMB_absparams.csv"], 
                            path = "googledrive/SMB_absparams.csv",
                            overwrite = T)
#NCBd
googledrive::drive_download(file = merged$id[merged$name=="NCB_absparams.csv"], 
                            path = "googledrive/NCB_absparams.csv",
                            overwrite = T)
#LMP07
googledrive::drive_download(file = merged$id[merged$name=="LMP07_absparams.csv"], 
                            path = "googledrive/LMP07_absparams.csv",
                            overwrite = T)
#LMP27
googledrive::drive_download(file = merged$id[merged$name=="LMP27_absparams.csv"], 
                            path = "googledrive/LMP27_absparams.csv",
                            overwrite = T)
#LMP72
googledrive::drive_download(file = merged$id[merged$name=="LMP72_absparams.csv"], 
                            path = "googledrive/LMP72_absparams.csv",
                            overwrite = T)
# Load them separately
CTB <- read.csv("googledrive/CTB_absparams.csv")
SMB <- read.csv("googledrive/SMB_absparams.csv")
NCBd <- read.csv("googledrive/NCB_absparams.csv")
LMP07 <- read.csv("googledrive/LMP07_absparams.csv")
LMP27 <- read.csv("googledrive/LMP27_absparams.csv")
LMP72 <- read.csv("googledrive/LMP72_absparams.csv")

# DateTime at midnight is missing 00:00:00 time, so filling in that time using grep                     
USF12$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF12$DateTime)] <- paste(
  USF12$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF12$DateTime)],"00:00:00")
USF20$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF20$DateTime)] <- paste(
  USF20$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF20$DateTime)],"00:00:00")
USF21$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF21$DateTime)] <- paste(
  USF21$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF21$DateTime)],"00:00:00")

# Convert the DateTime column to POSIXct
CTB$DateTime <- as.POSIXct(CTB$DateTime, format = "%Y-%m-%d %H:%M:%S")
SMB$DateTime <- as.POSIXct(SMB$DateTime, format = "%Y-%m-%d %H:%M:%S")
NCBd$DateTime <- as.POSIXct(NCBd$DateTime, format = "%Y-%m-%d %H:%M:%S")
LMP07$DateTime <- as.POSIXct(LMP07$DateTime, format = "%Y-%m-%d %H:%M:%S")
LMP27$DateTime <- as.POSIXct(LMP27$DateTime, format = "%Y-%m-%d %H:%M:%S")
LMP72$DateTime <- as.POSIXct(LMP72$DateTime, format = "%Y-%m-%d %H:%M:%S")

# Rename columns for all data frames
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}

# Apply the renaming to each data frame
CTB <- rename_columns(CTB)
SMB <- rename_columns(SMB)
NCBd <- rename_columns(NCBd)
LMP07 <- rename_columns(LMP07)
LMP27 <- rename_columns(LMP27)
LMP72 <- rename_columns(LMP72)

####----------------------------------------------------------------------####
# ----------------------------------------------------------------------
# 2. Extract Unique Month-Year Combinations
# ----------------------------------------------------------------------
# Combine month name and year to get a unique identifier (e.g., "July 2024", "August 2024")
# We use USF12 to determine the months, assuming USF20 and USF21 cover the same period.

CTB$MonthYear <- format(CTB$DateTime, "%B %Y")
unique_months <- unique(CTB$MonthYear)

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
  scan.spec <- data_month[24:225]
  
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
all_data_frames <- list(CTB = CTB, NCBd = NCBd, SMB = SMB, LMP07 = LMP07, LMP27 = LMP27, LMP72 = LMP72)

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
USF12$MonthYear <- NULL # Remove the temporary column

####----------------------------------------------------------------------####

