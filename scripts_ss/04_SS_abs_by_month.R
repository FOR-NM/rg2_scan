##==============================================================================
## Project: QuEST
## This script will help you if you have to use the moving window approach to do the calibration
## Then you can flag spectra if they are too low or too high 
##==============================================================================

library(dplyr)
library(spectrolab)

##############################################
#### Upload scan dataframe [with spectra] ####
##############################################
# This data is already matched #
# This is the "params and abs" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1WbfZWpSeXVLoSEvxqbVnjgvgo4uUwGtm")

# List all xlsx files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")
3

#SSM01
googledrive::drive_download(file = merged$id[merged$name=="SSM01_absparams.csv"], 
                            path = "googledrive/SSM01_absparams.csv",
                            overwrite = T)
#SSM20
googledrive::drive_download(file = merged$id[merged$name=="SSM20_absparams.csv"], 
                            path = "googledrive/SSM20_absparams.csv",
                            overwrite = T)
#SST13
googledrive::drive_download(file = merged$id[merged$name=="SST13_absparams.csv"], 
                            path = "googledrive/SST13_absparams.csv",
                            overwrite = T)

# Let's load them separately first
SSM01 <- read.csv("googledrive/SSM01_absparams.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
SSM20 <- read.csv("googledrive/SSM20_absparams.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
SST13 <- read.csv("googledrive/SST13_absparams.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)

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

# cleaning
SSM01 <- SSM01[, -1]
SSM20 <- SSM20[, -1]
SST13 <- SST13[, -1]

# List of your data frames
df_names <- c("SSM01", "SSM20", "SST13")

for (name in df_names) {
  if (exists(name)) {
    df <- get(name)
    
    # Identify which columns are wavelengths (names starting with numbers)
    spec_cols <- grep("^[0-9]", colnames(df))
    
    # Force those columns to be numeric
    # We use as.character first just in case they were imported as factors
    df <- df %>%
      mutate(across(all_of(spec_cols), ~as.numeric(as.character(.))))
    
    # Assign it back to the environment
    assign(name, df)
  }
}

####----------------------------------------------------------------------####
# ----------------------------------------------------------------------
# 2. Extract Unique Month-Year Combinations
# ----------------------------------------------------------------------
# Combine month name and year to get a unique identifier (e.g., "July 2024", "August 2024")
# We use SSM20 to determine the months, assuming SSM01 and SST13 cover the same period.

SSM20$MonthYear <- format(SSM20$DateTime, "%B %Y")
unique_months <- unique(SSM20$MonthYear)

# ----------------------------------------------------------------------
# 3. Core Function to Process and Plot Data for a Single Month
# ----------------------------------------------------------------------
process_and_plot_month <- function(data_frame, df_name, month_year_label) {
  
  # a. Filter data for the specific month
  data_month <- data_frame %>%
    dplyr::filter(format(DateTime, "%B %Y") == month_year_label)
  
  # Check 1a: Is the data frame physically empty?
  if (nrow(data_month) == 0) {
    message(paste("Skipping", df_name, "for", month_year_label, "- No data found."))
    return(NULL)
  }
  
  # b. Index spectral data dynamically
  # This selects only columns whose names are numbers (the wavelengths)
  scan.spec <- data_month %>% 
    dplyr::select(matches("^[0-9]"))
  
  # Check if we actually found spectral columns
  if (ncol(scan.spec) == 0) {
    message(paste("Skipping", df_name, "for", month_year_label, "- No spectral columns found."))
    return(NULL)
  }
  
  # --- NEW CHECK 1b: Is there actually numeric data? ---
  # This checks if the entire spectral matrix is just NAs
  if (all(is.na(scan.spec))) {
    message(paste("Skipping", df_name, "for", month_year_label, "- Data contains only NAs."))
    return(NULL)
  }
  
  # Check if there are any finite values (not Inf or -Inf)
  if (!any(is.finite(as.matrix(scan.spec)))) {
    message(paste("Skipping", df_name, "for", month_year_label, "- No finite values to plot."))
    return(NULL)
  }
  # ----------------------------------------------------
  
  # d. Create wavelength vector
  wl <- as.numeric(colnames(scan.spec))
  
  # If your column names are "200.00", "202.50", as.numeric works perfectly.
  # If there are any stray characters, this catches them:
  if (any(is.na(wl))) {
    # This keeps only columns where the NAME is a valid number
    valid_cols <- !is.na(as.numeric(colnames(scan.spec)))
    scan.spec <- scan.spec[, valid_cols]
    wl <- as.numeric(colnames(scan.spec))
  }
  
  # Verify wl doesn't have NAs (from previous error)
  if (any(is.na(wl))) {
    valid_wl <- !is.na(wl)
    scan.spec <- scan.spec[, valid_wl]
    wl <- wl[valid_wl]
  }
  
  # f. Create the spectra object
  num_samples <- 1:nrow(scan.spec)
  spec_object <- spectra(value = scan.spec, bands = wl, names = num_samples)
  
  # g. Save the plot
  filename <- paste0(df_name, "_Absorbance_", gsub(" ", "_", month_year_label), ".png")
  
  png(filename = filename, width = 800, height = 600)
  # Use try() so a single bad plot doesn't stop the whole loop
  try(plot(spec_object, main = paste("Absorbance Spectra for", df_name, "-", month_year_label)))
  dev.off()
  
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

