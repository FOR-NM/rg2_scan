##==============================================================================
## Project: QuEST
## This script cleans up time series scan data even further, removing outliers and other unwanted points
## Only do this if you want to clean data before calibrating, which is not always recommended
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(googledrive) 
library(dplyr)
library(tidyverse)

########################################
#### Clear folders that we will use ####
########################################
# List and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

##########################
#### Import scan data ####
##########################

#### Import abs and parameter data ####
# This is the "merged" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1llXcmKVhauTAHcnTuXuhhatPtEaMoeW2")

# List all the files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")

#NCB
googledrive::drive_download(file = merged$id[merged$name=="NCB_absparams_clean.csv"], 
                            path = "googledrive/NCB_absparams_clean.csv",
                            overwrite = T)
#LMP72
googledrive::drive_download(file = merged$id[merged$name=="LMP72_absparams_clean.csv"], 
                            path = "googledrive/LMP72_absparams_clean.csv",
                            overwrite = T)
#LMP27
googledrive::drive_download(file = merged$id[merged$name=="LMP27_absparams_clean.csv"], 
                            path = "googledrive/LMP27_absparams_clean.csv",
                            overwrite = T)

#CTB
googledrive::drive_download(file = merged$id[merged$name=="CTB_absparams_clean.csv"], 
                            path = "googledrive/CTB_absparams_clean.csv",
                            overwrite = T)

# Load them separately 
NCB <- read.csv("googledrive/NCB_absparams_clean.csv")
LMP72 <- read.csv("googledrive/LMP72_absparams_clean.csv")
LMP27 <- read.csv("googledrive/LMP27_absparams_clean.csv")
CTB <- read.csv("googledrive/CTB_absparams_clean.csv")

# Convert the datetime column to POSIXct
NCB$datetime <- as.POSIXct(NCB$datetime, format = "%Y-%m-%d %H:%M")
LMP72$datetime <- as.POSIXct(LMP72$datetime, format = "%Y-%m-%d %H:%M")
LMP27$datetime <- as.POSIXct(LMP27$datetime, format = "%Y-%m-%d %H:%M")
CTB$datetime <- as.POSIXct(CTB$datetime, format = "%Y-%m-%d %H:%M")

# Check for duplicates
sum(duplicated(NCB))
sum(duplicated(LMP72))
sum(duplicated(LMP27))
sum(duplicated(CTB))

##################
#### Clean up ####
##################

#### Clean values by standard deviation ####
# Define the columns to clean
columns_to_clean <- c("DOC_mg.l_clean", "NO3.N_mg.l_clean", "NO3_mg.l_clean", "TOC_mg.l_clean", "TSS_mg.l_clean")

# Define the number of standard deviations to consider as outliers
sd <- 4  # Adjust as needed

# ifelse() function checks whether the absolute deviation from the column's mean is greater than X times its standard deviation. 
# If this condition is true, the value is considered an outlier and replaced with NA; otherwise, the original value is retained.

# Apply cleaning CTB
CTB_clean <- CTB %>%
  mutate(TSS_clean = ifelse(abs(TSS_clean - mean(TSS_clean, na.rm = TRUE)) > 4 * sd(TSS_clean, na.rm = TRUE), 
                            NA, TSS_clean)) %>%
  mutate(TOC_clean = ifelse(abs(TOC_clean - mean(TOC_clean, na.rm = TRUE)) > 6 * sd(TOC_clean, na.rm = TRUE), 
                            NA, TOC_clean))

CTB_clean <- CTB %>%
  # pipes all of the columns in columns to clean
  mutate(across(all_of(columns_to_clean), 
                # . represents the current column being processed.
                # mean(., na.rm = TRUE) calculates the mean of the column, ignoring missing values (NA).
                # sd(., na.rm = TRUE) calculates the standard deviation of the column.
                # abs(. - mean(...)) computes the absolute deviation of each value from the column mean.
                .fns = ~ ifelse(abs(. - mean(., na.rm = TRUE)) > 2 * sd(., na.rm = TRUE), 
                                NA, 
                                .), 
                # names of the new columns where the cleaned values will be stored
                .names = "{.col}"))

# Apply cleaning LMP27
LMP27_clean <- LMP27 %>%
  # pipes all of the columns in columns to clean
  mutate(across(all_of(columns_to_clean), 
                # . represents the current column being processed.
                # mean(., na.rm = TRUE) calculates the mean of the column, ignoring missing values (NA).
                # sd(., na.rm = TRUE) calculates the standard deviation of the column.
                # abs(. - mean(...)) computes the absolute deviation of each value from the column mean.
                .fns = ~ ifelse(abs(. - mean(., na.rm = TRUE)) > 2 * sd(., na.rm = TRUE), 
                                NA, 
                                .), 
                # names of the new columns where the cleaned values will be stored
                .names = "{.col}"))

# Apply cleaning NCB
NCB_clean <- NCB %>%
  mutate(DOC_mg.l_clean = ifelse(abs(DOC_mg.l_clean - mean(DOC_mg.l_clean, na.rm = TRUE)) > 4 * sd(DOC_mg.l_clean, na.rm = TRUE), 
                            NA, DOC_mg.l_clean)) %>%
  mutate(NO3_mg.l_clean = ifelse(abs(NO3_mg.l_clean - mean(NO3_mg.l_clean, na.rm = TRUE)) > 2.5 * sd(NO3_mg.l_clean, na.rm = TRUE), 
                            NA, NO3_mg.l_clean)) %>%
  mutate(NO3.N_mg.l_clean = ifelse(abs(NO3.N_mg.l_clean - mean(NO3.N_mg.l_clean, na.rm = TRUE)) > 2.5 * sd(NO3.N_mg.l_clean, na.rm = TRUE), 
                          NA, NO3.N_mg.l_clean)) %>%
  mutate(TOC_mg.l_clean = ifelse(abs(TOC_mg.l_clean - mean(TOC_mg.l_clean, na.rm = TRUE)) > 4 * sd(TOC_mg.l_clean, na.rm = TRUE), 
                            NA, TOC_mg.l_clean)) %>%
  mutate(TSS_mg.l_clean = ifelse(abs(TSS_mg.l_clean - mean(TSS_mg.l_clean, na.rm = TRUE)) > 4 * sd(TSS_mg.l_clean, na.rm = TRUE), 
                            NA, TSS_mg.l_clean))

NCB_clean <- NCB %>%
  # pipes all of the columns in columns to clean
  mutate(across(all_of(columns_to_clean), 
                # . represents the current column being processed.
                # mean(., na.rm = TRUE) calculates the mean of the column, ignoring missing values (NA).
                # sd(., na.rm = TRUE) calculates the standard deviation of the column.
                # abs(. - mean(...)) computes the absolute deviation of each value from the column mean.
                .fns = ~ ifelse(abs(. - mean(., na.rm = TRUE)) > 2 * sd(., na.rm = TRUE), 
                                NA, 
                                .), 
                # names of the new columns where the cleaned values will be stored
                .names = "{.col}"))

#####################################
#### Plot all variables separate ####
#####################################

# Function to plot each variable separately in the same panel
plot_variables <- function(df) {
  # Ensure column selection works correctly
  df_long <- df %>%
    dplyr::select("datetime", "Temp_C", "TSS_mg.l_clean", "TOC_mg.l_clean", "NO3.N_mg.l_clean", "NO3_mg.l_clean", "DOC_mg.l_clean") %>%
    pivot_longer(cols = -datetime, names_to = "Variable", values_to = "Value")
  
  # Generate the plot
  ggplot(data = df_long, aes(x = datetime, y = Value, color = Variable)) +
    geom_line() +
    facet_wrap(~Variable, scales = "free_y", ncol = 1) +  # Separate plot for each variable, stacked vertically
    scale_x_datetime(date_breaks = "7 days", date_labels = "%m/%d") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Measured Value") +
    theme(legend.position = "none")  # Hide legend since we have separate panels
}

# Generate plots
print(plot_variables(CTB))
print(plot_variables(CTB_clean))

print(plot_variables(LMP27))
print(plot_variables(LMP27_clean))
 
print(plot_variables(NCB))
print(plot_variables(NCB_clean))

print(plot_variables(LMP72))
print(plot_variables(LMP72_clean))

#############################
#### Save filtered files ####
#############################

# Make sure it is in datetime format
SSM01_clean$DateTime <- format(SSM01_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(SSM01_clean,"googledrive/SSM01_filtered.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
SSM20_clean$DateTime <- format(SSM20_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(SSM20_clean,"googledrive/SSM20_filtered.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
SST13_clean$DateTime <- format(SST13_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(SST13_clean,"googledrive/SST13_filtered.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1qpsqrmcnALNS9OVtoIDICdEuW5LkVuIR"

# Upload the file to the specified Google Drive folder
drive_upload(media = "googledrive/SSM01_filtered.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/SSM20_filtered.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/SST13_filtered.csv", path = as_id(drive_folder_id))

