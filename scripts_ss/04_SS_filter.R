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
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1qpsqrmcnALNS9OVtoIDICdEuW5LkVuIR")

# List all the files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")

#SSM20
googledrive::drive_download(file = merged$id[merged$name=="SSM20_absparams_clean.csv"], 
                            path = "googledrive/SSM20_absparams_clean.csv",
                            overwrite = T)
#SST13
googledrive::drive_download(file = merged$id[merged$name=="SST13_absparams_clean.csv"], 
                            path = "googledrive/SST13_absparams_clean.csv",
                            overwrite = T)
#SSM01
googledrive::drive_download(file = merged$id[merged$name=="SSM01_absparams_clean.csv"], 
                            path = "googledrive/SSM01_absparams_clean.csv",
                            overwrite = T)

# Load them separately 
SSM20 <- read.csv("googledrive/SSM20_absparams_clean.csv")
SST13 <- read.csv("googledrive/SST13_absparams_clean.csv")
SSM01 <- read.csv("googledrive/SSM01_absparams_clean.csv")

# Convert the DateTime column to POSIXct
SSM20$DateTime <- as.POSIXct(SSM20$DateTime, format = "%Y-%m-%d %H:%M")
SST13$DateTime <- as.POSIXct(SST13$DateTime, format = "%Y-%m-%d %H:%M")
SSM01$DateTime <- as.POSIXct(SSM01$DateTime, format = "%Y-%m-%d %H:%M")

# Check for duplicates
sum(duplicated(SSM20))
sum(duplicated(SST13))
sum(duplicated(SSM01))

##################
#### Clean up ####
##################
# #### Keep rows with only 15-minute intervals ####
# SSM20 <- SSM20 %>%
#   filter(format(SSM20$DateTime, "%M") %in% c("00", "15", "30", "45"))
# 
# SST13 <- SST13 %>%
#   filter(format(SST13$DateTime, "%M") %in% c("00", "15", "30", "45"))
# 
# SSM01 <- SSM01 %>%
#   filter(format(SSM01$DateTime, "%M") %in% c("00", "15", "30", "45"))

# #### Remove error section from USF20 ####
# USF20 <- USF20 %>%
#   mutate(across(
#     c("DOC_clean", "NO3.N_clean", "NO3_clean", "TOC_clean", "TSS_clean", 21:230),
#     ~ ifelse(between(DateTime, as.Date("2024-09-25"), as.Date("2024-10-17")), NA, .)
#   ))

# #### Remove low volt at end of USF21 ####
# USF21 <- USF21[-c(11889:11961),]
# 
# USF21 <- USF21 %>%
#   mutate(across(
#     c("DOC_clean", "NO3.N_clean", "NO3_clean", "TOC_clean", "TSS_clean"),
#     ~ if_else(row_number() %in% c(1812, 97, 1810), NA, .)
#   ))


#### Clean values by standard deviation ####
# Define the columns to clean
columns_to_clean <- c("DOC_mg.l_clean", "NO3.N_mg.l_clean", "NO3_mg.l_clean", "TOC_mg.l_clean", "TSS_mg.l_clean")

# Define the number of standard deviations to consider as outliers
sd <- 4  # Adjust as needed

# ifelse() function checks whether the absolute deviation from the column's mean is greater than X times its standard deviation. 
# If this condition is true, the value is considered an outlier and replaced with NA; otherwise, the original value is retained.

# Apply cleaning SSM20
SSM20_clean <- SSM20 %>%
  mutate(TSS_clean = ifelse(abs(TSS_clean - mean(TSS_clean, na.rm = TRUE)) > 4 * sd(TSS_clean, na.rm = TRUE), 
                            NA, TSS_clean)) %>%
  mutate(TOC_clean = ifelse(abs(TOC_clean - mean(TOC_clean, na.rm = TRUE)) > 6 * sd(TOC_clean, na.rm = TRUE), 
                            NA, TOC_clean))

SSM20_clean <- SSM20 %>%
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

# Apply cleaning SST13
SST13_clean <- SST13 %>%
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

# Apply cleaning SSM01
SSM01_clean <- SSM01 %>%
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

SSM01_clean <- SSM01 %>%
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
    dplyr::select("DateTime", "Temp_C", "TSS_mg.l_clean", "TOC_mg.l_clean", "NO3.N_mg.l_clean", "NO3_mg.l_clean", "DOC_mg.l_clean") %>%
    pivot_longer(cols = -DateTime, names_to = "Variable", values_to = "Value")
  
  # Generate the plot
  ggplot(data = df_long, aes(x = DateTime, y = Value, color = Variable)) +
    geom_line() +
    facet_wrap(~Variable, scales = "free_y", ncol = 1) +  # Separate plot for each variable, stacked vertically
    scale_x_datetime(date_breaks = "7 days", date_labels = "%m/%d") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Measured Value") +
    theme(legend.position = "none")  # Hide legend since we have separate panels
}

# Generate plots
print(plot_variables(SSM01))
print(plot_variables(SSM01_clean))

print(plot_variables(SSM20))
print(plot_variables(SSM20_clean))
 
print(plot_variables(SST13))
print(plot_variables(SST13_clean))

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

