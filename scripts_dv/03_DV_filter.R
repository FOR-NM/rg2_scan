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
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1C7Z6v38dyhLKO6kGNzcjSRfTDl64j0WC")

# List all the files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")

#DVO
googledrive::drive_download(file = merged$id[merged$name=="02_DVO_absparams_clean.csv"], 
                            path = "googledrive/02_DVO_absparams_clean.csv",
                            overwrite = T)
#DVMS1
googledrive::drive_download(file = merged$id[merged$name=="02_DVMS1_absparams_clean.csv"], 
                            path = "googledrive/02_DVMS1_absparams_clean.csv",
                            overwrite = T)
#DVNWT5
googledrive::drive_download(file = merged$id[merged$name=="02_DVNWT5_absparams_clean.csv"], 
                            path = "googledrive/02_DVNWT5_absparams_clean.csv",
                            overwrite = T)

# Load them separately 
DVO <- read.csv("googledrive/02_DVO_absparams_clean.csv")
DVMS1 <- read.csv("googledrive/02_DVMS1_absparams_clean.csv")
DVNWT5 <- read.csv("googledrive/02_DVNWT5_absparams_clean.csv")

# Convert the DateTime column to POSIXct
DVO$DateTime <- as.POSIXct(DVO$DateTime, format = "%Y-%m-%d %H:%M")
DVMS1$DateTime <- as.POSIXct(DVMS1$DateTime, format = "%Y-%m-%d %H:%M")
DVNWT5$DateTime <- as.POSIXct(DVNWT5$DateTime, format = "%Y-%m-%d %H:%M")

# Check for duplicates
sum(duplicated(DVO))
sum(duplicated(DVMS1))
sum(duplicated(DVNWT5))

##################
#### Clean up ####
##################

# #### Keep rows with only 15-minute intervals ####
# DVO <- DVO %>%
#   filter(format(DVO$DateTime, "%M") %in% c("00", "15", "30", "45"))
# 
# DVMS1 <- DVMS1 %>%
#   filter(format(DVMS1$DateTime, "%M") %in% c("00", "15", "30", "45"))
# 
# DVNWT5 <- DVNWT5 %>%
#   filter(format(DVNWT5$DateTime, "%M") %in% c("00", "15", "30", "45"))

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

# Apply cleaning DVO
DVO_clean <- DVO %>%
  mutate(TSS_mg.l_clean = ifelse(abs(TSS_mg.l_clean - mean(TSS_mg.l_clean, na.rm = TRUE)) > 4 * sd(TSS_mg.l_clean, na.rm = TRUE), 
                            NA, TSS_mg.l_clean)) %>%
  mutate(TOC_mg.l_clean = ifelse(abs(TOC_mg.l_clean - mean(TOC_mg.l_clean, na.rm = TRUE)) > 6 * sd(TOC_mg.l_clean, na.rm = TRUE), 
                            NA, TOC_mg.l_clean))  %>%
  mutate(NO3_mg.l_clean = ifelse(abs(NO3_mg.l_clean - mean(NO3_mg.l_clean, na.rm = TRUE)) > 7 * sd(NO3_mg.l_clean, na.rm = TRUE),
                                 NA, NO3_mg.l_clean)) %>%
  mutate(NO3.N_mg.l_clean = ifelse(abs(NO3.N_mg.l_clean - mean(NO3.N_mg.l_clean, na.rm = TRUE)) > 7 * sd(NO3.N_mg.l_clean, na.rm = TRUE),
                                   NA, NO3.N_mg.l_clean)) 

# Apply cleaning DVMS1
DVMS1_clean <- DVMS1 %>%
  mutate(DOC_mg.l_clean = ifelse(abs(DOC_mg.l_clean - mean(DOC_mg.l_clean, na.rm = TRUE)) > 5 * sd(DOC_mg.l_clean, na.rm = TRUE), 
                                 NA, DOC_mg.l_clean)) %>%
  mutate(NO3_mg.l_clean = ifelse(abs(NO3_mg.l_clean - mean(NO3_mg.l_clean, na.rm = TRUE)) > 5 * sd(NO3_mg.l_clean, na.rm = TRUE), 
                                 NA, NO3_mg.l_clean)) %>%
  mutate(NO3.N_mg.l_clean = ifelse(abs(NO3.N_mg.l_clean - mean(NO3.N_mg.l_clean, na.rm = TRUE)) > 5 * sd(NO3.N_mg.l_clean, na.rm = TRUE), 
                                   NA, NO3.N_mg.l_clean)) %>%
  mutate(TOC_mg.l_clean = ifelse(abs(TOC_mg.l_clean - mean(TOC_mg.l_clean, na.rm = TRUE)) > 5 * sd(TOC_mg.l_clean, na.rm = TRUE), 
                                 NA, TOC_mg.l_clean)) %>%
  mutate(TSS_mg.l_clean = ifelse(abs(TSS_mg.l_clean - mean(TSS_mg.l_clean, na.rm = TRUE)) > 4 * sd(TSS_mg.l_clean, na.rm = TRUE), 
                                 NA, TSS_mg.l_clean))

# Apply cleaning DVNWT5
DVNWT5_clean <- DVNWT5 %>%
  mutate(DOC_mg.l_clean = ifelse(abs(DOC_mg.l_clean - mean(DOC_mg.l_clean, na.rm = TRUE)) > 5 * sd(DOC_mg.l_clean, na.rm = TRUE), 
                            NA, DOC_mg.l_clean)) %>%
  mutate(NO3_mg.l_clean = ifelse(abs(NO3_mg.l_clean - mean(NO3_mg.l_clean, na.rm = TRUE)) > 5 * sd(NO3_mg.l_clean, na.rm = TRUE), 
                            NA, NO3_mg.l_clean)) %>%
  mutate(NO3.N_mg.l_clean = ifelse(abs(NO3.N_mg.l_clean - mean(NO3.N_mg.l_clean, na.rm = TRUE)) > 5 * sd(NO3.N_mg.l_clean, na.rm = TRUE), 
                          NA, NO3.N_mg.l_clean)) %>%
  mutate(TOC_mg.l_clean = ifelse(abs(TOC_mg.l_clean - mean(TOC_mg.l_clean, na.rm = TRUE)) > 5 * sd(TOC_mg.l_clean, na.rm = TRUE), 
                            NA, TOC_mg.l_clean)) %>%
  mutate(TSS_mg.l_clean = ifelse(abs(TSS_mg.l_clean - mean(TSS_mg.l_clean, na.rm = TRUE)) > 5 * sd(TSS_mg.l_clean, na.rm = TRUE), 
                            NA, TSS_mg.l_clean))

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
print(plot_variables(DVO))
print(plot_variables(DVO_clean))

print(plot_variables(DVMS1))
print(plot_variables(DVMS1_clean))
 
print(plot_variables(DVNWT5))
print(plot_variables(DVNWT5_clean))

#############################
#### Save filtered files ####
#############################

# Make sure it is in datetime format
DVO_clean$DateTime <- format(DVO_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(DVO_clean,"googledrive/03_DVO_filtered.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
DVMS1_clean$DateTime <- format(DVMS1_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(DVMS1_clean,"googledrive/03_DVMS1_filtered.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
DVNWT5_clean$DateTime <- format(DVNWT5_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(DVNWT5_clean,"googledrive/03_DVNWT5_filtered.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1C7Z6v38dyhLKO6kGNzcjSRfTDl64j0WC"

# Upload the file to the specified Google Drive folder
drive_upload(media = "googledrive/03_DVO_filtered.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/03_DVMS1_filtered.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/03_DVNWT5_filtered.csv", path = as_id(drive_folder_id))

