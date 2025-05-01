##==============================================================================
## Project: QuEST
## Here we will prep grab sample data by matching the grab samples with the time stamp of the s::can for South Sandy
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(googledrive) 
library(googlesheets4)
library(dplyr)
library(readxl)
library(tidyverse)
library(lubridate) 
library(hms) # to fix time from xlsx

########################################
#### Clear folders that we will use ####
########################################
# List and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

##########################
#### Import chem data ####
##########################

#### Load chem data ####
# Chem data is for all the sites
chem <- googledrive::as_id("https://drive.google.com/drive/folders/1ZCVAoIamyMMtwh-Cy3SpeQx2IWYu6gg2")

# List all CSV files in the folder
chem_csv <- googledrive::drive_ls(path = chem, type = "csv")
3

# call the specific file you want (most recent one)
googledrive::drive_download(file = chem_csv$id[chem_csv$name=="2024-12-17_chem_data.csv"], 
                            path = "googledrive/2024-12-17_chem_data.csv",
                            overwrite = T)
# load it into R
wqual = read.csv("googledrive/2024-12-17_chem_data.csv")

# Filter to get just DV data
DV <- filter(wqual, Sub_Project == "Nevada")

# Format date columns
DV$Collection.Date <- as.Date(DV$Collection.Date, format = "%Y-%m-%d")

# Rename Collection Date column
DV <- DV %>% rename(Date = Collection.Date)

# Clean up a bit
drops <- c("X", "Project", "Sub_ProjectA", "pH", "Cond", "Spec_Cond", "DO_Conc",  "DO.", "Temperature Turbidity", "ID")
DV <- DV[ , !(names(DV) %in% drops)]

#### Combine same day-same site samples (reps and bottles) ####
# When there are reps per site per date we need to average them and use the average chem to calculate leverage
head(DV)
# Define the columns that need to be averaged
columns_to_average <- c("NPOC..mg.C.L.", "NO3..mg.N.L.", "NH4..ug.N.L.", "TDN..mg.N.L.", 
                        "PO4..ug.P.L.", "Cl..mg.Cl.L.", "SO4..mg.S.L.", "Na..mg.Na.L.", 
                        "K..mg.K.L.", "Mg..mg.Mg.L.", "Ca..mg.Ca.L.")

# Calculate averages or fill non-NA values for each Site and Date
data_avg <- DV %>%
  # Group by columns Date and Site, and other unique identifiers if necessary
  group_by(Date, Sample.Name) %>%
  
  # Summarize: for each column, take the mean if there are multiple values or the single non-NA value
  summarise(across(all_of(columns_to_average),
                   ~ if (all(is.na(.))) NA_real_ else mean(., na.rm = TRUE)),  # Calculate mean if there are values
            Sample.Name = paste0(first(Sample.Name), "_", first(Date), "_Avg"),    # Create a new Sample Name with _Avg
            .groups = "drop") # Ungroup to avoid nested data frames


# Count non-NA values in Q column using dplyr
nonna_counts_dplyr <- data_avg %>%
  summarise_all(~ sum(!is.na(.)))

#############################################################
#### Load sample info to get grab sample collection time ####
#############################################################
# Set the Google Sheets file ID
file_id <- "https://docs.google.com/spreadsheets/d/1QrCUc8WeSiLUtAxmhMi097pxgaJAF2Rr/edit?gid=1404002126#gid=1404002126"

# Download the Google Sheet as an Excel file (.xlsx)
drive_download(as_id(file_id), 
               path = "googledrive/2024_field_season.xlsx", 
               type = "xlsx", 
               overwrite = TRUE)

# Read the Excel file
times <- read_excel("googledrive/2024_field_season.xlsx")

# Fix date 
str(times$Date)  # Check structure
class(times$Date)  # Check specific class
times$Date <- as.Date(as.numeric(times$Date), origin = "1899-12-30")

# Fix time
times$Start_Time <- hms::as_hms(times$Start_Time)

#### This is if you want to change sample time to fit scan time ####
###DVO###
# times$Start_Time[times$`Site ID` == "DVO" &
#                    times$Date == "2024-05-23" &
#                    times$Start_Time == "09:30:00"] <- "09:45:00"

##################################
#### Rounding collection time ####
##################################

# Create extra columns so you don't erase original time               
times$TimeNotRounded <- times$Start_Time

# Combine Date and Time columns into a new DateTime column
times$DateTime <- paste(times$Date, times$Start_Time, sep = " ")
# Convert the DateTime column to POSIXct
times$DateTime <- as.POSIXct(times$DateTime, format = "%Y-%m-%d %H:%M")

# Round DateTime to the nearest 15-minute interval 
times$DateTime <- round_date(times$DateTime, unit="15 mins")

# Check if it worked!
str(times)

#######################################################################
#### Merge chem and sample log sheet to get sample collection time ####
#######################################################################

# filter only data for DVO, DVMS1 and DVNW5 (scan sites) 
wqual_scans <- data_avg %>% filter(Site %in% c("DVO", "DVMS1", "DVNW5"))

# merge wqual data first
sample_times <- merge(wqual_scans, samplelogsheet, by = c("Date", "Site"))

# Check for duplicates in the original datasets
sum(duplicated(sample_times))
# Remove duplicates from the original datasets
# sample_times <- sample_times %>% distinct()

##########################
#### Import scan data ####
##########################

#### Import abs and parameter data ####
# This is the "merged" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1qpsqrmcnALNS9OVtoIDICdEuW5LkVuIR")

# List all the files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")

#SSM01
googledrive::drive_download(file = merged$id[merged$name=="SSM01_filtered.csv"], 
                            path = "googledrive/SSM01_filtered.csv",
                            overwrite = T)
#SSM20
googledrive::drive_download(file = merged$id[merged$name=="SSM20_filtered.csv"], 
                            path = "googledrive/SSM20_filtered.csv",
                            overwrite = T)
#SST13
googledrive::drive_download(file = merged$id[merged$name=="SST13_filtered.csv"], 
                            path = "googledrive/SST13_filtered.csv",
                            overwrite = T)

# Load them separately 
SSM01 <- read.csv("googledrive/SSM01_filtered.csv")
SSM20 <- read.csv("googledrive/SSM20_filtered.csv")
SST13 <- read.csv("googledrive/SST13_filtered.csv")

# Convert the DateTime column to POSIXct
SSM01$DateTime <- as.POSIXct(SSM01$DateTime, format = "%Y-%m-%d %H:%M")
SSM20$DateTime <- as.POSIXct(SSM20$DateTime, format = "%Y-%m-%d %H:%M")
SST13$DateTime <- as.POSIXct(SST13$DateTime, format = "%Y-%m-%d %H:%M")

# Check for duplicates
sum(duplicated(SSM01))
sum(duplicated(SSM20))
sum(duplicated(SST13))

##################################
#### Merge chem and scan data ####
##################################

# Filter to get just one site at a time
U01 <- filter(sample_times, Site == "SSM01")
U20 <- filter(sample_times, Site == "SSM20")
U13 <- filter(sample_times, Site == "SST13")

# First check if the merge works
dat01 <- merge(SSM01, U01, by = "DateTime")
dat20 <- merge(SSM20, U20, by = "DateTime")
dat13 <- merge(SST13, U13, by = "DateTime")

# scan data first - perform a left join
data01 <- merge(SSM01, U01, by = "DateTime", all.x = TRUE)
data20 <- merge(SSM20, U20, by = "DateTime", all.x = TRUE)
data13 <- merge(SST13, U13, by = "DateTime", all.x = TRUE)

# Check for duplicates in the original datasets
sum(duplicated(data01))
sum(duplicated(data20))
sum(duplicated(data13))

############################
#### Save matched files ####
############################

# Make sure it is in datetime format
data01$DateTime <- format(data01$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(data01,"googledrive/SSM01_merged.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
data20$DateTime <- format(data20$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(data20,"googledrive/SSM20_merged.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
data13$DateTime <- format(data13$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(data13,"googledrive/SST13_merged.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1qpsqrmcnALNS9OVtoIDICdEuW5LkVuIR"

# Upload the file to the specified Google Drive folder
drive_upload(media = "googledrive/SSM01_merged.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/SSM20_merged.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/SST13_merged.csv", path = as_id(drive_folder_id))

