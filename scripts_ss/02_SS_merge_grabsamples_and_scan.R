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
googledrive::drive_download(file = chem_csv$id[chem_csv$name=="2026-01-07_chem_data.csv"], 
                            path = "googledrive/2026-01-07_chem_data.csv",
                            overwrite = T)
# load it into R
wqual = read.csv("googledrive/2026-01-07_chem_data.csv")

# Format date columns
wqual$Collection.Date <- as.Date(wqual$Collection.Date, format = "%m/%d/%y")


# Rename Collection Date column
wqual <- wqual %>% rename(Date = Collection.Date)

# Clean up a bit
drops <- c("X", "Project", "Sub_ProjectA", "pH", "Cond", "Spec_Cond", "DO_Conc",  "DO.", "Temperature Turbidity", "ID")
wqual <- wqual[ , !(names(wqual) %in% drops)]

# Filter to get just SS data
SS <- filter(wqual, Sub_Project == "Alabama")

#### Combine same day-same site samples (reps and bottles) ####

# When there are reps per site per date we need to average them and use the average chem to calculate leverage
head(SS)
# Define the columns that need to be averaged
columns_to_average <- c("NPOC..mg.C.L.", "NO3..mg.N.L.", "NH4..ug.N.L.", "TDN..mg.N.L.", 
                        "PO4..ug.P.L.", "Cl..mg.Cl.L.", "SO4..mg.S.L.", "Na..mg.Na.L.", 
                        "K..mg.K.L.", "Mg..mg.Mg.L.", "Ca..mg.Ca.L.")

# Calculate averages or fill non-NA values for each Site and Date
data_avg <- SS %>%
  # Group by columns Date and Site, and other unique identifiers if necessary
  group_by(Date, Site) %>%
  
  # Summarize: for each column, take the mean if there are multiple values or the single non-NA value
  summarise(across(all_of(columns_to_average),
                   ~ if (all(is.na(.))) NA_real_ else mean(., na.rm = TRUE)),  # Calculate mean if there are values
            Sample.Name = paste0(first(Site), "_", first(Date), "_Avg"),    # Create a new Sample Name with _Avg
            .groups = "drop") # Ungroup to avoid nested data frames


# Count non-NA values in Q column using dplyr
nonna_counts_dplyr <- data_avg %>%
  summarise_all(~ sum(!is.na(.)))

#### Load sample info to get grab sample collection time ####
samplelogsheet <- drive_get("https://docs.google.com/spreadsheets/d/1JVDwzSoHetQGHhYPoeoTOHzlmRrcWNPs-i5b8t2U754/edit?gid=2118868541#gid=2118868541")

# Download spreadsheet from Webster Lab Sample Log Sheet
drive_download(as_id(samplelogsheet$id), path = "googledrive/samplelogsheet.xlsx", overwrite = T)

# Fetch the file
samplelogsheet <- readxl::read_excel("googledrive/samplelogsheet.xlsx", sheet = "YSI", skip = 1)

# Format date and time columns
samplelogsheet$Date <- as.Date(samplelogsheet$`Collection Date`, format = "%Y-%m-%d")
samplelogsheet$Time <- format(as.POSIXct(samplelogsheet$`Time`, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")

# Clean up a bit
drops <- c("Sampling Time", "Crew Initials", "Pressure (mmHg)", "Dissolved O2 (%sat)", "pH", "Dissolved O2 (mg/L)")
samplelogsheet <- samplelogsheet[ , !(names(samplelogsheet) %in% drops)]

# #### Change sample time to fit scan time ####
# ###USF12###
# samplelogsheet$Time[samplelogsheet$Site == "USF12" &
#                       samplelogsheet$Date == "2024-05-23" &
#                       samplelogsheet$Time == "09:30:00"] <- "09:45:00"

##################################
#### Rounding collection time ####
##################################
# Create extra columns so you don't erase original time               
samplelogsheet$TimeNotRounded <- samplelogsheet$`Arrival Time`

# Extract only the time part (HH:MM:SS) from the rounded DateTime
samplelogsheet$Time <- format(samplelogsheet$`Arrival Time`, format = "%H:%M:%S")

# Combine Date and Time columns into a new DateTime column
samplelogsheet$DateTime <- paste(samplelogsheet$Date, samplelogsheet$Time, sep = " ")
# Convert the DateTime column to POSIXct
samplelogsheet$DateTime <- as.POSIXct(samplelogsheet$DateTime, format = "%Y-%m-%d %H:%M:%S")

# Round DateTime to the nearest 15-minute interval 
samplelogsheet$DateTime <- round_date(samplelogsheet$DateTime, unit="15 mins")

# Check if it worked!
str(samplelogsheet)

#######################################################################
#### Merge chem and sample log sheet to get sample collection time ####
#######################################################################
# filter only data for SSM20, SST13 and SSM01 (scan sites) 
wqual_scans <- data_avg %>% filter(Site %in% c("SSM01", "SSM20", "SST13"))

# merge wqual data first
sample_times <- merge(wqual_scans, samplelogsheet, by = c("Date", "Site"))

# Check for duplicates in the original datasets
sum(duplicated(sample_times))
# Remove duplicates from the original datasets
# sample_times <- sample_times %>% distinct()

sample_times <- sample_times[-6,]
##########################
#### Import scan data ####
##########################
#### Import abs and parameter data ####
# This is the "params and abs" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1WbfZWpSeXVLoSEvxqbVnjgvgo4uUwGtm")

# List all the files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")

#SSM01
googledrive::drive_download(file = merged$id[merged$name=="02_SSM01_absparams.csv"], 
                            path = "googledrive/02_SSM01_absparams.csv",
                            overwrite = T)
#SSM20
googledrive::drive_download(file = merged$id[merged$name=="02_SSM20_absparams.csv"], 
                            path = "googledrive/02_SSM20_absparams.csv",
                            overwrite = T)
#SST13
googledrive::drive_download(file = merged$id[merged$name=="02_SST13_absparams.csv"], 
                            path = "googledrive/02_SST13_absparams.csv",
                            overwrite = T)

# Load them separately 
SSM01 <- read.csv("googledrive/02_SSM01_absparams.csv")
SSM20 <- read.csv("googledrive/02_SSM20_absparams.csv")
SST13 <- read.csv("googledrive/02_SST13_absparams.csv")

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
# This is the "with grab" folder
drive_folder_id <- "1Wju54VbyACZ_RFtfeInSvBCiVDKFScGj"

# Upload the file to the specified Google Drive folder
drive_upload(media = "googledrive/SSM01_merged.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/SSM20_merged.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/SST13_merged.csv", path = as_id(drive_folder_id))

