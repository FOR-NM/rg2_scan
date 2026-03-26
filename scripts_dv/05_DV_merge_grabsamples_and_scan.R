##==============================================================================
## Project: QuEST
## Here we will prep grab sample data by matching the grab samples with the time stamp of the s::can for South Sandy
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(googledrive) 
library(openxlsx)
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
googledrive::drive_download(file = chem_csv$id[chem_csv$name=="2026-01-07_chem_data.csv"], 
                            path = "googledrive/2026-01-07_chem_data.csv",
                            overwrite = T)
# load it into R
wqual = read.csv("googledrive/2026-01-07_chem_data.csv")

# Filter to get just DV data
DV <- filter(wqual, Sub_Project == "Nevada")

# Format date columns
DV$Collection.Date <- as.Date(DV$Collection.Date, format = "%m/%d/%y")

# Rename Collection Date column
DV <- DV %>% 
  rename(Date = Collection.Date) 

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
  group_by(Date, Site) %>%
  
  # Summarize: for each column, take the mean if there are multiple values or the single non-NA value
  summarise(across(all_of(columns_to_average),
                   ~ if (all(is.na(.))) NA_real_ else mean(., na.rm = TRUE)),  # Calculate mean if there are values
            Sample.Name = paste0(first(Site), "_", first(Date), "_Avg"),    # Create a new Sample Name with _Avg
            .groups = "drop") # Ungroup to avoid nested data frames


# Count non-NA values in Q column using dplyr
nonna_counts_dplyr <- data_avg %>%
  summarise_all(~ sum(!is.na(.)))

#############################################################
#### Load sample info to get grab sample collection time ####
#############################################################
# Set the Google Sheets file ID
file_id <- "https://docs.google.com/spreadsheets/d/1f4iH0JrE9bNU3SSsXhK-gk3yzFa_k70TaJQ1BOPd3mk/edit?gid=0#gid=0"

# Download the Google Sheet as an Excel file (.xlsx)
drive_download(as_id(file_id), 
               path = "googledrive/All Site Information and Notes", 
               type = "xlsx", 
               overwrite = TRUE)

# Read the Excel file
times <- read_excel("googledrive/All Site Information and Notes.xlsx")

# Fix date 
str(times$Date)  # Check structure
class(times$Date)  # Check specific class

# Fix time
times <- times %>%
  mutate(Time = convertToDateTime(`Time Arrived (if applicable)`),
         # Extract only the Time part and store it as a character string
         Time = format(Time, format = "%H:%M:%S"))

#### This is if you want to change sample time to fit scan time ####
###DVO###
# times$Start_Time[times$`Site ID` == "DVO" &
#                    times$Date == "2024-05-23" &
#                    times$Start_Time == "09:30:00"] <- "09:45:00"

##################################
#### Rounding collection time ####
##################################
# Combine Date and Time columns into a new DateTime column
times$DateTime <- paste(times$Date, times$Time, sep = " ")
# Convert the DateTime column to POSIXct
times$DateTime <- as.POSIXct(times$DateTime, format = "%Y-%m-%d %H:%M")

# Round DateTime to the nearest 15-minute interval 
times$DateTime <- round_date(times$DateTime, unit="15 mins")

# Check if it worked!
str(times)

# filter only data for DVO, DVMS1 and DVNW5 (scan sites) 
times <- times %>% filter(Site %in% c("DVO", "DVMS1", "DVNWT5"))

#######################################################################
#### Merge chem and sample log sheet to get sample collection time ####
#######################################################################
# filter only data for DVO, DVMS1 and DVNW5 (scan sites) 
wqual_scans <- data_avg %>% filter(Site %in% c("DVO", "DVMS1", "DVNWT5"))

# merge wqual data first
sample_times <- merge(wqual_scans, times, by = c("Date", "Site"))

# Check for duplicates in the original datasets
sum(duplicated(sample_times))

# Remove duplicates from the original datasets
# sample_times <- sample_times %>% distinct()

#write.csv(sample_times, "sample_times_DV.csv")

##########################
#### Import scan data ####
##########################
#### Import abs and parameter data ####
# This is the "merged" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1gAfaUZKCoarEaSrPnhdUXy5lQ49Z_xm1")

# List all the files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")

#DVO
googledrive::drive_download(file = merged$id[merged$name=="02_DVO_absparams.csv"], 
                            path = "googledrive/02_DVO_absparams.csv",
                            overwrite = T)
#DVNWT5
googledrive::drive_download(file = merged$id[merged$name=="02_DVNWT5_absparams.csv"], 
                            path = "googledrive/02_DVNWT5_absparams.csv",
                            overwrite = T)
#DVMS1
googledrive::drive_download(file = merged$id[merged$name=="02_DVMS1_absparams.csv"], 
                            path = "googledrive/02_DVMS1_absparams.csv",
                            overwrite = T)

# Load them separately 
DVO <- read.csv("googledrive/02_DVO_absparams.csv")
DVNWT5 <- read.csv("googledrive/02_DVNWT5_absparams.csv")
DVMS1 <- read.csv("googledrive/02_DVMS1_absparams.csv")

# Convert the DateTime column to POSIXct
DVO$DateTime <- as.POSIXct(DVO$DateTime, format = "%Y-%m-%d %H:%M")
DVNWT5$DateTime <- as.POSIXct(DVNWT5$DateTime, format = "%Y-%m-%d %H:%M")
DVMS1$DateTime <- as.POSIXct(DVMS1$DateTime, format = "%Y-%m-%d %H:%M")

# Check for duplicates
sum(duplicated(DVO))
sum(duplicated(DVNWT5))
sum(duplicated(DVMS1))

##################################
#### Merge chem and scan data ####
##################################
# Filter to get just one site at a time
UO <- filter(sample_times, Site == "DVO")
UNWT5 <- filter(sample_times, Site == "DVNWT5")
UMS1 <- filter(sample_times, Site == "DVMS1")

# First check if the merge works
datO <- merge(DVO, UO, by = "DateTime")
datNWT5 <- merge(DVNWT5, UNWT5, by = "DateTime")
datMS1 <- merge(DVMS1, UMS1, by = "DateTime")

# scan data first - perform a left join
dataO <- merge(DVO, UO, by = "DateTime", all.x = TRUE)
dataNWT5 <- merge(DVNWT5, UNWT5, by = "DateTime", all.x = TRUE)
dataMS1 <- merge(DVMS1, UMS1, by = "DateTime", all.x = TRUE)

# Check for duplicates in the original datasets
sum(duplicated(dataO))
sum(duplicated(dataNWT5))
sum(duplicated(dataMS1))

##################
#### Clean up ####
##################
dataO <- dataO %>%
  dplyr::select(-c(X.x, X.y, `Time Arrived (if applicable)`, `Time Sensor(s) Were Cleaned (removed from water)`, 
                   `Time Sensor(s) Were Cleaned (returned to water)`, `Activites Done at Site`, `Stage Depth (m)`,
                   `Actual Depth (m)`, `Site Width (m)`, `Height of Water Column from Ground (m)`,
                   `Height of L.L. Bolt from Ground (m)`, `Height of L.L. Bolt from water column (m)`,
                   `* Height when Water Column is Above L.L. Bolt (m) *`, `Other Notes about Site`, X725.00.nm))
dataNWT5 <- dataNWT5 %>% 
  dplyr::select(-c(X.x, X.y,`Time Arrived (if applicable)`, `Time Sensor(s) Were Cleaned (removed from water)`, 
                   `Time Sensor(s) Were Cleaned (returned to water)`, `Activites Done at Site`, `Stage Depth (m)`,
                   `Actual Depth (m)`, `Site Width (m)`, `Height of Water Column from Ground (m)`,
                   `Height of L.L. Bolt from Ground (m)`, `Height of L.L. Bolt from water column (m)`,
                   `* Height when Water Column is Above L.L. Bolt (m) *`, `Other Notes about Site`))
dataMS1 <- dataMS1 %>%
  dplyr::select(-c(X.x, X.y, `Time Arrived (if applicable)`, `Time Sensor(s) Were Cleaned (removed from water)`, 
                   `Time Sensor(s) Were Cleaned (returned to water)`, `Activites Done at Site`, `Stage Depth (m)`,
                   `Actual Depth (m)`, `Site Width (m)`, `Height of Water Column from Ground (m)`,
                   `Height of L.L. Bolt from Ground (m)`, `Height of L.L. Bolt from water column (m)`,
                   `* Height when Water Column is Above L.L. Bolt (m) *`, `Other Notes about Site`, X725.00.nm))

##########################
#### Clean up spectra ####
##########################
# Here you find when your spectra go negative. For USF data is around 450-460nm (column 123)
# Do not remove any spectral values if for tss
# dataDVO_clean <- dataO[,-c(117:224)]
# dataDVNWT5_clean <- dataNWT5[,-c(97:224)]
# dataDVMS1_clean <- dataMS1[,-c(100:224)]

# data12 <- data12[,-c(220:228)]
# data20 <- data20[,-c(220:228)]
# data21 <- data21[,-c(220:228)]

#################################################
#### Clean up spectra, very low or high rows ####
#################################################
dataO_clean <- dataO %>%
  # Remove rows where the condition under -1 and above 100 is not met.
  dplyr::filter(!if_any(c(15:99), 
                        ~ . < -1 | . > 60))

dataNWT5_clean <- dataNWT5 %>%
  # Remove rows where the condition under -1 and above 100 is not met.
  dplyr::filter(!if_any(c(15:99), 
                        ~ . < -1 | . > 60))

dataMS1_clean <- dataMS1 %>%
  # Remove rows where the condition under -1 and above 100 is not met.
  dplyr::filter(!if_any(c(15:99), 
                        ~ . < -1 | . > 60))

############################
#### Save matched files ####
############################
# Make sure it is in datetime format
dataO_clean$DateTime <- POSIXct(dataO_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(dataO_clean,"googledrive/DVO_chem.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
dataNWT5_clean$DateTime <- POSIXct(dataNWT5_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(dataNWT5_clean,"googledrive/DVNWT5_chem.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
dataMS1_clean$DateTime <- POSIXct(dataMS1_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(dataMS1_clean,"googledrive/DVMS1_chem.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "with chem" folder
drive_folder_id <- "12N6uUxXTttdnadDrn43mL6ilz-Ui2eQX"

# Upload the file to the specified Google Drive folder
drive_put(media = "googledrive/DVO_chem.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/DVNWT5_chem.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/DVMS1_chem.csv", path = as_id(drive_folder_id))

