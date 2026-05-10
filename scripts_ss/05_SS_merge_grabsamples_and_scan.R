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
googledrive::drive_download(file = chem_csv$id[chem_csv$name=="2026-03-31_chem_data.csv"], 
                            path = "googledrive/2026-03-31_chem_data.csv",
                            overwrite = T)
# load it into R
wqual = read.csv("googledrive/2026-03-31_chem_data.csv")

# Format date columns
wqual$Collection.Date <- as.Date(wqual$Collection.Date, format = "%Y-%m-%d")


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
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1BNCKA7LdysjDH5_REI4WhH_P0Z4FIe0r")

# List all the files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")

#SSM01
googledrive::drive_download(file = merged$id[merged$name=="SSM01_absparams_clean.csv"], 
                            path = "googledrive/SSM01_absparams_clean.csv",
                            overwrite = T)
#SSM20
googledrive::drive_download(file = merged$id[merged$name=="SSM20_absparams_clean.csv"], 
                            path = "googledrive/SSM20_absparams_clean.csv",
                            overwrite = T)
#SST13
googledrive::drive_download(file = merged$id[merged$name=="SST13_absparams_clean.csv"], 
                            path = "googledrive/SST13_absparams_clean.csv",
                            overwrite = T)

# Load them separately 
SSM01 <- read.csv("googledrive/SSM01_absparams_clean.csv")
SSM20 <- read.csv("googledrive/SSM20_absparams_clean.csv")
SST13 <- read.csv("googledrive/SST13_absparams_clean.csv")

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

########################################
#### Clear corrupted spectral times ####
########################################
# List of your final dataframes
dfs <- c("data01", "data20", "data13")

for (df_name in dfs) {
  if (exists(df_name)) {
    df <- get(df_name)
    
    # Identify columns starting with X and a digit
    # This avoids accidentally renaming columns like "X" (if it's an ID)
    colnames(df) <- gsub("^X([0-9])", "\\1", colnames(df))
    
    # Force the spectral data to be numeric 
    # (In case the merge turned them back into characters)
    spec_cols <- grep("^[0-9]", colnames(df))
    df <- df %>%
      mutate(across(all_of(spec_cols), ~as.numeric(as.character(.))))
    
    assign(df_name, df)
  }
}

# 1. Define a list of your merged datasets
merged_list <- list(data01 = data01, data20 = data20, data13 = data13)

# 2. Process each to handle corrupted data
cleaned_data_list <- lapply(merged_list, function(df) {
  
  # Identify spectral columns
  spec_cols <- grep("^[0-9]", colnames(df), value = TRUE)
  
  df_clean <- df %>%
    # Masking with NA
    # If Status is 'Corrupted_No_Match', we turn the spectral data into NA
    mutate(across(all_of(spec_cols), 
                  ~ifelse(Status == "Corrupted_No_Match", NA, .))) %>%
    
    # Add a flag for your calibration step
    # This makes it easy to filter only good paired samples later
    mutate(ReadyForCalibration = ifelse(!is.na(Site) & Status != "Corrupted_No_Match", 
                                        TRUE, FALSE))
  
  return(df_clean)
})

# 3. Bring them back to the environment
data01_final <- cleaned_data_list$data01
data20_final <- cleaned_data_list$data20
data13_final <- cleaned_data_list$data13

# 4. QUICK CHECK: How many calibration points did we lose?
# (Where we had a grab sample but the scan was corrupted)
sum(data13_final$Status == "Corrupted_No_Match" & !is.na(data13_final$Site))
sum(data20_final$Status == "Corrupted_No_Match" & !is.na(data20_final$Site))
sum(data01_final$Status == "Corrupted_No_Match" & !is.na(data01_final$Site))

#################################################
#### Clean up spectra, very low or high rows ####
#################################################
data01_clean <- data01_final %>%
  # Remove rows where the condition under -1 and above 100 is not met.
  dplyr::filter(!if_any(c(18:126), 
                        ~ . < -3 | . > 60))

data20_clean <- data20_final %>%
  # Remove rows where the condition under -1 and above 100 is not met.
  dplyr::filter(!if_any(c(18:126), 
                        ~ . < -3 | . > 60))

data13_clean <- data13_final %>%
  # Remove rows where the condition under -1 and above 100 is not met.
  dplyr::filter(!if_any(c(18:129), 
                        ~ . < -3 | . > 60))

# Flag bad spectra — adapt column indices to your data
data01_clean <- data01_clean %>%
  mutate(spec_min = apply(.[, 25:39], 1, min, na.rm = TRUE),
         spec_max = apply(.[, 25:39], 1, max, na.rm = TRUE),
         bad_spec = spec_min < -20 | spec_max > 70)

ggplot(data01_clean, aes(x = DateTime, y = spec_min, color = bad_spec)) +
  geom_point(size = 0.5, alpha = 0.5) +
  scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "red")) +
  labs(title = "SSM01: minimum absorbance value over time",
       subtitle = "Red = spectra flagged as bad (min < -20)",
       x = "Date", y = "Minimum absorbance across all bands") +
  theme_minimal()
# Flag bad spectra — adapt column indices to your data
data13_clean <- data13_clean %>%
  mutate(spec_min = apply(.[, 25:39], 1, min, na.rm = TRUE),
         spec_max = apply(.[, 25:39], 1, max, na.rm = TRUE),
         bad_spec = spec_min < -20 | spec_max > 70)

ggplot(data13_clean, aes(x = DateTime, y = spec_min, color = bad_spec)) +
  geom_point(size = 0.5, alpha = 0.5) +
  scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "red")) +
  labs(title = "SST13: minimum absorbance value over time",
       subtitle = "Red = spectra flagged as bad (min < -20)",
       x = "Date", y = "Minimum absorbance across all bands") +
  theme_minimal()
# Flag bad spectra — adapt column indices to your data
data20_clean <- data20_clean %>%
  mutate(spec_min = apply(.[, 25:39], 1, min, na.rm = TRUE),
         spec_max = apply(.[, 25:39], 1, max, na.rm = TRUE),
         bad_spec = spec_min < -20 | spec_max > 70)

ggplot(data20_clean, aes(x = DateTime, y = spec_min, color = bad_spec)) +
  geom_point(size = 0.5, alpha = 0.5) +
  scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "red")) +
  labs(title = "SSM20: minimum absorbance value over time",
       subtitle = "Red = spectra flagged as bad (min < -20)",
       x = "Date", y = "Minimum absorbance across all bands") +
  theme_minimal()

############################
#### Save matched files ####
############################
# Make sure it is in datetime format
data01_clean$DateTime <- format(data01_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(data01_clean,"googledrive/SSM01_merged.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
data20_clean$DateTime <- format(data20_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(data20_clean,"googledrive/SSM20_merged.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
data13_clean$DateTime <- format(data13_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(data13_clean,"googledrive/SST13_merged.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "with grab" folders
drive_folder_id <- "1Wju54VbyACZ_RFtfeInSvBCiVDKFScGj"

# Upload the file to the specified Google Drive folder
drive_put(media = "googledrive/SSM01_merged.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/SSM20_merged.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/SST13_merged.csv", path = as_id(drive_folder_id))

