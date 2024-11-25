##==============================================================================
## Project: QuEST
## Here we will prep grab sample data by matching the grab samples with the time stamp of the s::can
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(googledrive) 
library(googlesheets4)
library(dplyr)
library(xts)
library(readxl)

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
googledrive::drive_download(file = chem_csv$id[chem_csv$name=="2024-11-15_chem_data.csv"], 
                            path = "googledrive/2024-11-15_chem_data.csv",
                            overwrite = T)
# load it into R
wqual = read.csv("googledrive/2024-11-15_chem_data.csv")

# Format date columns
wqual$Collection.Date <- as.Date(wqual$Collection.Date, format = "%m/%d/%y")

# Rename Collection Date column
wqual <- wqual %>% rename(Date = Collection.Date)

# Clean up a bit
drops <- c("X", "Project", "Sub_ProjectA", "pH", "Cond", "Spec_Cond", "DO_Conc",  "DO.", "Temperature Turbidity", "ID")
wqual <- wqual[ , !(names(wqual) %in% drops)]

# Filter to get just NM data
NM <- filter(wqual, Sub_Project == "New Mexico")

#### Combine same day-same site samples (reps and bottles) ####

# When there are reps per site per date we need to average them and use the average chem to calculate leverage
head(NM)
# Define the columns that need to be averaged
columns_to_average <- c("NPOC..mg.C.L.", "NO3..mg.N.L.", "NH4..ug.N.L.", "TDN..mg.N.L.", 
                        "PO4..ug.P.L.", "Cl..mg.Cl.L.", "SO4..mg.S.L.", "Na..mg.Na.L.", 
                        "K..mg.K.L.", "Mg..mg.Mg.L.", "Ca..mg.Ca.L.")

# Calculate averages or fill non-NA values for each Site and Date
data_avg <- NM %>%
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
samplelogsheet <- drive_get("https://docs.google.com/spreadsheets/d/1xxSKNQiXFZ-jtFHj2ruqwq5LqSrl9hc37rCcNp8gQ0s/edit?gid=0#gid=0")

# Download spreadsheet from Webster Lab Sample Log Sheet
drive_download(as_id(samplelogsheet$id), path = "googledrive/samplelogsheet.xlsx", overwrite = T)

# Fetch the file
samplelogsheet <- readxl::read_excel("googledrive/samplelogsheet.xlsx")

# Format date and time columns
samplelogsheet$Date <- as.Date(samplelogsheet$Date, format = "%Y/%m/%d")
samplelogsheet$Time <- as.POSIXct(samplelogsheet$Time, format = "%Y-%mm-%dd %H:%M:%S")
samplelogsheet$Time <- format(as.POSIXct(samplelogsheet$Time, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")

# Clean up a bit
drops <- c("Project", "Type", "SpC_uScm2", "temp_C pH", "DO_mg/L", "TDS", "sampled_by", 
           "collection_notes", "status_location", "DOC_mgL",  "DOC_file", "DOC_notes", "TSS", 
           "AFDM", "Network Role", "Discharge", "Discharge flag", "Discharge Notes", "QuEST_ID",
           "bottle_type", "filtered_uM", "temp_C", "pH", "lat", "lon", "Location", "ID")
samplelogsheet <- samplelogsheet[ , !(names(samplelogsheet) %in% drops)]

#### Change sample time to fit scan time ####
###USF12###
samplelogsheet$Time[samplelogsheet$Site == "USF12" & 
                      samplelogsheet$Date == "2024-05-23" & 
                      samplelogsheet$Time == "09:30:00"] <- "09:45:00"

samplelogsheet$Time[samplelogsheet$Site == "USF12" & 
                      samplelogsheet$Date == "2024-07-08" & 
                      samplelogsheet$Time == "08:30:00"] <- "07:15:00"

samplelogsheet$Time[samplelogsheet$Site == "USF12" & 
                      samplelogsheet$Date == "2024-06-19" & 
                      samplelogsheet$Time == "09:00:00"] <- "08:00:00"
###USF20###
samplelogsheet$Time[samplelogsheet$Site == "USF20" & 
                      samplelogsheet$Date == "2024-05-23" & 
                      samplelogsheet$Time == "12:30:00"] <- "12:45:00"

samplelogsheet$Time[samplelogsheet$Site == "USF20" & 
                      samplelogsheet$Date == "2024-07-08" & 
                      samplelogsheet$Time == "14:00:00"] <- "13:30:00"

samplelogsheet$Time[samplelogsheet$Site == "USF20" & 
                      samplelogsheet$Date == "2024-06-19" & 
                      samplelogsheet$Time == "14:45:00"] <- "14:15:00"
###USF21###
samplelogsheet$Time[samplelogsheet$Site == "USF21" & 
                      samplelogsheet$Date == "2024-06-27" & 
                      samplelogsheet$Time == "11:00:00"] <- "13:30:00"
 
#######################################################################
#### Merge chem and sample log sheet to get sample collection time ####
#######################################################################

# filter only data for USF12, 20 and 21 (scan sites)
wqual_scans <- data_avg %>% filter(Site %in% c("USF12", "USF21", "USF20"))

# wqual data first
sample_times <- merge(wqual_scans, samplelogsheet, by = c("Date", "Site"))

# Check for duplicates in the original datasets
sum(duplicated(sample_times))
# Remove duplicates from the original datasets
sample_times <- sample_times %>% distinct()

# Combine Date and Time columns into a new DateTime column
sample_times$DateTime <- paste(sample_times$Date, sample_times$Time, sep = " ")
# Convert the DateTime column to POSIXct
sample_times$DateTime <- as.POSIXct(sample_times$DateTime, format = "%Y-%m-%d %H:%M")

##########################
#### Import scan data ####
##########################

#### Import abs and parameter data ####
# This is the "merged" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1g6aSuGnb--Qeyk-rceX82Y5wSNzCqFg0")

# List all the files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")

#USF12
googledrive::drive_download(file = merged$id[merged$name=="USF12_absparams_Buttercup.csv"], 
                            path = "googledrive/USF12_absparams_Buttercup.csv",
                            overwrite = T)
#USF20
googledrive::drive_download(file = merged$id[merged$name=="USF20_absparams_Blossom.csv"], 
                            path = "googledrive/USF20_absparams_Blossom.csv",
                            overwrite = T)
#USF21
googledrive::drive_download(file = merged$id[merged$name=="USF21_absparams_Bubbles.csv"], 
                            path = "googledrive/USF21_absparams_Bubbles.csv",
                            overwrite = T)

# Let's load them separately first
USF12 <- read.csv("googledrive/USF12_absparams_Buttercup.csv")
USF20 <- read.csv("googledrive/USF20_absparams_Blossom.csv")
USF21 <- read.csv("googledrive/USF21_absparams_Bubbles.csv")

# Convert the DateTime column to POSIXct
USF12$DateTime <- as.POSIXct(USF12$DateTime, format = "%Y-%m-%d %H:%M")
USF20$DateTime <- as.POSIXct(USF20$DateTime, format = "%Y-%m-%d %H:%M")
USF21$DateTime <- as.POSIXct(USF21$DateTime, format = "%Y-%m-%d %H:%M")

##################################
#### Merge chem and scan data ####
##################################

# Filter to get just one site at a time
U12 <- filter(sample_times, Site == "USF12")
U20 <- filter(sample_times, Site == "USF20")
U21 <- filter(sample_times, Site == "USF21")

# First check if the merge works
dat12 <- merge(USF12, U12, by = "DateTime")
dat20 <- merge(USF20, U20, by = "DateTime")
dat21 <- merge(USF21, U21, by = "DateTime")

# scan data first - perform a left join
data12 <- merge(USF12, U12, by = "DateTime", all.x = TRUE)
data20 <- merge(USF20, U20, by = "DateTime", all.x = TRUE)
data21 <- merge(USF21, U21, by = "DateTime", all.x = TRUE)

# Check for duplicates in the original datasets
sum(duplicated(data12))
sum(duplicated(data20))
sum(duplicated(data21))

############################
#### Save matched files ####
############################

# Make sure it is in datetime format
data12$DateTime <- format(data12$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(data12,"googledrive/USF12_merged_Buttercup.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
data20$DateTime <- format(data20$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(data20,"googledrive/USF20_merged_Blossom.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
data21$DateTime <- format(data21$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(data21,"googledrive/USF21_merged_Bubbles.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1g6aSuGnb--Qeyk-rceX82Y5wSNzCqFg0"

# Upload the file to the specified Google Drive folder
drive_upload(media = "googledrive/USF12_merged_Buttercup.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/USF20_merged_Blossom.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/USF21_merged_Bubbles.csv", path = as_id(drive_folder_id))

