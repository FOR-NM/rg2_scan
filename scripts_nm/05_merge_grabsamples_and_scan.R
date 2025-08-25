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
library(tidyverse)

########################################
#### Clear folders that we will use ####
########################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

##########################
#### Import chem data ####
##########################
#### load chem data ####
# chem data is for all the sites
chem <- googledrive::as_id("https://drive.google.com/drive/folders/1ZCVAoIamyMMtwh-Cy3SpeQx2IWYu6gg2")

# list all CSV files in the folder
chem_csv <- googledrive::drive_ls(path = chem, type = "csv")
3

# call the specific file you want (most recent one)
googledrive::drive_download(file = chem_csv$id[chem_csv$name=="2024-11-15_chem_data.csv"], 
                            path = "googledrive/2024-11-15_chem_data.csv",
                            overwrite = T)
# load it into R
wqual = read.csv("googledrive/2024-11-15_chem_data.csv")

# format date columns
wqual$Collection.Date <- as.Date(wqual$Collection.Date, format = "%m/%d/%y")

# rename Collection Date column
wqual <- wqual %>% rename(Date = Collection.Date)

# clean up a bit
drops <- c("X", "Project", "Sub_ProjectA", "pH", "Cond", "Spec_Cond", "DO_Conc",  "DO.", "Temperature Turbidity", "ID")
wqual <- wqual[ , !(names(wqual) %in% drops)]

# filter to get just NM data
NM <- filter(wqual, Sub_Project == "New Mexico")

#### combine same day-same site samples (reps and bottles) ####

# when there are reps per site per date we need to average them and use the average chem to calculate leverage
head(NM)
# define the columns that need to be averaged
columns_to_average <- c("NPOC..mg.C.L.", "NO3..mg.N.L.", "NH4..ug.N.L.", "TDN..mg.N.L.", 
                        "PO4..ug.P.L.", "Cl..mg.Cl.L.", "SO4..mg.S.L.", "Na..mg.Na.L.", 
                        "K..mg.K.L.", "Mg..mg.Mg.L.", "Ca..mg.Ca.L.")

# calculate averages or fill non-NA values for each Site and Date
data_avg <- NM %>%
  # Group by columns Date and Site, and other unique identifiers if necessary
  group_by(Date, Site) %>%
  
  # summarize: for each column, take the mean if there are multiple values or the single non-NA value
  summarise(across(all_of(columns_to_average),
                   ~ if (all(is.na(.))) NA_real_ else mean(., na.rm = TRUE)),  # calculate mean if there are values
            Sample.Name = paste0(first(Site), "_", first(Date), "_Avg"),    # create a new Sample Name with _Avg
            .groups = "drop") # Ungroup to avoid nested data frames


# count non-NA values in Q column using dplyr
nonna_counts_dplyr <- data_avg %>%
  summarise_all(~ sum(!is.na(.)))

#### load sample info to get grab sample collection time ####
samplelogsheet <- drive_get("https://docs.google.com/spreadsheets/d/1xxSKNQiXFZ-jtFHj2ruqwq5LqSrl9hc37rCcNp8gQ0s/edit?gid=0#gid=0")

# download spreadsheet from Webster Lab Sample Log Sheet
drive_download(as_id(samplelogsheet$id), path = "googledrive/samplelogsheet.xlsx", overwrite = T)

# fetch the file
samplelogsheet <- readxl::read_excel("googledrive/samplelogsheet.xlsx")

# format date and time columns
samplelogsheet$Date <- as.Date(samplelogsheet$Date, format = "%Y/%m/%d")
samplelogsheet$Time <- as.POSIXct(samplelogsheet$Time, format = "%Y-%mm-%dd %H:%M:%S")
samplelogsheet$Time <- format(as.POSIXct(samplelogsheet$Time, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")

# clean up a bit
drops <- c("Project", "Type", "SpC_uScm2", "temp_C pH", "DO_mg/L", "TDS", "sampled_by", 
           "collection_notes", "status_location", "DOC_mgL",  "DOC_file", "DOC_notes", "TSS", 
           "AFDM", "Network Role", "Discharge", "Discharge flag", "Discharge Notes", "QuEST_ID",
           "bottle_type", "filtered_uM", "temp_C", "pH", "lat", "lon", "Location", "ID")
samplelogsheet <- samplelogsheet[ , !(names(samplelogsheet) %in% drops)]

#### change sample time to fit scan time ####
###USF12###
samplelogsheet$Time[samplelogsheet$Site == "USF12" & 
                      samplelogsheet$Date == "2024-05-23" & 
                      samplelogsheet$Time == "09:30:00"] <- "09:45:00"

samplelogsheet$Time[samplelogsheet$Site == "USF12" & 
                      samplelogsheet$Date == "2024-07-08" & 
                      samplelogsheet$Time == "08:30:00"] <- "10:00:00"

samplelogsheet$Time[samplelogsheet$Site == "USF12" & 
                      samplelogsheet$Date == "2024-06-19" & 
                      samplelogsheet$Time == "09:00:00"] <- "10:30:00"
###USF20###
samplelogsheet$Time[samplelogsheet$Site == "USF20" & 
                      samplelogsheet$Date == "2024-05-23" & 
                      samplelogsheet$Time == "12:30:00"] <- "12:45:00"

samplelogsheet$Time[samplelogsheet$Site == "USF20" & 
                      samplelogsheet$Date == "2024-07-08" & 
                      samplelogsheet$Time == "14:00:00"] <- "16:00:00"

samplelogsheet$Time[samplelogsheet$Site == "USF20" & 
                      samplelogsheet$Date == "2024-06-19" & 
                      samplelogsheet$Time == "14:45:00"] <- "17:00:00"
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

# check for duplicates in the original datasets
sum(duplicated(sample_times))
# remove duplicates from the original datasets
sample_times <- sample_times %>% distinct()

# combine Date and Time columns into a new DateTime column
sample_times$DateTime <- paste(sample_times$Date, sample_times$Time, sep = " ")
# convert the DateTime column to POSIXct
sample_times$DateTime <- as.POSIXct(sample_times$DateTime, format = "%Y-%m-%d %H:%M")

##########################
#### Import scan data ####
##########################
#### import abs and parameter data ####
# this is the "merged" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1g6aSuGnb--Qeyk-rceX82Y5wSNzCqFg0")

# list all the files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")

#USF12
googledrive::drive_download(file = merged$id[merged$name=="USF12_filtered_Buttercup.csv"], 
                            path = "googledrive/USF12_filtered_Buttercup.csv",
                            overwrite = T)
#USF20
googledrive::drive_download(file = merged$id[merged$name=="USF20_filtered_Blossom.csv"], 
                            path = "googledrive/USF20_filtered_Blossom.csv",
                            overwrite = T)
#USF21
googledrive::drive_download(file = merged$id[merged$name=="USF21_filtered_Bubbles.csv"], 
                            path = "googledrive/USF21_filtered_Bubbles.csv",
                            overwrite = T)

# load them separately 
USF12 <- read.csv("googledrive/USF12_filtered_Buttercup.csv")
USF20 <- read.csv("googledrive/USF20_filtered_Blossom.csv")
USF21 <- read.csv("googledrive/USF21_filtered_Bubbles.csv")

# convert the DateTime column to POSIXct
USF12$DateTime <- as.POSIXct(USF12$DateTime, format = "%Y-%m-%d %H:%M")
USF20$DateTime <- as.POSIXct(USF20$DateTime, format = "%Y-%m-%d %H:%M")
USF21$DateTime <- as.POSIXct(USF21$DateTime, format = "%Y-%m-%d %H:%M")

# check for duplicates
sum(duplicated(USF12))
sum(duplicated(USF20))
sum(duplicated(USF21))

##################################
#### Merge chem and scan data ####
##################################
# filter to get just one site at a time
U12 <- filter(sample_times, Site == "USF12")
U20 <- filter(sample_times, Site == "USF20")
U21 <- filter(sample_times, Site == "USF21")

# first check if the merge works
dat12 <- merge(USF12, U12, by = "DateTime")
dat20 <- merge(USF20, U20, by = "DateTime")
dat21 <- merge(USF21, U21, by = "DateTime")

# scan data first - perform a left join
data12 <- merge(USF12, U12, by = "DateTime", all.x = TRUE)
data20 <- merge(USF20, U20, by = "DateTime", all.x = TRUE)
data21 <- merge(USF21, U21, by = "DateTime", all.x = TRUE)

# check for duplicates in the original datasets
sum(duplicated(data12))
sum(duplicated(data20))
sum(duplicated(data21))

############################
#### Save matched files ####
############################
# make sure it is in datetime format
data12$DateTime <- format(data12$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(data12,"googledrive/USF12_merged_Buttercup.csv" , row.names=FALSE, quote=FALSE)
# make sure it is in datetime format
data20$DateTime <- format(data20$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(data20,"googledrive/USF20_merged_Blossom.csv" , row.names=FALSE, quote=FALSE)
# make sure it is in datetime format
data21$DateTime <- format(data21$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(data21,"googledrive/USF21_merged_Bubbles.csv" , row.names=FALSE, quote=FALSE)

# define the target folder ID in Google Drive
# this is the "merged" folder
drive_folder_id <- "1g6aSuGnb--Qeyk-rceX82Y5wSNzCqFg0"

# upload the file to the specified Google Drive folder
drive_upload(media = "googledrive/USF12_merged_Buttercup.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/USF20_merged_Blossom.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/USF21_merged_Bubbles.csv", path = as_id(drive_folder_id))

