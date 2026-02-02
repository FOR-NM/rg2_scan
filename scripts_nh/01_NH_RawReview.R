##==============================================================================
## Project: QuEST
## Script to visualize raw scan data from Lamprey River
## Here we will plot some images but will not be saving cleaned data back. 
## It is just so see what your data looks like and understand what needs to get done
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(googledrive) #Download docs from Drive
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate) # Edit date format
library(tidyverse)
library(dataRetrieval) # download USGS discharge data
library(xts) # time series

########################################
#### Clear folders that we will use ####
########################################
# List and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

#####################
#### Import Data ####
#####################
# load data from Google Drive. This is the "merged" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1llXcmKVhauTAHcnTuXuhhatPtEaMoeW2")
scan_csvs <- googledrive::drive_ls(path = scan, type = "csv")
3

# create empty list to store data frames
scan_list <- list()

# loop over each file in the `scan_csvs` data frame
for (i in seq_along(scan_csvs$id)) {
  # define the local file path
  local_path <- file.path("googledrive", scan_csvs$name[i])
  
  # download the file
  googledrive::drive_download(
    file = scan_csvs$id[i],
    path = local_path,
    overwrite = TRUE
  )
  
  # read the header row (row 2)
  header <- read_csv(local_path, n_max = 1, col_names = TRUE)
  
  # read the data starting from row 4 using the header as column names
  data <- read_csv(local_path)
  
  # store the data in the list
  scan_list[[scan_csvs$name[i]]] <- data
}
# remove abs
scan_list <- scan_list[-c(1,3:7)]

# Ensure DateTime column is properly formatted
scan_list <- lapply(scan_list, function(df) {
  df$DateTime <- as.POSIXct(df$DateTime, "%Y-%m-%d %H:%M:%S") # Ensure consistent format
  return(df)
})

### keep rows with only 15-minute intervals ###
# loop through each data frame in the list
#for (i in seq_along(scan_list)) {
# access the current data frame
#df <- scan_list[[i]]

# filter function 
#df <- df %>%
#filter(format(df$dateTime, "%M") %in% c("00", "15", "30", "45"))
# update the data frame in the list
#scan_list[[i]] <- df
#}

#### clean out by specifics of each data set ####
# look at your data and decide if you need to do anything else with it
scan_list[1]

# # IN THIS CASE I am going to remove the first few rows of data to clean it more since they are junk
# 
# scan_list[[1]] <- scan_list[[1]][-c(1:93), ]
# 
# #second data frame:
# scan_list[2]
# 
# scan_list[[2]] <- scan_list[[2]][-c(1:28), ]
# 
# #third data frame:
# scan_list[3]
# 
# scan_list[[3]] <- scan_list[[3]][-c(1:9), ]

##################
#### Plotting ####
##################
### DOC ###
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = `DOCeq [mg/l] - Measured value`)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "2 weeks", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
}

### NO3 ###
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = NO3_mg.l)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
}

### NO3N ###
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = NO3.N_mg.l)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
  
}

### TOC ###
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = TOC_mg.l)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
  
}

### TSS ###
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = TSS_mg.l)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
  
}

### Temp ###
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = Temp_C)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
}

###########################
#### Plot all together ####
###########################
for (i in seq_along(scan_list)) {
  
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df) + 
    geom_line(aes(x=DateTime, y=`Temperature_19 [°C] - Measured value`, color='Temp_C')) +
    geom_line(aes(x=DateTime, y=`TSSeq [mg/l] - Measured value`, color='TSS')) +
    geom_line(aes(x=DateTime, y=`TOCeq [mg/l] - Measured value`, color='TOC')) +
    geom_line(aes(x=DateTime, y=`NO3-Neq [mg/l] - Measured value`, color='NO3-N')) +
    geom_line(aes(x=DateTime, y=`NO3eq [mg/l] - Measured value`, color='NO3')) +
    geom_line(aes(x=DateTime, y=`DOCeq [mg/l] - Measured value`, color='DOC')) +
    scale_x_datetime(date_breaks = "2 weeks", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45)) +
    ylab("Measured")
  print(p)
}

##################################
#### Pull USGS discharge data ####
##################################
# these are codes and functions specific to the USGS package (dataRetrieval)
retrieve_usgs_data <- function(start_date, end_date, site_no = "01073319", p_code = "00060") {
  #Retrieve the USGS discharge data as an instantaneous (uv) data type.
  usgs_data <- readNWISuv(siteNumbers = site_no, parameterCd = p_code, startDate = start_date, endDate = end_date)
  #Rename columns to more user-friendly names.
  usgs_data <- renameNWISColumns(usgs_data)
}

# retrieve USGS data for different s::can sites, each has different deployment dates
USGS_CTB <- retrieve_usgs_data("2024-05-20", "2025-09-01")
USGS_NCBd <- retrieve_usgs_data("2024-05-20", "2025-09-01")
USGS_SBM <- retrieve_usgs_data("2024-05-20", "2025-09-01")
USGS_LMP72 <- retrieve_usgs_data("2024-05-20", "2025-09-01")
USGS_LMP27 <- retrieve_usgs_data("2024-05-20", "2025-09-01")
USGS_LMP07 <- retrieve_usgs_data("2024-05-20", "2025-09-01")

USGS_CTB$DateTime <- USGS_CTB$dateTime
USGS_NCBd$DateTime <- USGS_NCBd$dateTime
USGS_SBM$DateTime <- USGS_SBM$dateTime
USGS_LMP72$DateTime <- USGS_LMP72$dateTime
USGS_LMP27$DateTime <- USGS_LMP27$dateTime
USGS_LMP07$DateTime <- USGS_LMP07$dateTime

########################################
#### Merge parameters and USGS data ####
########################################
# extract parameters from list
NCBd <- scan_list[["NHNCBd_params.csv"]]
CTB <- scan_list[["NHCTB_params.csv"]]
SMB <- scan_list[["NHSBM_params.csv"]]
LMP72 <- scan_list[["NHLMP72_params.csv"]]
LMP27 <- scan_list[["NHLMP27_params.csv"]]
LMP07 <- scan_list[["NHLMP07_params.csv"]]

# First check if the merge works
datCTB <- merge(CTB, USGS_CTB, by = "DateTime")
datNCBd <- merge(NCBd, USGS_NCBd, by = "DateTime")
datSMB <- merge(SMB, USGS_SBM, by = "DateTime")
datLMP72 <- merge(LMP72, USGS_LMP72, by = "DateTime")
datLMP27 <- merge(LMP27, USGS_LMP27, by = "DateTime")
datLMP07 <- merge(LMP07, USGS_LMP07, by = "DateTime")

# return to list
combined <- list()

combined$CTB<- datCTB
combined$NCBd<- datNCBd
combined$SMB<- datSMB 
combined$LMP72<- datLMP72 
combined$LMP27<- datLMP27 
combined$LMP07<- datLMP07 

#####################################
#### Plot all variables separate ####
#####################################
plot_usgs_faceted <- function(df, label) {
  
  # Columns you want to plot
  vars <- c(
    "TSSeq [mg/l] - Measured value",
    "TOCeq [mg/l] - Measured value",
    "NO3-Neq [mg/l] - Measured value",
    "NO3eq [mg/l] - Measured value",
    "DOCeq [mg/l] - Measured value",
    "Flow_Inst"
  )
  
  # Convert to numeric if they exist
  df <- df %>%
    mutate(across(any_of(vars), as.numeric))
  
  # Reshape to long
  long_df <- df %>%
    select(any_of(c("DateTime", vars))) %>%
    pivot_longer(cols = -DateTime,
                 names_to = "Variable",
                 values_to = "Value")
  
  # Plot
  ggplot(long_df, aes(DateTime, Value, color = Variable)) +
    geom_line() +
    facet_wrap(~Variable, scales = "free_y", ncol = 1) +
    scale_x_datetime(date_breaks = "2 week", date_labels = "%m/%d") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab(label) +
    ggtitle(label) +
    theme(legend.position = "none")
}

print(plot_usgs_faceted(combined$NCBd, "NCBd"))
print(plot_usgs_faceted(combined$CTB, "CTB"))
print(plot_usgs_faceted(combined$SMB, "SMB"))
print(plot_usgs_faceted(combined$LMP72, "LMP72"))
print(plot_usgs_faceted(combined$LMP27, "LMP27"))
print(plot_usgs_faceted(combined$LMP07, "LMP07"))

