##==============================================================================
## Project: QuEST
## Script to visualize raw scan data from South Sandy
## Here we will plot some images but will not be saving cleaned data back. 
## It is just so see what your data looks like and understand what needs to get done
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(googledrive) #Download docs from Drive
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate) # Edit date format

########################################
#### Clear folders that we will use ####
########################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

#################################
#### Import & Visualize Data ####
#################################
# load data from Google drive, this is the "merged" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1qpsqrmcnALNS9OVtoIDICdEuW5LkVuIR")
# list all CSV files in the folder
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
  
  # read the data starting from row 4 using the header as column names
  data <- read.csv(local_path)
  
  # store the data in the list
  scan_list[[scan_csvs$name[i]]] <- data
}

#### remove abs files from list ####
# we are just plotting parameters to see how data looks
# remove 1st through 3rd items in this case, check position of abs files
scan_list = scan_list[-c(1:3)]

################################
#### Format DateTime column ####
################################
# loop through each data frame in the list
for (i in seq_along(scan_list)) {
  # access the current data frame
  df <- scan_list[[i]]
  
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  # Update the data frame in the list
  scan_list[[i]] <- df
}

# check the contents of the list and make sure there are no NAs
str(scan_list)

#################
#### Tidying #### 
#################
## rename columns
# loop through each data frame in the list
for (i in seq_along(scan_list)) {
  # access the current data frame
  df <- scan_list[[i]]
  
  # change names for easier handling
  df <- df %>%
    dplyr::rename(
      DOCeq_mg.l = DOCeq..mg.l....Measured.value,
      NO3.N_mg.l = NO3.Neq..mg.l....Measured.value,
      NO3_mg.l = NO3eq..mg.l....Measured.value,
      TOC_mg.l = TOCeq..mg.l....Measured.value,
      TSS_mg.l = TSSeq..mg.l....Measured.value,
      Temp_C = Temperature_21...C....Measured.value
    )
  
  # update the data frame in the list
  scan_list[[i]] <- df
}

### keep rows with only 15-minute intervals ###
# # loop through each data frame in the list
# for (i in seq_along(scan_list)) {
# # access the current data frame
#   df <- scan_list[[i]]
#   
#   # filter function 
#   df <- df %>%
#     filter(format(df$dateTime, "%M") %in% c("00", "15", "30", "45"))
#   # update the data frame in the list
#   scan_list[[i]] <- df
# #}

# #### clean out by specifics of each data set ####
# # look at your data and decide if you need to do anything else with it
# scan_list[1]
# 
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
  # access the current data frame
  df <- scan_list[[i]]
  # plot
  p <- ggplot(data = df, aes(x = DateTime, y = DOCeq_mg.l)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
}

### NO3 ###
for (i in seq_along(scan_list)) {
  # access the current data frame
  df <- scan_list[[i]]
  # plot
  p <- ggplot(data = df, aes(x = DateTime, y = NO3_mg.l)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
}

### NO3N ###
for (i in seq_along(scan_list)) {
  # access the current data frame
  df <- scan_list[[i]]
  # plot
  p <- ggplot(data = df, aes(x = DateTime, y = NO3.N_mg.l)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
  
}

### TOC ###
for (i in seq_along(scan_list)) {
  # access the current data frame
  df <- scan_list[[i]]
  # plot
  p <- ggplot(data = df, aes(x = DateTime, y = TOC_mg.l)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
  
}

### TSS ###
for (i in seq_along(scan_list)) {
  # access the current data frame
  df <- scan_list[[i]]
  # plot
  p <- ggplot(data = df, aes(x = DateTime, y = TSS_mg.l)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  print(p)
  
}

### Temp ###
for (i in seq_along(scan_list)) {
  # access the current data frame
  df <- scan_list[[i]]
  # plot
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
  
  # access the current data frame
  df <- scan_list[[i]]
  # plot
  p <- ggplot(data = df) + 
    geom_line(aes(x=DateTime, y=Temp_C, color='Temp_C')) +
    geom_line(aes(x=DateTime, y=TSS_mg.l, color='TSS')) +
    geom_line(aes(x=DateTime, y=TOC_mg.l, color='TOC')) +
    geom_line(aes(x=DateTime, y=NO3.N_mg.l, color='NO3-N')) +
    geom_line(aes(x=DateTime, y=NO3_mg.l, color='NO3')) +
    geom_line(aes(x=DateTime, y=DOCeq_mg.l, color='DOC')) +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45)) +
    ylab("Measured")
  print(p)
}
