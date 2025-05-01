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
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

##########################
#### Import scan data ####
##########################
#### import abs and parameter data ####
# this is the "clean" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1g6aSuGnb--Qeyk-rceX82Y5wSNzCqFg0")

# list all the files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")
3

#USF12
googledrive::drive_download(file = merged$id[merged$name=="USF12_absparams_Buttercup_clean.csv"], 
                            path = "googledrive/USF12_absparams_Buttercup_clean.csv",
                            overwrite = T)
#USF20
googledrive::drive_download(file = merged$id[merged$name=="USF20_absparams_Blossom_clean.csv"], 
                            path = "googledrive/USF20_absparams_Blossom_clean.csv",
                            overwrite = T)
#USF21
googledrive::drive_download(file = merged$id[merged$name=="USF21_absparams_Bubbles_clean.csv"], 
                            path = "googledrive/USF21_absparams_Bubbles_clean.csv",
                            overwrite = T)

# load them separately 
USF12 <- read.csv("googledrive/USF12_absparams_Buttercup_clean.csv")
USF20 <- read.csv("googledrive/USF20_absparams_Blossom_clean.csv")
USF21 <- read.csv("googledrive/USF21_absparams_Bubbles_clean.csv")


# DateTime at midnight is missing 00:00:00 time, so filling in that time using grep                     
USF12$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF12$DateTime)] <- paste(
  USF12$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF12$DateTime)],"00:00:00")
USF20$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF20$DateTime)] <- paste(
  USF20$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF20$DateTime)],"00:00:00")
USF21$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF21$DateTime)] <- paste(
  USF21$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF21$DateTime)],"00:00:00")

# convert the DateTime column to POSIXct
USF12$DateTime <- as.POSIXct(USF12$DateTime, format = "%Y-%m-%d %H:%M")
USF20$DateTime <- as.POSIXct(USF20$DateTime, format = "%Y-%m-%d %H:%M")
USF21$DateTime <- as.POSIXct(USF21$DateTime, format = "%Y-%m-%d %H:%M")

# check for duplicates
sum(duplicated(USF12))
sum(duplicated(USF20))
sum(duplicated(USF21))

##################
#### Clean up ####
##################
#### keep rows with only 15-minute intervals ####
USF12 <- USF12 %>%
  filter(format(USF12$DateTime, "%M") %in% c("00", "15", "30", "45"))

USF20 <- USF20 %>%
  filter(format(USF20$DateTime, "%M") %in% c("00", "15", "30", "45"))

USF21 <- USF21 %>%
  filter(format(USF21$DateTime, "%M") %in% c("00", "15", "30", "45"))

#### remove error section from USF20 ###
USF20 <- USF20 %>%
  mutate(across(
    c("DOC_clean", "NO3.N_clean", "NO3_clean", "TOC_clean", "TSS_clean", 21:230),
    ~ ifelse(between(DateTime, as.Date("2024-09-25"), as.Date("2024-10-17")), NA, .)
  ))

#### remove low volt at end of USF21 ###
USF21 <- USF21[-c(11889:11961),]

USF21 <- USF21 %>%
  mutate(across(
    c("DOC_clean", "NO3.N_clean", "NO3_clean", "TOC_clean", "TSS_clean"),
    ~ if_else(row_number() %in% c(1812, 97, 1810), NA, .)
  ))

#### clean values by standard deviation ####
# define the columns to clean
columns_to_clean <- c("DOC_clean", "NO3.N_clean", "NO3_clean", "TOC_clean", "TSS_clean")

# define the number of standard deviations to consider as outliers
sd <- 3  # adjust as needed

# apply cleaning USF12
data12_clean <- USF12 %>%
  mutate(TSS_clean = ifelse(abs(TSS_clean - mean(TSS_clean, na.rm = TRUE)) > 4 * sd(TSS_clean, na.rm = TRUE), 
                            NA, TSS_clean)) %>%
  mutate(TOC_clean = ifelse(abs(TOC_clean - mean(TOC_clean, na.rm = TRUE)) > 6 * sd(TOC_clean, na.rm = TRUE), 
                            NA, TOC_clean))

# apply cleaning USF20
data20_clean <- USF20 %>%
  mutate(across(all_of(columns_to_clean), 
                .fns = ~ ifelse(abs(. - mean(., na.rm = TRUE)) > 6 * sd(., na.rm = TRUE), 
                                NA, 
                                .), 
                .names = "{.col}_clean"))

# apply cleaning USF21
data21_clean <- USF21 %>%
  mutate(DOC_clean = ifelse(abs(DOC_clean - mean(DOC_clean, na.rm = TRUE)) > 5 * sd(DOC_clean, na.rm = TRUE), 
                            NA, DOC_clean)) %>%
  mutate(NO3_clean = ifelse(abs(NO3_clean - mean(NO3_clean, na.rm = TRUE)) > 6 * sd(NO3_clean, na.rm = TRUE), 
                            NA, NO3_clean)) %>%
  mutate(NO3N_clean = ifelse(abs(NO3.N_clean - mean(NO3.N_clean, na.rm = TRUE)) > 3 * sd(NO3.N_clean, na.rm = TRUE), 
                          NA, NO3.N_clean)) %>%
  mutate(TOC_clean = ifelse(abs(TOC_clean - mean(TOC_clean, na.rm = TRUE)) > 4 * sd(TOC_clean, na.rm = TRUE), 
                            NA, TOC_clean)) %>%
  mutate(TSS_clean = ifelse(abs(TSS_clean - mean(TSS_clean, na.rm = TRUE)) > 1.5 * sd(TSS_clean, na.rm = TRUE), 
                            NA, TSS_clean))

#####################################
#### Plot all variables separate ####
#####################################
# function to plot each variable separately in the same panel
plot_variables <- function(df) {
  # Ensure column selection works correctly
  df_long <- df %>%
    dplyr::select("DateTime", "Temp_C", "TSS_clean", "TOC_clean", "NO3.N_clean", "NO3_clean", "DOC_clean") %>%
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

# generate plots
print(plot_variables(USF12))
print(plot_variables(data12_clean))

print(plot_variables(USF20))
print(plot_variables(data20_clean))

print(plot_variables(USF21))
print(plot_variables(data21_clean))


colSums(data12_clean == 0, na.rm = TRUE)
colSums(data20_clean == 0, na.rm = TRUE)
colSums(data21_clean == 0, na.rm = TRUE)

#############################
#### Save filtered files ####
#############################
# make sure it is in datetime format
data12_clean$DateTime <- format(data12_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(data12_clean,"googledrive/USF12_filtered_Buttercup.csv" , row.names=FALSE, quote=FALSE)
# make sure it is in datetime format
data20_clean$DateTime <- format(data20_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(data20_clean,"googledrive/USF20_filtered_Blossom.csv" , row.names=FALSE, quote=FALSE)
# make sure it is in datetime format
data21_clean$DateTime <- format(data21_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(data21_clean,"googledrive/USF21_filtered_Bubbles.csv" , row.names=FALSE, quote=FALSE)

# define the target folder ID in Google Drive
# this is the "clean" folder
drive_folder_id <- "1g6aSuGnb--Qeyk-rceX82Y5wSNzCqFg0"

# upload the file to the specified Google Drive folder
drive_upload(media = "googledrive/USF12_filtered_Buttercup.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/USF20_filtered_Blossom.csv", path = as_id(drive_folder_id))
drive_upload(media = "googledrive/USF21_filtered_Bubbles.csv", path = as_id(drive_folder_id))

