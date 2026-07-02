##==============================================================================
## Project: FOR-NM
## Adapted from QuEST
## Original Author: Manuela Londono
## Modified by : Marcela Mendoza 
## removed redundant skipping of rows from files (before deployment), clean code to iterate through sites,
## added input and output path variables 
## Script to merge parameters and fingerprint s::can data
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================
library(googledrive)
library(dplyr)

# site names : 
site_names <- c("USF02","USF24", "USF25", "USF40",  "USF41")
#merged_timestamps folder
input_path<- "data/merged_timestamps/"
output_path<-"data/merged_params_and_abs/"
dir.create(file.path('data', 'merged_params_and_abs'))

for (site in site_names){
  print(site)
  # to do , gdrive flow, for now read local file 
  #######################################
  #### Import abs and parameter data ####
  #######################################
  # this is the "merged_timestamps" folder
 # scan <- googledrive::as_id("https://drive.google.com/drive/folders/1-dUxVn1hBWy2MpHeIjVt-2QSujpVhijy")
  
  # list all the files in the folder
  #scan_csvs <- googledrive::drive_ls(path = scan, type = "csv")
  
  #site parameters
  #googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="USF12_params.csv"], 
                              #path = "googledrive/USF12_params.csv",
                              #overwrite = T)
  #site abs
  #googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="USF12_abs.csv"], 
                              #path = "googledrive/USF12_abs.csv",
                              #overwrite = T)
  
  
  # load fingerprints and parameter data
  site_params <- read.csv(paste0(input_path, site,"_params.csv"))
  site_abs <- read.csv(paste0(input_path, site,"_abs.csv"))
  
  colSums(site_params == 0, na.rm = TRUE)
  #############################
  #### Tidy both data sets ####
  #############################
  # to do: remove rows before deployment 
  
  # change datetime format
  site_params <- site_params %>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
  site_abs <- site_abs %>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
  
  #################################
  #### Merge parameter and abs ####
  #################################
  # param data first
  site_merged <- left_join(site_params, site_abs, by = "DateTime")
  
  ##################
  #### Clean up #### TO DO, incorporate from datasheets 
  ##################
  # Remove data before deployment
  #USF12_merged <- USF12_merged %>%
    #filter(DateTime > "2024-05-07 13:00:00")
  
  #USF12_merged <- USF12_merged %>%
    #select(-X.x, -X.y)
  
  #########################################
  #### Save merged file to local and To Do: Drive ####
  #########################################
  # save the new data frame to a CSV file
  write.csv(site_merged,file.path(paste0(output_path, site,"_absparams.csv")) , row.names=FALSE, quote=FALSE)
  
  # define the target folder ID in Google Drive
  # this is the "merged" folder
  #drive_folder_id <- "1hlc9U54d70T5-hml_F9RM8FAiUCVRFmp"
  
  # upload the file to the specified Google Drive folder
  #drive_upload(media = "googledrive/USF12_absparams.csv", path = as_id(drive_folder_id))
  
}


##############################
#### TO DO ####
##############################
# previous code manually got rid of rows before 'deployment ' at the beginning, handle that automatically w datasheets



