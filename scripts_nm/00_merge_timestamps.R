##==============================================================================
## Project: FOR-NM
## Adapted from QuEST
## Original Author: Manuela Londono
## Script to merge scan files in one and combining abs and parameters data 
## from the separate sheets. 
##==============================================================================

library(readxl) #to read excel
library(googledrive)
library(dplyr)
library(readr)

##########################
#### Import scan data ####
##########################
#### list, download once, and read BOTH sheets from that same local file ####
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1UDrRJ10t04kXT2Op0W-GZz9vEe2B_C8H")
# list all xlsx files in the folder
scan_csvs <- googledrive::drive_ls(path = scan, pattern = "*.xlsx")
2
scan_list_param <- list()  # sheet 1: params
scan_list       <- list()  # sheet 2: compensated fingerprint / abs

# loop over each file in the `scan_csvs` data frame -- download ONCE per
# file and read both sheets from that same local copy (previously this
# downloaded every file twice, once in a separate loop per sheet)
for (i in seq_along(scan_csvs$id)) {
  # define the local file path
  local_path <- file.path("data/raw", scan_csvs$name[i])

  # download the file (once)
  googledrive::drive_download(
    file = scan_csvs$id[i],
    path = local_path,
    overwrite = TRUE
  )

  #### sheet 1: params ####
  # read the header row (row 2)
  header_param <- read_excel(local_path, skip = 1, n_max = 1, col_names = FALSE)
  # convert the header to a character vector and clean empty names
  col_names_param <- as.character(unlist(header_param[1, ]))
  col_names_param[col_names_param == ""] <- paste0("X", seq_along(col_names_param[col_names_param == ""]))
  # read the data starting from row 4 using the header as column names
  data_param <- read_excel(local_path, skip = 4, col_names = col_names_param)

  #### sheet 2: compensated fingerprint / abs ####
  header_abs <- read_excel(local_path, sheet = 2, skip = 1, n_max = 1, col_names = FALSE)
  col_names_abs <- as.character(unlist(header_abs[1, ]))
  col_names_abs[col_names_abs == ""] <- paste0("X", seq_along(col_names_abs[col_names_abs == ""]))
  data_abs <- read_excel(local_path, sheet = 2, skip = 4, col_names = col_names_abs)

  # store the data in the lists
  scan_list_param[[scan_csvs$name[i]]] <- data_param
  scan_list[[scan_csvs$name[i]]]       <- data_abs
}

####################################
#### Combine data for each site ####
####################################
# loop through each data frame in the list to change DateTime column name
for (i in seq_along(scan_list_param)) {
  # Access the current data frame
  df <- scan_list_param[[i]]

  # change names for easier handling
  colnames(df)[1] ="DateTime"

  # make sure DateTime is in the correct format
  df <- df %>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

  # # convert all text columns that contain numbers, stripping commas safely
  # # (introduced to fix the USF41 6-11 file, which had commas in numeric fields)
  # df <- df %>%
  #   mutate(across(where(is.character) & !contains("status"), ~parse_number(.)))

  # Update the data frame in the list
  scan_list_param[[i]] <- df
}

# site names
site_names <- c("USF02", "USF24", "USF25", "USF40", "USF41")

# group files in `scan_list_param` by matching `site_names` in file names

scan_list_by_site <- lapply(site_names, function(site) {
  # names(scan_list_param) gives the names of all files in scan_list_param.
  site_files <- names(scan_list_param)[grepl(site, names(scan_list_param))]
  # grep checks if the current site (e.g., USF12) appears in each file name in scan_list_param.
  # this returns a logical vector (TRUE for matches, FALSE otherwise).
  scan_list_param[site_files] # select only the files for this site
  # the [ ] indexing selects only the file names where the match is TRUE.
})

# name the list by site
names(scan_list_by_site) <- site_names

# combine data for each site
combined_by_site <- lapply(scan_list_by_site, function(site_data_list) {
  # bind rows of all data frames for the site
  bind_rows(site_data_list) %>%
    arrange(DateTime) %>%  # ensure chronological order if 'DateTime' exists
    distinct(DateTime, .keep_all = TRUE) # remove duplicates
})

##############################
#### Save combined files  ####
##############################
# ensure DateTime column is properly formatted
combined_by_site <- lapply(combined_by_site, function(df) {
  df$DateTime <- format(df$DateTime, "%Y-%m-%d %H:%M:%S") # ensure consistent format
  return(df)
})

lapply(names(combined_by_site), function(site) {
  write.csv(combined_by_site[[site]], file.path("data/merged_timestamps", paste0(site, "_params.csv")))
})

##==============================================================================
## Now we combine the compensated abs tab (sheet 2 of the same excel files)
## -- already read into `scan_list` in the combined download loop above.
##==============================================================================

####################################
#### Combine data for each site ####
####################################
# change DateTime column name
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]

  # change names for easier handling
  colnames(df)[1] ="DateTime"

  # make sure DateTime is in the correct format
  df <- df %>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

  # convert all text columns that contain numbers, stripping commas safely
  # (introduced to fix the USF41 6-11 file, which had commas in numeric fields)
  df <- df %>%
    mutate(across(where(is.character) & !contains("status"), ~parse_number(.)))

  # update the data frame in the list
  scan_list[[i]] <- df
}

# group files in `scan_list` by matching `site_names` in file names
scan_list_by_site <- lapply(site_names, function(site) {
  # names(scan_list) gives the names of all files in scan_list.
  site_files <- names(scan_list)[grepl(site, names(scan_list))]
  # grep checks if the current site (e.g., SSM01) appears in each file name in scan_list.
  # this returns a logical vector (TRUE for matches, FALSE otherwise).
  scan_list[site_files] # select only the files for this site
  # the [ ] indexing selects only the file names where the match is TRUE.
})

# name the list by site
names(scan_list_by_site) <- site_names

# makes every non DateTime column to numeric
scan_list_by_site <- lapply(scan_list_by_site, function(site_data_list) {
  lapply(site_data_list, function(df) {
    df %>% mutate(across(-DateTime, ~ suppressWarnings(as.numeric(.))))
  })
})
# combine data for each site
combined_by_site_compfing <- lapply(scan_list_by_site, function(site_data_list) {
  # bind rows of all data frames for the site
  bind_rows(site_data_list) %>%
    arrange(DateTime) %>%  # ensure chronological order if 'DateTime' exists
    distinct(DateTime, .keep_all = TRUE) # remove duplicates
})

##############################
#### Save combined files  ####
##############################
# ensure DateTime column is properly formatted
combined_by_site_compfing <- lapply(combined_by_site_compfing, function(df) {
  df$DateTime <- format(df$DateTime, "%Y-%m-%d %H:%M:%S") # ensure consistent format
  return(df)
})

lapply(names(combined_by_site_compfing), function(site) {
  write.csv(combined_by_site_compfing[[site]], file.path("data/merged_timestamps", paste0(site, "_abs.csv")))
})

###############################################
#### Combine abs and params into one file  ####
###############################################
#merged_timestamps folder
input_path<- "data/merged_timestamps/"
output_path<-"data/merged_params_and_abs/"
dir.create(file.path('data', 'merged_params_and_abs'))

for (site in site_names){
  print(site)
  # to do , gdrive flow, for now read local file 
  
  
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
  
  ####################################################
  #### Save merged file to local and To Do: Drive ####
  ####################################################
  # save the new data frame to a CSV file
  write.csv(site_merged,file.path(paste0(output_path, site,"_absparams.csv")) , row.names=FALSE, quote=FALSE)
  
  # define the target folder ID in Google Drive
  # this is the "merged" folder
  #drive_folder_id <- "1hlc9U54d70T5-hml_F9RM8FAiUCVRFmp"
  
  # upload the file to the specified Google Drive folder
  #drive_upload(media = "googledrive/USF12_absparams.csv", path = as_id(drive_folder_id))
  
}

