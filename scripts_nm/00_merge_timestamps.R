##==============================================================================
## Project: FOR-NM
## Adapted from QuEST
## Original Author: Manuela Londono
## This script merges scan files in one and combines abs and parameters into one 
## from the separate sheets. 
##==============================================================================

library(readxl) #to read excel
library(googledrive)
library(dplyr)
library(readr)
library(parallel)

##########################
#### Import scan data ####
##########################
#### list, download in parallel, and read BOTH sheets from that same local file ####
scan <- googledrive::as_id("https://drive.google.com/drive/folders/143s0aoqhJB8XUgz2lgRHmgWqI8rG6XwW")
# list all xlsx files in the folder
scan_csvs <- googledrive::drive_ls(path = scan, pattern = "*.xlsx")

# Retry wrapper so one bad transfer doesn't kill the whole loop.
# Google Drive downloads occasionally hang (curl "Operation too slow" after
# ~10 min of near-zero throughput); this retries a few times with a short
# pause before giving up on that one file and moving on.
download_with_retry <- function(file, path, max_tries = 3, wait_sec = 15) {
  for (attempt in seq_len(max_tries)) {
    ok <- tryCatch({
      googledrive::drive_download(file = file, path = path, overwrite = TRUE)
      TRUE
    }, error = function(e) {
      message(sprintf("  attempt %d/%d failed for %s: %s", attempt, max_tries, path, conditionMessage(e)))
      FALSE
    })
    if (isTRUE(ok)) return(TRUE)
    if (attempt < max_tries) Sys.sleep(wait_sec)
  }
  FALSE
}

# 1. Download all files in PARALLEL first (parallel::mclapply -- base R,
#    fork-based so Mac/Linux only, which is what this runs on), THEN read
#    them sequentially from local disk. This was downloading one multi-MB
#    file at a time before -- with 20+ files that serial wait was the real
#    bottleneck, not the double-download issue fixed earlier.
n_workers <- max(1, parallel::detectCores() - 1)
message(sprintf("Downloading %d files (%d in parallel)...", nrow(scan_csvs), n_workers))

download_ok <- unlist(parallel::mclapply(seq_along(scan_csvs$id), function(i) {
  local_path <- file.path("data/raw", scan_csvs$name[i])
  download_with_retry(file = scan_csvs$id[i], path = local_path, max_tries = 3, wait_sec = 15)
}, mc.cores = n_workers))

failed_downloads <- scan_csvs$name[!download_ok]
if (length(failed_downloads) > 0) {
  warning("The following files failed to download after retries and were skipped:\n",
          paste(" -", failed_downloads, collapse = "\n"))
}

scan_list_param <- list()  # sheet 1: params
scan_list       <- list()  # sheet 2: compensated fingerprint / abs
failed_reads    <- character(0)

# 2. loop over each SUCCESSFULLY DOWNLOADED file and read it (local disk, fast)
for (i in seq_along(scan_csvs$id)) {
  if (!download_ok[i]) next
  local_path <- file.path("data/raw", scan_csvs$name[i])

  result <- tryCatch({
    #### sheet 1: params ####
    header_param <- read_excel(local_path, skip = 1, n_max = 1, col_names = FALSE)
    col_names_param <- as.character(unlist(header_param[1, ]))
    col_names_param[col_names_param == ""] <- paste0("X", seq_along(col_names_param[col_names_param == ""]))
    data_param <- read_excel(local_path, skip = 4, col_names = col_names_param)

    #### sheet 2: compensated fingerprint / abs ####
    header_abs <- read_excel(local_path, sheet = 2, skip = 1, n_max = 1, col_names = FALSE)
    col_names_abs <- as.character(unlist(header_abs[1, ]))
    col_names_abs[col_names_abs == ""] <- paste0("X", seq_along(col_names_abs[col_names_abs == ""]))
    data_abs <- read_excel(local_path, sheet = 2, skip = 4, col_names = col_names_abs)

    list(param = data_param, abs = data_abs)
  }, error = function(e) {
    message(sprintf("  failed to read/parse %s: %s", scan_csvs$name[i], conditionMessage(e)))
    NULL
  })

  if (is.null(result)) {
    failed_reads <- c(failed_reads, scan_csvs$name[i])
    next
  }

  scan_list_param[[scan_csvs$name[i]]] <- result$param
  scan_list[[scan_csvs$name[i]]]       <- result$abs
}

if (length(failed_reads) > 0) {
  warning("The following files downloaded but failed to read/parse and were skipped:\n",
          paste(" -", failed_reads, collapse = "\n"))
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
site_names <- c("USF24", "USF25", "USF40", "USF41")

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

