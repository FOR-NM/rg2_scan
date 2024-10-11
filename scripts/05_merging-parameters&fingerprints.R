##==============================================================================
## Project: QuEST
## Script to merge parameters and fingerprint s::can data
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(googledrive)
library(readxl)
library(purrr)

#####################
#### Import Data ####
#####################
# Load data from Google Drive
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1np2B4bSWaNMIYE2FHL3YOnZ20FRudsEy")
scan_csvs <- googledrive::drive_ls(path = scan, type = "xlsx")

# Create empty lists to store data frames for the first and second sheets
params_list <- list()
fingerprints_list <- list()

# Loop over each file in `scan_csvs` and read the first two sheets
for (i in seq_along(scan_csvs$id)) {
  local_path <- file.path("googledrive", scan_csvs$name[i])
  
  # Download the file
  googledrive::drive_download(file = scan_csvs$id[i], path = local_path, overwrite = TRUE)
  
  # Attempt to read the first sheet
  params <- read_excel(local_path, sheet = 1, skip = 1)
  
  # Attempt to read the second sheet
  fingerprints <- read_excel(local_path, sheet = 2, skip = 1)
  
  # Store the data frames in their respective lists if not NULL
  params_list[[scan_csvs$name[i]]] <- params

  fingerprints_list[[scan_csvs$name[i]]] <- fingerprints
}

# Print the list names to verify
print(names(params_list))
print(names(fingerprints_list))

#####################
#### Merge lists ####
#####################
# The map2 function is used to iterate over two lists or vectors in parallel.
#.x corresponds to elements of sheet1_list.
#.y corresponds to elements of sheet2_list.

merged_list <- map2(params_list, fingerprints_list, ~ {
  merge(.x, .y, by = 'Parameter:', all = TRUE)
  })
