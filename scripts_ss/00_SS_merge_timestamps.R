##==============================================================================
## Project: QuEST
## Script to merge scan files in one (using timestamp)
##==============================================================================

library(readxl) #to read excel 

##########################
#### Import scan data ####
##########################

#### Import abs and parameter data ####
# This is the "raw" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1x6tPgXn-DgmBVvFTMG0TEo2AxqHqQLwV")
# List all CSV files in the folder
scan_csvs <- googledrive::drive_ls(path = scan)

# Create empty list to store data frames
scan_list <- list()

# Loop over each file in the `scan_csvs` data frame
for (i in seq_along(scan_csvs$id)) {
  # Define the local file path
  local_path <- file.path("googledrive", scan_csvs$name[i])
  
  # Download the file
  googledrive::drive_download(
    file = scan_csvs$id[i],
    path = local_path,
    overwrite = TRUE
  )
  
  # Read the header row (row 2)
  header <- read_excel(local_path, skip = 1, n_max = 1, col_names = FALSE)
  # Convert the header to a character vector and clean empty names
  col_names <- as.character(unlist(header[1, ]))
  col_names[col_names == ""] <- paste0("X", seq_along(col_names[col_names == ""]))
  
  # Read the data starting from row 4 using the header as column names
  data <- read_excel(local_path, skip = 4, col_names = col_names)
  
  # Store the data in the list
  scan_list[[scan_csvs$name[i]]] <- data
}
