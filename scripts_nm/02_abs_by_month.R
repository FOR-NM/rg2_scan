##==============================================================================
## Project: FOR-NM
## Adapted from QuEST
## This script will plot all the absorbances by month to be able to take a 
## better look at your data and see what months may be problematic.
## Then you can flag spectra if they are too low or too high
##==============================================================================

library(dplyr)
library(spectrolab)
library(googledrive)

##############################################
#### Upload scan dataframe [with spectra] ####
##############################################
# site names
site_names <- c("USF24", "USF25", "USF40", "USF41")

local_dir <- "data/clean"

# # #Uncomment this block to pull the "<site>_clean.csv" files from # # # 
# the Drive "clean" folder before reading.

# # This is the "clean" folder
# scan <- googledrive::as_id("")
#
# # list all csv files in the folder
# merged <- googledrive::drive_ls(path = scan, type = "csv")
#
# for (site in site_names) {
#   # match by site code in the filename, same convention as 00/01 -- avoids
#   # needing to know each file's exact (nicknamed) name ahead of time
#   site_files <- merged[grepl(site, merged$name), ]
#
#   if (nrow(site_files) == 0) {
#     warning(sprintf("No Drive file matching site '%s' found in the clean folder -- skipping download, will use whatever's already in %s.", site, local_dir))
#     next
#   }
#   if (nrow(site_files) > 1) {
#     warning(sprintf("Multiple Drive files matched site '%s' -- downloading the first match (%s).", site, site_files$name[1]))
#   }
#
#   googledrive::drive_download(
#     file = site_files$id[1],
#     path = file.path(local_dir, paste0(site, "_clean.csv")),
#     overwrite = TRUE
#   )
# }

#################################
#### Load and tidy each site ####
#################################
scan_list <- list()

for (site in site_names) {
  local_path <- file.path(local_dir, paste0(site, "_absparams_clean.csv"))

  df <- read.csv(local_path, na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects

  # DateTime at midnight is missing 00:00:00 time, so filling in that time using grep
  midnight_rows <- grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}$", df$DateTime)
  df$DateTime[midnight_rows] <- paste(df$DateTime[midnight_rows], "00:00:00")

  # Convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")

  # rename spectral columns (strip the "X" prefix / ".nm" suffix R adds)
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))

  scan_list[[site]] <- df
}

####----------------------------------------------------------------------####
# ----------------------------------------------------------------------
# 2. Extract Unique Month-Year Combinations
# ----------------------------------------------------------------------
# Use the UNION of every site's dates, not just one site -- sites can have
# very different deployment lengths, so picking one site's months would
# silently skip any month that exists in a longer-running site but not in
# whichever site happens to be first. Computed as a plain vector (not
# stored as a column) so there's nothing to clean up afterward.
all_dates <- do.call(c, lapply(scan_list, function(df) df$DateTime))
all_dates <- all_dates[!is.na(all_dates)]
month_starts <- sort(unique(format(all_dates, "%Y-%m")))
unique_months <- format(as.Date(paste0(month_starts, "-01")), "%B %Y")

# ----------------------------------------------------------------------
# 3. Core Function to Process and Plot Data for a Single Month
# ----------------------------------------------------------------------

process_and_plot_month <- function(data_frame, df_name, month_year_label) {

  # a. Filter data for the specific month
  data_month <- data_frame %>%
    dplyr::filter(format(DateTime, "%B %Y") == month_year_label)

  # Check if there is any data for this month
  if (nrow(data_month) == 0) {
    message(paste("Skipping", df_name, "for", month_year_label, "- No data found."))
    return(NULL)
  }

  # b. Index spectral data by name -- any column whose name is purely
  # numeric (e.g. "200.00") is a wavelength column, so nothing to look up
  # by hand per file. Relies on rename_columns() already having stripped
  # the "X" prefix / ".nm" suffix off these columns.
  wl_cols <- names(data_month)[!is.na(suppressWarnings(as.numeric(names(data_month))))]
  scan.spec <- data_month[wl_cols]

  # c. Create absorbance object
  abs_data <- scan.spec

  # d. Create wavelength vector
  wl <- as.numeric(colnames(abs_data))

  # e. Create sample vector
  last_row <- as.numeric(nrow(abs_data))
  num_samples <- c(1:last_row)

  # f. Create the spectra object and plot
  # NOTE: The 'spectra' object is required for the 'plot' function from 'spectrolab'
  spec_object <- spectra(value = abs_data, bands = wl, names = num_samples)

  # g. Create the plot object
  p <- plot(spec_object, main = paste("Absorbance Spectra for", df_name, "-", month_year_label))

  # h. Save the plot with a png() device (base/spectrolab plot, not ggplot)
  filename <- paste0(df_name, "_Absorbance_", gsub(" ", "_", month_year_label), ".png")

  png(filename = filename, width = 800, height = 600)
  plot(spec_object, main = paste("Absorbance Spectra for", df_name, "-", month_year_label))
  dev.off()

  message(paste("Successfully saved plot:", filename))
}

# ----------------------------------------------------------------------
# 4. Loop Through All Months and Datasets
# ----------------------------------------------------------------------
for (month in unique_months) {
  for (site in names(scan_list)) {
    process_and_plot_month(data_frame = scan_list[[site]],
                           df_name = site,
                           month_year_label = month)
  }
}

