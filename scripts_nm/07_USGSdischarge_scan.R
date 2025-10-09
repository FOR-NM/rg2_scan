##==============================================================================
## Project: QuEST - Script to plot scan data with USGS discharge gauge data
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================
library(dataRetrieval) # download USGS discharge data
library(googledrive) # download docs from Drive
library(tidyverse)
library(readxl) # to read Excel
library(lubridate) # Edit date format
library(xts) # time series
library(ggplot2)

########################################
#### Clear folders that we will use ####
########################################
# list and delete all files in the folder
files <- list.files(path = "scan_figs", full.names = TRUE)
file.remove(files)

files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

#####################
#### Import Data ####
#####################
# load data from Google Drive. his is the "filtered" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1H7-rk99ettzbB84Ot5cj94eO-MnnjLZq")
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

# remove extra files for this
# scan_list <- scan_list[-c(4:9)]

#################
#### Tidying ####
#################
# change some names for easier manipulation
scan_list <- lapply(scan_list, function(df) {
  # make sure numeric variables are numeric
  df <- df %>%
    mutate(across(c(DOC_mg.l, NO3.N_mg.l, NO3_mg.l, TOC_mg.l, TSS_mg.l, Temp_C), as.numeric)) %>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
  
  return(df)
})


##################################
#### Pull USGS discharge data ####
##################################
# these are codes and functions specific to the USGS package (dataRetrieval)
retrieve_usgs_data <- function(start_date, end_date, site_no = "08315480", p_code = "00060") {
  #Retrieve the USGS discharge data as an instantaneous (uv) data type.
  usgs_data <- readNWISuv(siteNumbers = site_no, parameterCd = p_code, startDate = start_date, endDate = end_date)
  #Rename columns to more user-friendly names.
  usgs_data <- renameNWISColumns(usgs_data)
}

# retrieve USGS data for different s::can sites, each has different deployment dates
santafeUSGS_20 <- retrieve_usgs_data("2024-05-08", "2025-09-01")
santafeUSGS_12 <- retrieve_usgs_data("2024-05-07", "2025-09-01")
santafeUSGS_21 <- retrieve_usgs_data("2024-06-27", "2025-08-07")

santafeUSGS_12$DateTime <- santafeUSGS_12$dateTime
santafeUSGS_20$DateTime <- santafeUSGS_20$dateTime
santafeUSGS_21$DateTime <- santafeUSGS_21$dateTime

#####################################
#### Plot all variables separate ####
#####################################
# function to retrieve and plot USGS data with separate facets for each variable
plot_usgs_faceted <- function(df, usgs_df, label) {
  # convert to xts and merge data frames
  df_xts <- xts(df, order.by = df$DateTime)
  usgs_xts <- xts(usgs_df, order.by = usgs_df$DateTime)
  combined_xts <- merge(df_xts, usgs_xts, join = "outer")
  combined_df <- data.frame(DateTime = index(combined_xts), coredata(combined_xts))
  
  # convert columns to numeric, if necessary
  combined_df <- combined_df %>% mutate(across(c(Temp_C, TSS_clean, TOC_clean, NO3.N_clean, NO3_clean, DOC_clean, Flow_Inst), as.numeric))
  
  # reshape data to long format for faceting
  combined_long <- combined_df %>%
    dplyr::select(DateTime, Temp_C, TSS_clean, TOC_clean, NO3.N_clean, NO3_clean, DOC_clean, Flow_Inst) %>%
    pivot_longer(cols = -DateTime, names_to = "Variable", values_to = "Value")
  
  # plot using facet_wrap for each variable
  ggplot(data = combined_long, aes(x = DateTime, y = Value, color = Variable)) +
    geom_line() +
    facet_wrap(~Variable, scales = "free_y", ncol = 1) +  # separate facet for each variable
    scale_x_datetime(date_breaks = "1 week", date_labels = "%m/%d") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab(label) +
    ggtitle(label) +
    theme(legend.position = "none")  # Hide the legend as it's redundant with faceting
}

# plot
print(plot_usgs_faceted(scan_list[[1]], santafeUSGS_12, scan_csvs$name[1]))
print(plot_usgs_faceted(scan_list[[2]], santafeUSGS_21, scan_csvs$name[2]))
print(plot_usgs_faceted(scan_list[[3]], santafeUSGS_20, scan_csvs$name[3]))

### save figures to folder ###
for (i in seq_along(scan_filtered)) {
  # match the correct USGS data with each scan
  usgs_data <- switch(i,
                      santafeUSGS_12,
                      santafeUSGS_20,
                      santafeUSGS_21)
  
  # generate the plot
  plot <- plot_usgs_faceted(scan_filtered1[[i]], usgs_data, scan_csvs$name[i])
  
  # save the plot to a file
  ggsave(paste0("scan_figs/", scan_csvs$name[i], "_sep-outlier.png"), plot)
}

####################################
#### Merge USGS and s::can data ####
####################################
# function to merge USGS data with s::can data
merge_usgs_with_scan <- function(scan_df, usgs_df) {
  
  # convert both data frames to xts objects
  scan_xts <- xts(scan_df, order.by = scan_df$DateTime)
  usgs_xts <- xts(usgs_df, order.by = usgs_df$DateTime)
  
  # merge the xts objects
  combined_xts <- merge(scan_xts, usgs_xts, join = "outer")
  
  # convert back to data frame
  combined_df <- data.frame(DateTime = index(combined_xts), coredata(combined_xts))
  
  return(combined_df)
}

# list of USGS data frames corresponding to scan_filtered
usgs_list <- list(
  santafeUSGS_12,
  santafeUSGS_20,
  santafeUSGS_21
)

# merge USGS data with scan_filtered data frames
scan_with_usgs <- mapply(merge_usgs_with_scan, scan_filtered, usgs_list, SIMPLIFY = FALSE)

###################################
#### Save merged data to Drive ####
###################################
# function to remove file extension
remove_extension <- function(file_name) {
  sub("\\.[[:alnum:]]+$", "", file_name)
}

# loop through each data frame in the list
for (i in seq_along(scan_with_usgs)) {
  # Access the current data frame
  df <- scan_with_usgs[[i]]
  
  # define the file name and path
  clean_name <- remove_extension(scan_csvs$name[i])
  file_name <- paste0("googledrive/", clean_name, "_filtered.csv")
  
  # save the new data frame to a CSV file
  write.csv(df, file_name, row.names=FALSE, quote=FALSE)
  
  # define the target folder ID in Google Drive
  drive_folder_id <- "1DZktlQUHaot_r4e_fD9ip6zcxHWqslMP"
  
  # upload the file to the specified Google Drive folder
  drive_upload(media = file_name, path = as_id(drive_folder_id))
}


  
# plot using facet_wrap for each variable
ggplot(data = scan_list[["USF12_filtered_Buttercup.csv"]], aes(x = DateTime, y = DOC_clean)) +
  geom_point() +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%m/%d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(legend.position = "none")  # Hide the legend as it's redundant with faceting
