##==============================================================================
## Project: QuEST - Script to plot scan data with USGS discharge gauge data
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================
library(dataRetrieval) # download USGS discharge data
library(googledrive) # download docs from Drive
library(tidyverse)
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
files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

################################
#### Import calibrated Data ####
################################
# load data from Google Drive. his is the "predicted" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1wa1ycqUYv56y3fTn1-VaN2K-NLU3rFeU")
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

#######################################
#### Import GLOBAL calibrated Data ####
#######################################
# load data from Google Drive. his is the "with chem" folder
scan <- googledrive::as_id("https://drive.google.com/drive/u/1/folders/1qjM3Zze-I5ycFCHNcd997UG6gYXBUoX8")
global_csvs <- googledrive::drive_ls(path = scan, type = "csv")
3

# create empty list to store data frames
global_list <- list()

# loop over each file in the `global_csvs` data frame
for (i in seq_along(global_csvs$id)) {
  # define the local file path
  local_path <- file.path("googledrive", global_csvs$name[i])
  
  # download the file
  googledrive::drive_download(
    file = global_csvs$id[i],
    path = local_path,
    overwrite = TRUE
  )
  
  # read the header row (row 2)
  header <- read_csv(local_path, n_max = 1, col_names = TRUE)
  
  # read the data starting from row 4 using the header as column names
  data <- read_csv(local_path)
  
  # store the data in the list
  global_list[[global_csvs$name[i]]] <- data
}

# loop over each data frame in the list
for (file_name in names(global_list)) {
  # Get the data frame
  data <- global_list[[file_name]]
  # filter to only character columns
  data <- data[, -c(19:110)]
  # store the data in the list
  global_list[[file_name]] <- data
}

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
santafeUSGS_20 <- retrieve_usgs_data("2024-05-08", "2025-10-20")
santafeUSGS_12 <- retrieve_usgs_data("2024-05-07", "2025-10-20")
santafeUSGS_21 <- retrieve_usgs_data("2024-06-27", "2025-10-20")

santafeUSGS_12$DateTime <- santafeUSGS_12$dateTime
santafeUSGS_20$DateTime <- santafeUSGS_20$dateTime
santafeUSGS_21$DateTime <- santafeUSGS_21$dateTime

####################################
#### Combine data for each site ####
####################################
# combine lists
mlist =  c(scan_list, global_list)

# loop through each data frame in the list to change DateTime column name
for (i in seq_along(mlist)) {
  # Access the current data frame
  df <- mlist[[i]]
  # change name of first column
  colnames(df)[1] ="DateTime"
  # Update the data frame in the list
  mlist[[i]] <- df
}

# site names
site_names <- c("USF12", "USF20", "USF21")

# group files in `mlist` by matching `site_names` in file names
scan_list_by_site <- lapply(site_names, function(site) {
  # names(mlist) gives the names of all files in mlist
  site_files <- names(mlist)[grepl(site, names(mlist))] 
  # grep checks if the current site (e.g., USF12) appears in each file name in mlist 
  # this returns a logical vector (TRUE for matches, FALSE otherwise).
  mlist[site_files] # select only the files for this site
  # the [ ] indexing selects only the file names where the match is TRUE.
})

# name the list by site
names(scan_list_by_site) <- site_names

# combine data for each site
combined_by_site <- lapply(scan_list_by_site, function(site_data_list) {
  # This merges all data frames in the sub-list by the 'DateTime' column
  site_data_list %>% reduce(full_join, by = "DateTime")
})

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
scan_with_usgs <- mapply(merge_usgs_with_scan, combined_by_site, usgs_list, SIMPLIFY = FALSE)

##################################################
#### Plot all variables separate for raw data ####
##################################################
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

#########################################################
#### Plot all variables separate for calibrated data ####
#########################################################
# function to retrieve and plot USGS data with separate facets for each variable
plot_usgs_faceted <- function(df, usgs_df, label) {
  # convert to xts and merge data frames
  df_xts <- xts(df, order.by = df$DateTime)
  usgs_xts <- xts(usgs_df, order.by = usgs_df$DateTime)
  combined_xts <- merge(df_xts, usgs_xts, join = "outer")
  combined_df <- data.frame(DateTime = index(combined_xts), coredata(combined_xts))
  
  # convert columns to numeric, if necessary
  combined_df <- combined_df %>% mutate(across(c(NO3N.comps_clean, NO3N.comps, NO3.N_mg.l, DOC.comps_clean, DOC.comps, DOC_mg.l, Flow_Inst), as.numeric))
  
  # reshape data to long format for faceting
  combined_long <- combined_df %>%
    dplyr::select(DateTime, NO3N.comps_clean, NO3N.comps, NO3.N_mg.l, DOC.comps_clean, DOC.comps, DOC_mg.l, Flow_Inst) %>%
    pivot_longer(cols = -DateTime, names_to = "Variable", values_to = "Value")

  # plot using facet_wrap for each variable
  ggplot(data = combined_long, aes(x = DateTime, y = Value, color = Variable)) +
    geom_line() +
    facet_wrap(~Variable, scales = "free_y", ncol = 1) +  # separate facet for each variable
    scale_x_datetime(date_breaks = "2 week", date_labels = "%m/%d") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(label) +
    theme(legend.position = "none") 
}

# plot
print(plot_usgs_faceted(combined_by_site[[1]], santafeUSGS_12, "USF12"))
print(plot_usgs_faceted(combined_by_site[[2]], santafeUSGS_20, "USF20"))
print(plot_usgs_faceted(combined_by_site[[3]], santafeUSGS_21, "USF21"))

### save figures to folder ###
for (i in seq_along(combined_by_site)) {
  # match the correct USGS data with each scan
  usgs_data <- switch(i,
                      santafeUSGS_12,
                      santafeUSGS_20,
                      santafeUSGS_21)
  
  # generate the plot
  plot <- plot_usgs_faceted(combined_by_site[[i]], usgs_data, scan_csvs$name[i])
  
  # save the plot to a file
  ggsave(paste0("scan_figs/", scan_csvs$name[i], "_sep-outlier.png"), plot)
}

###################################
#### Plot just a section of it ####
###################################
short_USGS12 <- santafeUSGS_12 %>% 
  filter(as.POSIXct(DateTime) > as.POSIXct("2024-08-10") & as.POSIXct(DateTime) < as.POSIXct("2024-09-15"))
short_USGS20 <- santafeUSGS_20 %>% 
  filter(as.POSIXct(DateTime) > as.POSIXct("2024-08-10") & as.POSIXct(DateTime) < as.POSIXct("2024-09-15"))
short_USGS21<- santafeUSGS_21 %>% 
  filter(as.POSIXct(DateTime) > as.POSIXct("2024-08-10") & as.POSIXct(DateTime) < as.POSIXct("2024-09-15"))

Date1 <- as.Date("2024-08-10", "%Y-%m-%d")
Date2 <- as.Date("2024-09-15", "%Y-%m-%d")

# create empty list to store data frames
scan_subdf <- list()
# loop through each data frame in the list to change DateTime column name
for (i in seq_along(combined_by_site)) {
  # Access the current data frame
  df <- combined_by_site[[i]]
  # change time frame
  subdf <- df[df$DateTime < Date2 & df$DateTime > Date1,]
  # update the data frame in the list
  scan_subdf[[i]] <- subdf
}

# function to retrieve and plot USGS data with separate facets for each variable
plot_usgs_faceted <- function(df, usgs_df, label) {
  # convert to xts and merge data frames
  df_xts <- xts(df, order.by = df$DateTime)
  usgs_xts <- xts(usgs_df, order.by = usgs_df$DateTime)
  combined_xts <- merge(df_xts, usgs_xts, join = "outer")
  combined_df <- data.frame(DateTime = index(combined_xts), coredata(combined_xts))
  
  # convert columns to numeric, if necessary
  combined_df <- combined_df %>% mutate(across(c(NO3N.comps_clean, NO3N.comps, NO3.N_mg.l, DOC.comps_clean, DOC.comps, DOC_mg.l, Flow_Inst), as.numeric))
  
  # reshape data to long format for faceting
  combined_long <- combined_df %>%
    dplyr::select(DateTime, NO3N.comps_clean, NO3N.comps, NO3.N_mg.l, DOC.comps_clean, DOC.comps, DOC_mg.l, Flow_Inst) %>%
    pivot_longer(cols = -DateTime, names_to = "Variable", values_to = "Value")
  
  # plot using facet_wrap for each variable
  ggplot(data = combined_long, aes(x = DateTime, y = Value, color = Variable)) +
    geom_line() +
    facet_wrap(~Variable, scales = "free_y", ncol = 1) +  # separate facet for each variable
    scale_x_datetime(date_breaks = "1 week", date_labels = "%m/%d") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(label) +
    theme(legend.position = "none") 
}

# generate plots
print(plot_usgs_faceted(scan_subdf[[1]], short_USGS12, "USF12"))
print(plot_usgs_faceted(scan_subdf[[2]], short_USGS20, "USF20"))
print(plot_usgs_faceted(scan_subdf[[3]], short_USGS21, "USF21"))

#######################
#### Plot with SNV ####
#######################
USF12 <- scan_with_usgs[["USF12"]]

USF12 <- USF12 %>%
  rename("CalibratedCleaned" = "NO3N.comps_clean",
         "Calibrated" = "NO3N.comps",
         "SNV" = "NO3N12.2.comps",
        "Global" = "NO3.N_mg.l")

plot_usgs_faceted <- function(df, usgs_df, label) {
  # convert to xts and merge data frames
  df_xts <- xts(df, order.by = df$DateTime)
  usgs_xts <- xts(usgs_df, order.by = usgs_df$DateTime)
  combined_xts <- merge(df_xts, usgs_xts, join = "outer")
  combined_df <- data.frame(DateTime = index(combined_xts), coredata(combined_xts))
  
  # convert columns to numeric, if necessary
  USF12 <- USF12 %>% mutate(across(c(CalibratedCleaned, Calibrated, SNV, Global, Flow_Inst), as.numeric))
  
  # reshape data to long format for faceting
  USF12_long <- USF12 %>%
  dplyr::select(DateTime, CalibratedCleaned, Calibrated, SNV, Global, Flow_Inst) %>%
  pivot_longer(cols = -DateTime, names_to = "Variable", values_to = "Value")
  
  # plot using facet_wrap for each variable
  ggplot(data = USF12_long, aes(x = DateTime, y = Value, color = Variable)) +
  geom_line() +
  facet_wrap(~Variable, scales = "free_y", ncol = 1) +  # separate facet for each variable
  scale_x_datetime(date_breaks = "2 week", date_labels = "%m/%d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(label) +
  theme(legend.position = "none") 
}

# generate plots
print(plot_usgs_faceted(scan_with_usgs[[1]], santafeUSGS_12, "USF12"))

############################################
#### Plot time series with grab samples ####
############################################
USF12 <- scan_with_usgs[["USF12"]]

# make sure everything is numeric
USF12$NO3N.comps <- as.numeric(USF12$NO3N.comps)
USF12$NO3..mg.N.L. <- as.numeric(USF12$NO3..mg.N.L.)

# plot
ggplot(data = USF12) +
  geom_line(aes(x = DateTime, y = NO3N.comps, color = "Sensor (SNV-PLSR)"), 
            linewidth = 0.5) +
  geom_point(aes(x = DateTime, y = NO3..mg.N.L., color = "Grab Samples"), 
             size = 3) +
  scale_color_manual(values = c("Sensor (SNV-PLSR)" = "blue", "Grab Samples" = "red")) +
  labs(y = "NO3-N (mg/L)", x = "Date") +
  theme_bw()

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

