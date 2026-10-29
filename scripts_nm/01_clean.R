##==============================================================================
## Project: QuEST - Script to tidy up a bit scan data and plot it
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================
library(googledrive) # download docs from Drive
library(tidyverse)
library(lubridate) # edit date format
library(ggplot2)

#####################
#### Import Data ####
#####################
# load data from Google drive. This is the "merge timestamps" folder
#scan <- googledrive::as_id("https://drive.google.com/drive/folders/1-dUxVn1hBWy2MpHeIjVt-2QSujpVhijy")
# list all CSV files in the folder
#scan_csvs <- googledrive::drive_ls(path = scan)
input_path<- "data/merged_params_and_abs/" #local path

scan<- input_path
scan_csvs<- list.files(path = scan, pattern = "\\.csv$")

# create empty list to store data frames
scan_list <- list()

# loop over each file in the `scan_csvs` data frame
for (i in seq_along(scan_csvs)) {
  # define the local file path
  local_path <- file.path(input_path, scan_csvs[i])
  
  # read the data starting from row 4 using the header as column names
  data <- read_csv(local_path)
  
  # store the data in the list
  scan_list[[scan_csvs[i]]] <- data
}

#################
#### Tidying ####
#################
# change some names for easier manipulation
scan_list <- lapply(scan_list, function(df) {
  # rename columns by matching the existing names
  df <- df %>%
    rename(
      'DOC_mg.l' = 'DOCeq..mg.l....Measured.value',
      'NO3.N_mg.l' = 'NO3.Neq..mg.l....Measured.value',
      'NO3_mg.l' =  'NO3eq..mg.l....Measured.value',
      'TOC_mg.l' = 'TOCeq..mg.l....Measured.value',
      'TSS_mg.l' = 'TSSeq..mg.l....Measured.value',
      'Temp_C' = "Temperature_21...C....Measured.value",
    )
  
  # ensure numeric variables are converted to numeric
  df <- df %>%
    mutate(across(c(DOC_mg.l, NO3.N_mg.l, NO3_mg.l, TOC_mg.l, TSS_mg.l, Temp_C), as.numeric)) %>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
  
  return(df)
})

#####################################################################
#### Plot all variables separate before removing no medium reads ####
#####################################################################
# function to plot each variable separately in the same panel
plot_variables <- function(df, file_name) {
  # ensure column selection works correctly
  df_long <- df %>%
    dplyr::select("DateTime", "Temp_C", "TSS_mg.l", "TOC_mg.l", "NO3.N_mg.l", "NO3_mg.l", "DOC_mg.l") %>%
    pivot_longer(cols = -DateTime, names_to = "Variable", values_to = "Value")
  
  # generate the plot
  ggplot(data = df_long, aes(x = DateTime, y = Value, color = Variable)) +
    geom_line() +
    facet_wrap(~Variable, scales = "free_y", ncol = 1) +  # separate plot for each variable, stacked vertically
    scale_x_datetime(date_breaks = "15 days", date_labels = "%m/%d") +
    ggtitle(file_name) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Measured Value") +
    theme(legend.position = "none")  # hide legend since we have separate panels
}

# generate plots
print(plot_variables(scan_list[[1]], scan_csvs[1]))
print(plot_variables(scan_list[[2]], scan_csvs[2]))
print(plot_variables(scan_list[[3]], scan_csvs[3]))
print(plot_variables(scan_list[[4]], scan_csvs[4]))

### save figures to folder ###
for (i in seq_along(scan_list)) {
  ggsave(paste0("scan_figs/", scan_csvs[i], "raw.png"), plot_variables(scan_list[[i]], scan_csvs[i]))
}

#########################################################################################
#### Count every status flag that actually appears in the data, per file           ####
#########################################################################################
# Status codes aren't a fixed, known list, and a cell can hold several at once
# joined with ":" (e.g. "MATH_ERR:NO_MEDIUM:COMP_BELOW_REF:VAL_BELOW:NEG_MED").
# Instead of hardcoding each flag to check for (and having to add a new line
# every time a new one turns up, like MED_TEMP_LOW/COMP_BELOW_REF just did),
# this finds every distinct flag actually present and counts rows for all of
# them automatically.

# for each file: split every status cell's compound value on ":", collapse
# to the flags actually present in each row (so a flag repeated across
# several status columns in the same row only counts once for that row),
# and tally how many rows contain each flag
file_flag_counts <- lapply(scan_list, function(data) {
  # filter to only character columns (the status columns, at this point in the pipeline)
  char_data <- data[, sapply(data, is.character), drop = FALSE]
  
  row_flags <- apply(char_data, 1, function(row) {
    row <- row[!is.na(row) & row != ""]
    unique(unlist(strsplit(row, ":", fixed = TRUE)))
  })
  
  table(unlist(row_flags))
})

# union of every flag seen across ALL files, so every file gets a column for
# every flag anyone has -- 0 where that file never saw it, instead of the
# column being missing
all_flags <- sort(unique(unlist(lapply(file_flag_counts, names))))

# combine into one wide table: one row per file, one column per flag
final_count_table <- do.call(rbind, lapply(names(file_flag_counts), function(file_name) {
  counts <- file_flag_counts[[file_name]]
  row_counts <- setNames(as.list(rep(0L, length(all_flags))), all_flags)
  row_counts[names(counts)] <- as.integer(counts)
  data.frame(File = file_name, as.data.frame(row_counts, check.names = FALSE))
}))

# print the final table
print(final_count_table)

### save the final table to a CSV file
write.csv(final_count_table, "final_NAcount_table.csv", row.names = TRUE)

#################################################
#### Clean service dates (out of water days) ####
#################################################
# apply changes to status columns across all data frames in the list
scan_list <- lapply(scan_list, function(df) {
  
  # rename columns by matching the existing names
  df <- df %>%
    rename(
      'DOC_status' = 'DOCeq..mg.l....Measured.status',  # rename the status column for DOC
      'NO3.N_status' = 'NO3.Neq..mg.l....Measured.status',  # rename the status column for NO3.N
      'NO3_status' = 'NO3eq..mg.l....Measured.status',  # rename the status column for NO3
      'TOC_status' = 'TOCeq..mg.l....Measured.status',  # rename the status column for TOC
      'TSS_status' = 'TSSeq..mg.l....Measured.status'  # rename the status column for TSS
    )
  
  # ensure numeric variables are converted to numeric
  df <- df %>%
    mutate(across(c(DOC_mg.l, NO3.N_mg.l, NO3_mg.l, TOC_mg.l, TSS_mg.l, Temp_C), as.numeric)) %>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
  
  # Status can be compound (e.g. "MATH_ERR:NO_MEDIUM:COMP_BELOW_REF:VAL_BELOW:NEG_MED"),
  # so an exact match against a fixed list of strings misses any combination
  # that isn't byte-for-byte one of those -- switched to a substring check:
  # any status containing "NO_MEDIUM" anywhere in it counts.
  # !is.na(status) guards against grepl() returning NA for a missing status
  # (unlike %in%, which safely returns FALSE for NA) -- without that guard,
  # a row with no status at all would turn into NA here instead of staying
  # as its original (not-flagged) value.
  has_no_medium <- function(status) {
    !is.na(status) & grepl("NO_MEDIUM", status, fixed = TRUE)
  }
  
  # overwrite the value columns in place with NA wherever their own status
  # flags a bad read -- no separate "_clean" columns. The *_status columns
  # (kept as-is) already say WHY a value is NA, so there's no need for an
  # extra flag column on top of that.
  df <- df %>%
    mutate(
      DOC_mg.l = ifelse(has_no_medium(DOC_status), NA, DOC_mg.l),
      NO3.N_mg.l = ifelse(has_no_medium(NO3.N_status), NA, NO3.N_mg.l),
      NO3_mg.l = ifelse(has_no_medium(NO3_status), NA, NO3_mg.l),
      TOC_mg.l = ifelse(has_no_medium(TOC_status), NA, TOC_mg.l),
      TSS_mg.l = ifelse(has_no_medium(TSS_status), NA, TSS_mg.l)
    )
  
  # find all spectral columns (those starting with "X" and ending with ".nm")
  spectra_cols <- grep("^X[0-9]+\\.[0-9]+\\.nm$", colnames(df), value = TRUE)
  
  # If there are spectral columns, clean them
  if (length(spectra_cols) > 0) {
    # loop through each spectral column and apply the NA logic based on status
    for (col in spectra_cols) {
      
      # apply the NA logic based on status values
      df[[col]] <- ifelse(
        has_no_medium(df$DOC_status) |
          has_no_medium(df$NO3.N_status) |
          has_no_medium(df$NO3_status) |
          has_no_medium(df$TOC_status) |
          has_no_medium(df$TSS_status),
        NA, df[[col]]
      )
    }
  }
  
  # return the cleaned dataframe -- without this, the last expression run
  # above is the if/for block, which evaluates to NULL, so lapply() would
  # silently collect a list of NULLs instead of the cleaned dataframes
  return(df)
})

#####################################
#### Plot all variables separate ####
#####################################
# function to plot each variable separately in the same panel, to do: rename 
plot_variables <- function(df, file_name) {
  # ensure column selection works correctly
  df_long <- df %>%
    dplyr::select("DateTime", "Temp_C", "TSS_mg.l", "TOC_mg.l", "NO3.N_mg.l", "NO3_mg.l", "DOC_mg.l") %>%
    pivot_longer(cols = -DateTime, names_to = "Variable", values_to = "Value")
  
  # generate the plot
  ggplot(data = df_long, aes(x = DateTime, y = Value, color = Variable)) +
    geom_line() +
    facet_wrap(~Variable, scales = "free_y", ncol = 1) +  # separate plot for each variable, stacked vertically
    scale_x_datetime(date_breaks = "15 days", date_labels = "%m/%d") +
    ggtitle(file_name) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Measured Value") +
    theme(legend.position = "none")  # hide legend since we have separate panels
}

# generate plots and save 
for (i in seq_along(scan_csvs)){
  print(plot_variables(scan_list[[i]], scan_csvs[i]))
  ggsave(paste0("scan_figs/", scan_csvs[i], ".png"), plot_variables(scan_list[[i]], scan_csvs[i]))
}

# ###################################
# #### Plot just a section of it #### To do: erase this? or place it somewhere else 
# ###################################
# Date1 <- as.Date("2025-08-25", "%Y-%m-%d")
# Date2 <- as.Date("2025-08-30", "%Y-%m-%d")
# 
# # create empty list to store data frames
# scan_subdf <- list()
# 
# # loop through each data frame in the list to change DateTime column name
# for (i in seq_along(scan_filtered1)) {
#   # Access the current data frame
#   df <- scan_filtered1[[i]]
#   
#   # change time frame
#   subdf <- df[df$DateTime < Date2 & df$DateTime > Date1,]
#   
#   # update the data frame in the list
#   scan_subdf[[i]] <- subdf
# }
# 
# # function to plot each variable separately in the same panel
# plot_variables <- function(df, file_name) {
#   # ensure column selection works correctly
#   df_long <- df %>%
#     dplyr::select("DateTime", "Temp_C", "TSS_mg.l", "TOC_mg.l", "NO3.N_mg.l", "NO3_mg.l", "DOC_mg.l") %>%
#     pivot_longer(cols = -DateTime, names_to = "Variable", values_to = "Value")
#   
#   # generate the plot
#   ggplot(data = df_long, aes(x = DateTime, y = Value, color = Variable)) +
#     geom_line() +
#     facet_wrap(~Variable, scales = "free_y", ncol = 1) +  # separate plot for each variable, stacked vertically
#     scale_x_datetime(date_breaks = "7 days", date_labels = "%m/%d") +
#     ggtitle(file_name) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#     ylab("Measured Value") +
#     theme(legend.position = "none")  # hide legend since we have separate panels
# }
# 
# # generate plots
# print(plot_variables(scan_subdf[[1]], scan_csvs$name[1]))
# print(plot_variables(scan_subdf[[2]], scan_csvs$name[2]))
# print(plot_variables(scan_subdf[[3]], scan_csvs$name[3]))
# 
# ### save figures to folder ###
# for (i in seq_along(scan_subdf)) {
#   ggsave(paste0("scan_figs/", scan_csvs$name[i], "_subdf.png"), plot_variables(scan_subdf[[i]], scan_csvs$name[i]))
# }

####################################
#### Save cleaned data to Drive ####
####################################
# function to remove file extension
remove_extension <- function(file_name) {
  sub("\\.[[:alnum:]]+$", "", file_name)
}

# loop through each data frame in the list
for (i in seq_along(scan_list)) {
  # access the current data frame
  df <- scan_list[[i]]
  
  # define the file name and path
  clean_name <- remove_extension(scan_csvs[i])
  file_name <- file.path("data/clean", paste0(clean_name, "_clean.csv"))
  
  # save the new data frame to a CSV file
  write.csv(df, file_name, row.names=FALSE, quote=FALSE)
  
  # define the target folder ID in Google Drive, this is the "clean" folder
  drive_folder_id <- "1j_I0brd1bzAfHlG3is0WRRayPR1MEcUs"
  
  # upload the file to the specified Google Drive folder
  drive_put(media = file_name, path = as_id(drive_folder_id))
}
