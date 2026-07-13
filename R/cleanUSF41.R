# script to fix USF41 on 06_11 file
##takes in a file id and keeps sheets pertinent to that.  writes to excel file 

# Install the package if you haven't already
install.packages("openxlsx")

# Load the library
library(openxlsx)


input_path<- "data/raw/"
file_name<- '2026-06-11_NMUSF41_aqua.xlsx'
local_path <- paste0(input_path, file_name )

# check in for correct id with scan 
usf41_id<- '25390201' # not using this variable 


# 1. Load the existing Excel workbook
wb <- loadWorkbook(local_path)

# 2. Remove the sheet by its exact name
removeWorksheet(wb, sheet = "24080205 Compensated Fingerprin")
removeWorksheet(wb, sheet = "24080205 Fingerprint _Abs_m_") 

#Remove specific columns from the first sheet by index 
# take in the appropriate set of columns for params 
target_sheet <- "Parameter"             # The name of the sheet you want to modify
cols_to_remove <- c(2:19)             # Specific range of columns to delete (e.g., Columns B to S)

# Read the data from the specific sheet into a dataframe
sheet_data <- read.xlsx(wb, sheet = target_sheet)

# Remove the specific range of columns from the dataframe
# (Note: if your Excel sheet has column headers, R reads them as row 1 or column names)
sheet_data_cleaned <- sheet_data[, -cols_to_remove]


# Remove and re-add the sheet to clear it out completely
removeWorksheet(wb, sheet = target_sheet)
addWorksheet(wb, sheetName = target_sheet)


# Write the cleaned dataframe back into the same sheet
writeData(wb, sheet = target_sheet, x = sheet_data_cleaned)


# Save the modified workbook back to the file
saveWorkbook(wb, local_path, overwrite = TRUE)


# to do, this introduces characters in columns that have numeric values.   have to avoid this 
  
