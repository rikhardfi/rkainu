# Kainu et al. 2015 reference spirometry values, adjustment imports
library(readxl)
library(dplyr)

# Specify the file path
file_path <- "kainu/Kainu et al. 2015 supplement 1.xlsx"

# Loop through each sheet, import the first 4 columns, and combine them into a single dataset
Kainu_adjustments <- lapply(excel_sheets(file_path), function(sheet) {
  
  # Import the first four columns
  data <- read_excel(file_path, sheet = sheet, range = cell_cols(1:4))
  
  # Create the 'sex' column based on the sheet name
  data$sex <- ifelse(grepl("female", tolower(sheet)), 1, ifelse(grepl("male", tolower(sheet)), 0, NA))
  
  # Extract the text between "Kainu " and " male"/" female" for the 'flow_volume_variable' column
  flow_volume_variable <- sub("Kainu (.*?) (male|female)", "\\1", sheet)
  data$flow_volume_variable <- flow_volume_variable
  
  # Return the dataset
  return(data)
}) %>%
  # Combine all datasets into one
  bind_rows()