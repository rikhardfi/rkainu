# Kainu coefficients
library(readxl)
library(dplyr)

# Create the dataset by reading each sheet
coefficients_dataset <- lapply(excel_sheets(file_path), function(sheet) {
  
  # Read specific cells for coefficients
  a0 <- read_excel(file_path, sheet = sheet, range = "I4", col_names = FALSE, col_types = "numeric") %>% as.numeric()
  a1 <- read_excel(file_path, sheet = sheet, range = "I5", col_names = FALSE, col_types = "numeric") %>% as.numeric()
  a2 <- read_excel(file_path, sheet = sheet, range = "I6", col_names = FALSE, col_types = "numeric") %>% as.numeric()
  b0 <- read_excel(file_path, sheet = sheet, range = "K4", col_names = FALSE, col_types = "numeric") %>% as.numeric()
  b1 <- read_excel(file_path, sheet = sheet, range = "K6", col_names = FALSE, col_types = "numeric") %>% as.numeric()
  
  # Create the 'sex' column based on the sheet name
  sex <- ifelse(grepl("female", tolower(sheet)), 1, ifelse(grepl("male", tolower(sheet)), 0, NA))
  
  # Extract the text between "Kainu " and " male"/" female" for the 'flow_volume_variable' column
  flow_volume_variable <- sub("Kainu (.*?) (male|female)", "\\1", sheet)
  
  # Create a data frame for the current sheet
  data.frame(flow_volume_variable = flow_volume_variable,
             sex = sex,
             a0 = a0,
             a1 = a1,
             a2 = a2,
             b0 = b0,
             b1 = b1)
}) %>%
  # Combine all sheet data into a single dataset
  bind_rows()

# Display the resulting dataset
print(coefficients_dataset)