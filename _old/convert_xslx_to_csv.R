library(readxl)
library(dplyr)
library(janitor)

# Define the parent directory containing all monthly folders
parent_folder <- "/Users/estellepfitzer/Desktop/PB_Downloads/"

# List all month folders (excluding hidden files)
month_folders <- list.dirs(parent_folder, full.names = TRUE, recursive = FALSE)

# Function to clean and process each file
clean_data <- function(file_path) {
  df <- read_excel(file_path, col_names = FALSE)  # Read without headers
  
  colnames(df) <- df[9, ]  # Set row 9 as column names
  df <- df[-(1:9), ]  # Remove first 9 rows
  df <- df[1:(nrow(df) - 2), ]  # Remove last 2 rows
  rownames(df) <- NULL  # Reset row indices
  df <- clean_names(df)  # Clean column names
  
  return(df)
}

# Process each month's folder separately
for (month_folder in month_folders) {
  # Get all Excel files in the month folder
  file_list <- list.files(month_folder, pattern = "*.xlsx", full.names = TRUE)
  
  # Read, clean, and combine all files in the folder
  month_data <- bind_rows(lapply(file_list, clean_data))
  
  # Define output CSV file name based on folder name
  output_csv <- paste0(month_folder, "/Merged_", basename(month_folder), ".csv")
  
  # Save the cleaned data as a CSV
  write.csv(month_data, output_csv, row.names = FALSE)
  
  print(paste("Saved:", output_csv))  # Debug message
}

