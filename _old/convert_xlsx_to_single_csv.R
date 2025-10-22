
###########For DHT only#############
# Load required libraries
library(readxl)
library(dplyr)
library(janitor)

# Define the file path
file_path <- "/Users/estellepfitzer/Desktop/PB_Downloads/00_DigitalHealth/Apr2025/PitchBook_All_Columns_2025_04_02_15_15_08.xlsx"

# Function to clean and process the file
clean_data <- function(file_path) {
  df <- read_excel(file_path, col_names = FALSE)  # Read without headers
  
  colnames(df) <- df[7, ]  # Set row 9 as column names
  df <- df[-(1:7), ]  # Remove first 9 rows
  df <- clean_names(df)  # Clean column names
  
  return(df)
}

# Process the file
df_cleaned <- clean_data(file_path)

# Define output CSV file path
output_csv <- "/Users/estellepfitzer/Desktop/PB_Downloads/EP_PitchBook_Cleaned_2025_03_12.csv"

# Save the cleaned data to CSV
write.csv(df_cleaned, output_csv, row.names = FALSE)

# Print confirmation message
print(paste("Cleaned dataset saved at:", output_csv))

