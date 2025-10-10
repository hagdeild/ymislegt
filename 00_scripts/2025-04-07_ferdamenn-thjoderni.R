# Load required libraries
library(readxl)
library(tidyverse)

# Function to process each year block in the data
process_excel_data <- function(file_path, sheet_name = 2) {
  # Read the Excel file
  raw_data <- read_excel(file_path, sheet = sheet_name)
  
  # Initialize empty list to store each year's data frame
  all_years_data <- list()
  
  # Get column names (first row might be the title, so we'll examine data)
  col_names <- names(raw_data)
  
  # Initialize variables to track where each year block starts and ends
  year_start_indices <- c()
  year_end_indices <- c()
  
  # Find the indices where each year block starts and ends
  for (i in 1:nrow(raw_data)) {
    if (!is.na(raw_data[i, 1]) && grepl("^\\d{4}$", as.character(raw_data[i, 1]))) {
      year_start_indices <- c(year_start_indices, i)
      
      # If we found a new year start and already have a previous year,
      # mark the end of the previous year
      if (length(year_start_indices) > 1) {
        year_end_indices <- c(year_end_indices, i - 1)
      }
    }
  }
  # Add the end index for the last year
  year_end_indices <- c(year_end_indices, nrow(raw_data))
  
  # Process each year block
  for (i in 1:length(year_start_indices)) {
    start_idx <- year_start_indices[i]
    end_idx <- year_end_indices[i]
    
    # Extract this year's data
    year_data <- raw_data[start_idx:end_idx, ]
    
    # Get the year from the first row, first column
    current_year <- as.character(year_data[1, 1])
    
    # Get the month names from the first row
    month_names <- as.character(unlist(year_data[1, 2:13]))
    
    # Extract country data (exclude the first row with month names and the last row with totals)
    country_data <- year_data[2:(nrow(year_data)-1), ]
    
    # Remove any rows that might be NA (e.g., blank rows between year blocks)
    country_data <- country_data[!is.na(country_data[[1]]), ]
    
    # Remove 'Samtals' row if it exists
    country_data <- country_data[country_data[[1]] != "Samtals", ]
    
    # Create a data frame for this year
    year_df <- data.frame(
      country = character(),
      date = character(),
      value = numeric(),
      stringsAsFactors = FALSE
    )
    
    # For each country
    for (j in 1:nrow(country_data)) {
      country_name <- as.character(country_data[j, 1])
      
      # For each month (skip the 'Alls' column)
      for (k in 1:12) {
        # Check if we have data for this month
        if (k <= length(month_names) && !is.na(country_data[j, k+1])) {
          # Create date string (year-month)
          date_str <- paste(current_year, month_names[k], sep = "-")
          
          # Extract the value, removing any thousand separators
          value_str <- as.character(country_data[j, k+1])
          value_str <- gsub("\\.", "", value_str)  # Remove thousand separators if present
          value <- as.numeric(value_str)
          
          # Add to the data frame
          year_df <- rbind(year_df, data.frame(
            country = country_name,
            date = date_str,
            value = value,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    # Add this year's data to the list
    all_years_data[[i]] <- year_df
  }
  
  # Combine all years into a single data frame
  final_data <- do.call(rbind, all_years_data)
  
  # Convert date string to proper date format (first day of the month)
  month_map <- c(
    "Jan" = "01", "Feb" = "02", "Mar" = "03", "Apr" = "04", 
    "Maí" = "05", "Jún" = "06", "Júl" = "07", "Ágú" = "08", 
    "Sep" = "09", "Okt" = "10", "Nóv" = "11", "Des" = "12"
  )
  
  # Split date into year and month
  final_data <- final_data %>%
    separate(date, into = c("year", "month"), sep = "-") %>%
    mutate(
      month_num = month_map[month],
      date = paste(year, month_num, "01", sep = "-"),
      date = as.Date(date)
    ) %>%
    select(date, country, value)
  
  return(final_data)
}



# Load the function defined in the previous code snippet

# Assuming your file is named "tourist_departures.xlsx"
file_path <- "00_data/2022-2025-brottfarir-erlendra-farthega-feb.xlsx"

# Process the data
tourist_data <- process_excel_data(file_path)


tourist_data %>% 
  as_tibble() %>%
  arrange(date, country) %>% 
  group_by(country) %>% 
  mutate(diff = value / lag(value, 12) - 1) %>% 
  ungroup() %>% 
  drop_na() %>% 
  filter(date >= "2023-01-01") %>% 
  ggplot(aes(date, diff, col = country)) + 
  geom_line()
