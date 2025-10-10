library(fs)
library(lubridate)

# Paths to the directories
paths <- c("D:/GÖGN LS/", "D:/Leiðsögn/")

# Helper function to safely list files
safe_dir_ls <- function(path, recurse = TRUE) {
  tryCatch(
    dir_ls(path, recurse = recurse),
    error = function(e) {
      warning(sprintf("Skipping directory '%s': %s", path, e$message))
      return(character(0))
    }
  )
}

# List all files in the specified directories
all_files <- unlist(lapply(paths, safe_dir_ls, recurse = TRUE))

# Filter only Excel files
excel_files <- all_files[grepl("\\.xlsx$|\\.xls$", all_files)]

# Get file information
file_info <- file_info(excel_files)

# Filter files modified in 2024
files_modified_2024 <- file_info[file_info$modification_time >= ymd("2024-01-01") & file_info$modification_time < ymd("2025-01-01"),]

# Display the filtered files
View(files_modified_2024)



search_files <- function(paths, pattern = NULL, year = NULL) {
  # List all files in the specified directories
  all_files <- unlist(lapply(paths, safe_dir_ls, recurse = TRUE))
  
  # Filter files based on the name pattern if provided
  if (!is.null(pattern)) {
    all_files <- all_files[grepl(pattern, all_files, ignore.case = TRUE)]
  }
  
  # Get file information
  file_info <- file_info(all_files)
  
  # Filter files based on the modification year if provided
  if (!is.null(year)) {
    start_date <- ymd(paste0(year, "-01-01"))
    end_date <- ymd(paste0(year + 1, "-01-01"))
    file_info <- file_info[file_info$modification_time >= start_date & file_info$modification_time < end_date,]
  }
  
  return(file_info)
}


year <- 2024
pattern <- "laun"

files_found <- search_files(paths, pattern, year)

View(files_found)
