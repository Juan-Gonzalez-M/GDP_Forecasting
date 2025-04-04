# data_import.R - Functions for importing and preparing data
# This file contains functions to import and preprocess data from the Excel file

#' Import data from Excel file
#'
#' @param file_path Path to the Excel file
#' @param quarterly_sheet Sheet number for quarterly data
#' @param monthly_sheet Sheet number for monthly data
#' @param aggregation_sheet Sheet number for data aggregation scheme
#' @param auxiliary_sheet Sheet number for auxiliary quarterly data
#'
#' @return A list containing imported data frames
#' @export
import_data <- function(file_path = DATA_FILE, 
                        quarterly_sheet = QUARTERLY_SHEET,
                        monthly_sheet = MONTHLY_SHEET,
                        aggregation_sheet = AGGREGATION_SHEET,
                        auxiliary_sheet = AUXILIARY_SHEET) {
  
  message("Importing data from ", file_path)
  
  # Check if file exists
  if (!file.exists(file_path)) {
    stop("Data file not found: ", file_path)
  }
  
  # Import quarterly data
  tryCatch({
    quarterly_data <- readxl::read_excel(file_path, sheet = quarterly_sheet)
    message("Imported quarterly data: ", nrow(quarterly_data), " rows, ", 
            ncol(quarterly_data), " columns")
  }, error = function(e) {
    stop("Error importing quarterly data: ", e$message)
  })
  
  # Import monthly data
  tryCatch({
    monthly_data <- readxl::read_excel(file_path, sheet = monthly_sheet)
    message("Imported monthly data: ", nrow(monthly_data), " rows, ", 
            ncol(monthly_data), " columns")
  }, error = function(e) {
    stop("Error importing monthly data: ", e$message)
  })
  
  # Import data aggregation scheme
  tryCatch({
    aggregation_scheme <- readxl::read_excel(file_path, sheet = aggregation_sheet)
    message("Imported aggregation scheme: ", nrow(aggregation_scheme), " rows, ", 
            ncol(aggregation_scheme), " columns")
  }, error = function(e) {
    stop("Error importing aggregation scheme: ", e$message)
  })
  
  # Import auxiliary quarterly data
  tryCatch({
    auxiliary_data <- readxl::read_excel(file_path, sheet = auxiliary_sheet)
    message("Imported auxiliary data: ", nrow(auxiliary_data), " rows, ", 
            ncol(auxiliary_data), " columns")
  }, error = function(e) {
    stop("Error importing auxiliary data: ", e$message)
  })
  
  # Return a list containing all data frames
  return(list(
    quarterly = quarterly_data,
    monthly = monthly_data,
    aggregation = aggregation_scheme,
    auxiliary = auxiliary_data
  ))
}

#' Prepare time series data from data frames
#'
#' @param data_list List containing data frames as returned by import_data()
#'
#' @return A list containing prepared time series objects
#' @export
prepare_time_series <- function(data_list) {
  message("Preparing time series data...")
  
  # Prepare quarterly time series
  quarterly_ts <- prepare_quarterly_ts(data_list$quarterly)
  
  # Prepare monthly time series
  monthly_ts <- prepare_monthly_ts(data_list$monthly)
  
  # Return a list containing all time series objects
  return(list(
    quarterly = quarterly_ts,
    monthly = monthly_ts
  ))
}

#' Prepare quarterly time series from data frame
#'
#' @param quarterly_df Data frame containing quarterly data
#' @return A list of quarterly time series objects
#' @importFrom lubridate year quarter
#' @export
prepare_quarterly_ts <- function(quarterly_df) {
  # Create a list to store time series objects
  ts_list <- list()
  
  # Assuming first column is date or time period
  date_col <- names(quarterly_df)[1]
  series_cols <- names(quarterly_df)[-1]
  
  # Parse dates if necessary
  dates <- quarterly_df[[date_col]]
  start_year <- NULL
  start_quarter <- NULL
  success <- FALSE
  
  # Print out the first few date values for debugging
  message("First few date values: ", paste(head(dates), collapse=", "))
  
  if (is.character(dates) || is.factor(dates)) {
    # Convert factor to character
    dates <- as.character(dates)
    
    # Try YYYY-Qq format (e.g., "2010-Q1", "2010Q1", "2010 Q1")
    pattern1 <- "^(\\d{4})[-\\s]?[Qq](\\d)$"
    if (all(grepl(pattern1, dates))) {
      years <- as.numeric(sub(pattern1, "\\1", dates))
      quarters <- as.numeric(sub(pattern1, "\\2", dates))
      start_year <- min(years)
      start_quarter <- min(quarters[years == start_year])
      message("Parsed dates as YYYY-Qq format. Start: ", start_year, "Q", start_quarter)
      success <- TRUE
    }
    
    # Try "YYYY-MM-DD" format
    if (!success) {
      tryCatch({
        parsed_dates <- as.Date(dates, format = "%Y-%m-%d")
        if (!all(is.na(parsed_dates))) {
          start_year <- lubridate::year(min(parsed_dates, na.rm = TRUE))
          start_quarter <- ceiling(lubridate::month(min(parsed_dates, na.rm = TRUE)) / 3)
          message("Parsed dates as YYYY-MM-DD format. Start: ", start_year, "Q", start_quarter)
          success <- TRUE
        }
      }, error = function(e) {
        # Continue to next format
      })
    }
    
    # Try "mmm-yy" format (e.g., "Mar-14")
    if (!success) {
      pattern_mmm_yy <- "^([A-Za-z]{3})-([0-9]{2})$"
      if (all(grepl(pattern_mmm_yy, dates))) {
        # Extract month and year
        months <- tolower(sub(pattern_mmm_yy, "\\1", dates))
        years <- as.numeric(sub(pattern_mmm_yy, "\\2", dates))
        
        # Map month abbreviations to numbers
        month_map <- c(
          "jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4, "may" = 5, "jun" = 6,
          "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dec" = 12
        )
        
        # Convert to numeric months
        month_nums <- sapply(tolower(months), function(m) month_map[m])
        
        # Add 2000 to 2-digit years (adjust if your dates are from 1900s)
        years <- ifelse(years < 50, 2000 + years, 1900 + years)
        
        # Get quarters from months
        quarters <- ceiling(month_nums / 3)
        
        # Find minimum date
        min_idx <- which.min(years * 10 + quarters)
        start_year <- years[min_idx]
        start_quarter <- quarters[min_idx]
        
        message("Parsed dates as mmm-yy format. Start: ", start_year, "Q", start_quarter)
        success <- TRUE
      }
    }
    
    # Try "YYYY-MM" format
    if (!success) {
      pattern2 <- "^(\\d{4})[-/\\s](\\d{1,2})$"
      if (all(grepl(pattern2, dates))) {
        years <- as.numeric(sub(pattern2, "\\1", dates))
        months <- as.numeric(sub(pattern2, "\\2", dates))
        start_year <- min(years)
        start_month <- min(months[years == start_year])
        start_quarter <- ceiling(start_month / 3)
        message("Parsed dates as YYYY-MM format. Start: ", start_year, "Q", start_quarter)
        success <- TRUE
      }
    }
    
    # If dates are just years (YYYY)
    if (!success) {
      pattern3 <- "^(\\d{4})$"
      if (all(grepl(pattern3, dates))) {
        years <- as.numeric(dates)
        start_year <- min(years)
        start_quarter <- 1
        message("Parsed dates as YYYY format. Start: ", start_year, "Q", start_quarter)
        success <- TRUE
      }
    }
    
    # If no pattern matches, try to extract numbers as years
    if (!success) {
      tryCatch({
        # Try to extract any 4-digit number as a year
        year_pattern <- "\\b(\\d{4})\\b"
        years <- as.numeric(regmatches(dates, regexec(year_pattern, dates)))
        if (length(years) > 0 && !all(is.na(years))) {
          start_year <- min(years, na.rm = TRUE)
          start_quarter <- 1
          message("Extracted years from dates. Start: ", start_year, "Q", start_quarter)
          success <- TRUE
        }
      }, error = function(e) {
        # Continue to default
      })
    }
  } else if (inherits(dates, "Date")) {
    
    # If dates are already Date objects
    start_year <- lubridate::year(min(dates, na.rm = TRUE))
    start_quarter <- ceiling(lubridate::month(min(dates, na.rm = TRUE)) / 3)
    message("Using existing Date objects. Start: ", start_year, "Q", start_quarter)
    success <- TRUE
  } else if (is.numeric(dates)) {
    
    # If dates are numeric, try to interpret as years
    start_year <- min(dates, na.rm = TRUE)
    start_quarter <- 1
    message("Interpreting numeric values as years. Start: ", start_year, "Q", start_quarter)
    success <- TRUE
  }
  
  # Default case - use current year if all else fails
  if (!success || is.null(start_year) || is.null(start_quarter)) {
    year_now <- as.numeric(format(Sys.Date(), "%Y"))
    warning("Could not parse dates. Using current year: ", 
            year_now, "Q1 instead of defaulting to 2000Q1")
    start_year <- year_now
    start_quarter <- 1
    
    # Try to infer from data where possible
    if (is.numeric(dates) && length(dates) > 0) {
      numeric_dates <- as.numeric(dates)
      min_date <- min(numeric_dates[!is.na(numeric_dates)], na.rm = TRUE)
      if (!is.na(min_date) && min_date > 1900 && min_date < 2100) {
        start_year <- floor(min_date)
        message("Inferred start_year from numeric data: ", start_year)
      }
    }
  }
  
  # Handle POSIXct/POSIXlt dates explicitly
  if (!success && (inherits(dates, "POSIXct") || inherits(dates, "POSIXlt"))) {
    # For POSIXct/POSIXlt dates
    date_min <- min(dates, na.rm = TRUE)
    start_year <- as.numeric(format(date_min, "%Y"))
    
    # For quarterly data, extract the quarter
    start_quarter <- ceiling(as.numeric(format(date_min, "%m")) / 3)
    # Or for monthly data, extract the month
    start_month <- as.numeric(format(date_min, "%m"))
    
    message("Using POSIXct dates. Start: ", start_year, 
            ifelse(exists("start_quarter"), paste0("Q", start_quarter), 
                   paste0("-", start_month)))
    success <- TRUE
  }
  
  # Create time series objects for each series
  for (col in series_cols) {
    # Extract values
    values <- quarterly_df[[col]]
    
    # Check for missing values
    if (any(is.na(values))) {
      warning("Missing values found in series '", col, "'. These will be interpolated.")
      # Simple linear interpolation
      values <- zoo::na.approx(values, na.rm = FALSE)
      # If there are still NA values at the beginning or end, replace with nearby values
      values <- zoo::na.locf(values, fromLast = TRUE)
      values <- zoo::na.locf(values)
    }
    
    # Create a quarterly time series object
    ts_obj <- ts(values, start = c(start_year, start_quarter), frequency = 4)
    
    # Store the time series object in the list
    ts_list[[col]] <- ts_obj
  }
  
  message("Created ", length(ts_list), " quarterly time series.")
  return(ts_list)
}

#' Prepare monthly time series from data frame
#'
#' @param monthly_df Data frame containing monthly data
#' @return A list of monthly time series objects
#' @importFrom lubridate year month
#' @export
prepare_monthly_ts <- function(monthly_df) {
  # Create a list to store time series objects
  ts_list <- list()
  
  # Assuming first column is date or time period
  date_col <- names(monthly_df)[1]
  series_cols <- names(monthly_df)[-1]
  
  # Parse dates if necessary
  dates <- monthly_df[[date_col]]
  start_year <- NULL
  start_month <- NULL
  success <- FALSE
  
  # Print out the first few date values for debugging
  message("First few date values: ", paste(head(dates), collapse=", "))
  
  if (is.character(dates) || is.factor(dates)) {
    # Convert factor to character
    dates <- as.character(dates)
    
    # Try "YYYY-MM" format (e.g., "2010-01", "2010/01", "2010 01")
    pattern1 <- "^(\\d{4})[-/\\s](\\d{1,2})$"
    if (all(grepl(pattern1, dates))) {
      years <- as.numeric(sub(pattern1, "\\1", dates))
      months <- as.numeric(sub(pattern1, "\\2", dates))
      start_year <- min(years)
      start_month <- min(months[years == start_year])
      message("Parsed dates as YYYY-MM format. Start: ", start_year, "-", start_month)
      success <- TRUE
    }
    
    # Try "YYYY-MM-DD" format
    if (!success) {
      tryCatch({
        parsed_dates <- as.Date(dates, format = "%Y-%m-%d")
        if (!all(is.na(parsed_dates))) {
          start_year <- lubridate::year(min(parsed_dates, na.rm = TRUE))
          start_month <- lubridate::month(min(parsed_dates, na.rm = TRUE))
          message("Parsed dates as YYYY-MM-DD format. Start: ", start_year, "-", start_month)
          success <- TRUE
        }
      }, error = function(e) {
        # Continue to next format
      })
    }
    
    # Try "mmm-yy" format (e.g., "Mar-14")
    if (!success) {
      pattern_mmm_yy <- "^([A-Za-z]{3})-([0-9]{2})$"
      if (all(grepl(pattern_mmm_yy, dates))) {
        # Extract month and year
        months <- tolower(sub(pattern_mmm_yy, "\\1", dates))
        years <- as.numeric(sub(pattern_mmm_yy, "\\2", dates))
        
        # Map month abbreviations to numbers
        month_map <- c(
          "jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4, "may" = 5, "jun" = 6,
          "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dec" = 12
        )
        
        # Convert to numeric months
        month_nums <- sapply(tolower(months), function(m) month_map[m])
        
        # Add 2000 to 2-digit years (adjust if your dates are from 1900s)
        years <- ifelse(years < 50, 2000 + years, 1900 + years)
        
        # Find minimum date
        min_idx <- which.min(years * 100 + month_nums)
        start_year <- years[min_idx]
        start_month <- month_nums[min_idx]
        
        message("Parsed dates as mmm-yy format. Start: ", start_year, "-", start_month)
        success <- TRUE
      }
    }
    
    # Try "MMM YYYY" format (e.g., "Jan 2010")
    if (!success) {
      tryCatch({
        parsed_dates <- as.Date(paste("01", dates), format = "%d %b %Y")
        if (!all(is.na(parsed_dates))) {
          start_year <- lubridate::year(min(parsed_dates, na.rm = TRUE))
          start_month <- lubridate::month(min(parsed_dates, na.rm = TRUE))
          message("Parsed dates as MMM YYYY format. Start: ", start_year, "-", start_month)
          success <- TRUE
        }
      }, error = function(e) {
        # Continue to next format
      })
    }
    
    # Try "YYYY-Mmm" format (e.g., "2010-M01", "2010M01")
    if (!success) {
      pattern2 <- "^(\\d{4})[-\\s]?[Mm](\\d{1,2})$"
      if (all(grepl(pattern2, dates))) {
        years <- as.numeric(sub(pattern2, "\\1", dates))
        months <- as.numeric(sub(pattern2, "\\2", dates))
        start_year <- min(years)
        start_month <- min(months[years == start_year])
        message("Parsed dates as YYYY-Mmm format. Start: ", start_year, "-", start_month)
        success <- TRUE
      }
    }
  } else if (inherits(dates, "Date")) {
    # If dates are already Date objects
    start_year <- lubridate::year(min(dates, na.rm = TRUE))
    start_month <- lubridate::month(min(dates, na.rm = TRUE))
    message("Using existing Date objects. Start: ", start_year, "-", start_month)
    success <- TRUE
  }
  
  # Handle POSIXct/POSIXlt dates explicitly
  if (!success && (inherits(dates, "POSIXct") || inherits(dates, "POSIXlt"))) {
    # For POSIXct/POSIXlt dates
    date_min <- min(dates, na.rm = TRUE)
    start_year <- as.numeric(format(date_min, "%Y"))
    
    # For quarterly data, extract the quarter
    start_quarter <- ceiling(as.numeric(format(date_min, "%m")) / 3)
    # Or for monthly data, extract the month
    start_month <- as.numeric(format(date_min, "%m"))
    
    message("Using POSIXct dates. Start: ", start_year, 
            ifelse(exists("start_quarter"), paste0("Q", start_quarter), 
                   paste0("-", start_month)))
    success <- TRUE
  }
  
  # Default case - use current year if all else fails
  if (!success || is.null(start_year) || is.null(start_quarter)) {
    year_now <- as.numeric(format(Sys.Date(), "%Y"))
    warning("Could not parse dates. Using first quarter of current year: ", 
            year_now, "Q1")
    start_year <- year_now
    start_quarter <- 1
    
    # Try to infer from data where possible
    if (is.numeric(dates) && length(dates) > 0) {
      min_date <- min(dates, na.rm = TRUE)
      if (min_date > 1900 && min_date < 2100) {
        start_year <- floor(min_date)
        message("Inferred start_year from numeric data: ", start_year)
      }
    }
  }
  
  # Create time series objects for each series
  for (col in series_cols) {
    # Extract values
    values <- monthly_df[[col]]
    
    # Check for missing values
    if (any(is.na(values))) {
      warning("Missing values found in series '", col, "'. These will be interpolated.")
      # Simple linear interpolation
      values <- zoo::na.approx(values, na.rm = FALSE)
      # If there are still NA values at the beginning or end, replace with nearby values
      values <- zoo::na.locf(values, fromLast = TRUE)
      values <- zoo::na.locf(values)
    }
    
    # Create a monthly time series object
    ts_obj <- ts(values, start = c(start_year, start_month), frequency = 12)
    
    # Store the time series object in the list
    ts_list[[col]] <- ts_obj
  }
  
  message("Created ", length(ts_list), " monthly time series.")
  return(ts_list)
}