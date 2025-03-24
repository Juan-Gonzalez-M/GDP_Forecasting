# utils.R - Utility functions
# This file contains miscellaneous helper functions

#' Check if a package is installed and load it
#'
#' @param package_name Name of the package to check
#'
#' @return Logical indicating whether the package was successfully loaded
#' @export
check_and_load_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    warning("Package '", package_name, "' is not installed. Attempting to install...")
    install.packages(package_name)
    
    if (!requireNamespace(package_name, quietly = TRUE)) {
      warning("Failed to install package '", package_name, "'")
      return(FALSE)
    }
  }
  
  library(package_name, character.only = TRUE)
  return(TRUE)
}

#' Create a cache directory for storing intermediate results
#'
#' @param cache_dir Directory for caching results
#'
#' @return Path to the cache directory
#' @export
create_cache_dir <- function(cache_dir = "cache") {
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
    message("Created cache directory: ", cache_dir)
  }
  return(cache_dir)
}

#' Save an object to the cache
#'
#' @param object Object to save
#' @param file_name File name for the saved object
#' @param cache_dir Cache directory
#'
#' @return Invisibly returns the path to the saved file
#' @export
cache_save <- function(object, file_name, cache_dir = "cache") {
  # Ensure cache directory exists
  create_cache_dir(cache_dir)
  
  # Create full path
  full_path <- file.path(cache_dir, file_name)
  
  # Save object
  saveRDS(object, file = full_path)
  message("Saved object to cache: ", full_path)
  
  return(invisible(full_path))
}

#' Load an object from the cache
#'
#' @param file_name File name of the saved object
#' @param cache_dir Cache directory
#' @param default Default value to return if the file doesn't exist
#'
#' @return The loaded object or the default value
#' @export
cache_load <- function(file_name, cache_dir = "cache", default = NULL) {
  # Create full path
  full_path <- file.path(cache_dir, file_name)
  
  # Check if file exists
  if (!file.exists(full_path)) {
    message("Cache file not found: ", full_path)
    return(default)
  }
  
  # Load object
  object <- readRDS(file = full_path)
  message("Loaded object from cache: ", full_path)
  
  return(object)
}

#' Format time for logging messages
#'
#' @return Character string with formatted time
#' @export
format_time <- function() {
  format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
}

#' Log a message with timestamp
#'
#' @param ... Parts of the message to log
#' @param level Log level (default: "INFO")
#' @param file Optional file to write the log message to
#'
#' @return Invisibly returns the log message
#' @export
log_message <- function(..., level = "INFO", file = NULL) {
  # Create message with timestamp and level
  msg <- paste0(format_time(), " [", level, "] ", paste0(..., collapse = ""))
  
  # Print to console
  message(msg)
  
  # Write to file if specified
  if (!is.null(file)) {
    write(msg, file = file, append = TRUE)
  }
  
  return(invisible(msg))
}

#' Create a function to measure execution time
#'
#' @param expr Expression to evaluate
#' @param name Optional name for the timed operation
#'
#' @return The result of the expression
#' @export
time_operation <- function(expr, name = NULL) {
  # Start timer
  start_time <- Sys.time()
  
  # Evaluate expression
  result <- eval(expr, parent.frame())
  
  # End timer
  end_time <- Sys.time()
  elapsed <- end_time - start_time
  
  # Log time
  if (is.null(name)) {
    name <- deparse(substitute(expr))
  }
  
  log_message("Operation '", name, "' completed in ", 
              format(elapsed, digits = 2), " ", attr(elapsed, "units"))
  
  return(result)
}

#' Get system information for debugging
#'
#' @return List of system information
#' @export
get_system_info <- function() {
  info <- list(
    r_version = R.version.string,
    platform = Sys.info()["sysname"],
    date = Sys.Date(),
    user = Sys.info()["user"],
    packages = as.data.frame(installed.packages()[, c("Package", "Version")]),
    memory = gc()
  )
  
  return(info)
}

#' Print a progress bar in the console
#'
#' @param current Current step
#' @param total Total number of steps
#' @param width Width of the progress bar
#'
#' @return Invisibly returns TRUE
#' @export
progress_bar <- function(current, total, width = 50) {
  # Calculate percentage
  pct <- current / total
  
  # Calculate number of filled positions
  filled <- round(pct * width)
  
  # Create bar
  bar <- paste0(
    "[", 
    paste0(rep("=", filled), collapse = ""),
    paste0(rep(" ", width - filled), collapse = ""),
    "] ",
    sprintf("%3d%%", round(pct * 100))
  )
  
  # Print bar (overwrite previous line)
  cat("\r", bar, " (", current, "/", total, ")", sep = "")
  
  # Add newline if completed
  if (current == total) {
    cat("\n")
  }
  
  return(invisible(TRUE))
}

#' Find the best parameter combination for a model using grid search
#'
#' @param param_grid List of parameters to test
#' @param eval_func Function to evaluate a parameter combination
#' @param minimize Logical indicating whether to minimize or maximize the evaluation metric
#'
#' @return Best parameter combination
#' @export
grid_search <- function(param_grid, eval_func, minimize = TRUE) {
  # Create all combinations of parameters
  param_names <- names(param_grid)
  param_values <- param_grid
  
  # Create a grid of all parameter combinations
  param_combinations <- expand.grid(param_values)
  
  # Initialize best parameters and score
  best_score <- ifelse(minimize, Inf, -Inf)
  best_params <- NULL
  
  # Evaluate each parameter combination
  n_combinations <- nrow(param_combinations)
  log_message("Starting grid search with ", n_combinations, " parameter combinations")
  
  for (i in 1:n_combinations) {
    # Extract current parameter combination
    current_params <- as.list(param_combinations[i, ])
    names(current_params) <- param_names
    
    # Evaluate current parameters
    score <- do.call(eval_func, current_params)
    
    # Update best parameters if better
    if ((minimize && score < best_score) || (!minimize && score > best_score)) {
      best_score <- score
      best_params <- current_params
    }
    
    # Show progress
    progress_bar(i, n_combinations)
  }
  
  log_message("Grid search completed. Best score: ", best_score)
  
  return(list(
    params = best_params,
    score = best_score
  ))
}

#' Parse a date string to a date object, trying multiple formats
#'
#' @param date_str Date string
#' @param formats Vector of date formats to try
#'
#' @return Date object or NULL if parsing fails
#' @export
parse_date <- function(date_str, formats = c("%Y-%m-%d", "%Y/%m/%d", "%d-%m-%Y", "%d/%m/%Y", 
                                             "%Y-Q%q", "%YQ%q", "%Y-M%m", "%YM%m")) {
  # Try each format
  for (fmt in formats) {
    tryCatch({
      # Special handling for quarterly format
      if (grepl("Q", fmt)) {
        # Parse year and quarter
        year <- as.numeric(substr(date_str, 1, 4))
        quarter <- as.numeric(substr(date_str, nchar(date_str), nchar(date_str)))
        return(as.Date(paste0(year, "-", (quarter * 3 - 2), "-01")))
      }
      
      # Special handling for monthly format with M
      if (grepl("M", fmt)) {
        # Parse year and month
        year <- as.numeric(substr(date_str, 1, 4))
        month <- as.numeric(substr(date_str, nchar(date_str), nchar(date_str)))
        return(as.Date(paste0(year, "-", month, "-01")))
      }
      
      # Standard date parsing
      date <- as.Date(date_str, format = fmt)
      if (!is.na(date)) {
        return(date)
      }
    }, error = function(e) {
      # Do nothing, try next format
    })
  }
  
  # If all formats fail, return NULL
  warning("Could not parse date string: ", date_str)
  return(NULL)
}

#' Calculate confidence intervals for a forecast
#'
#' @param forecast_ts Forecast time series
#' @param error_std Standard deviation of forecast errors
#' @param confidence Confidence level (e.g., 0.95 for 95% confidence)
#'
#' @return List with lower and upper bounds as time series
#' @export
calculate_forecast_intervals <- function(forecast_ts, error_std, confidence = 0.95) {
  # Calculate z-score for the confidence level
  z_score <- qnorm((1 + confidence) / 2)
  
  # Calculate margin of error
  margin <- z_score * error_std
  
  # Calculate lower and upper bounds
  lower_bound <- forecast_ts - margin
  upper_bound <- forecast_ts + margin
  
  # Create time series objects for bounds
  lower_ts <- ts(as.numeric(lower_bound), 
                 start = start(forecast_ts), 
                 frequency = frequency(forecast_ts))
  
  upper_ts <- ts(as.numeric(upper_bound), 
                 start = start(forecast_ts), 
                 frequency = frequency(forecast_ts))
  
  return(list(
    lower = lower_ts,
    upper = upper_ts
  ))
}

#' Convert a univariate time series to a supervised learning problem
#'
#' @param ts_obj Time series object
#' @param lag_order Number of lags to include
#' @param horizon Forecast horizon
#'
#' @return Data frame with lagged values and target
#' @export
create_supervised_dataset <- function(ts_obj, lag_order = 4, horizon = 1) {
  # Convert time series to numeric vector
  x <- as.numeric(ts_obj)
  n <- length(x)
  
  # Check if enough data
  if (n <= lag_order + horizon) {
    stop("Not enough data for the specified lag order and horizon")
  }
  
  # Create matrix of lagged values
  X <- matrix(NA, nrow = n - lag_order - horizon + 1, ncol = lag_order)
  y <- numeric(n - lag_order - horizon + 1)
  
  for (i in 1:(n - lag_order - horizon + 1)) {
    # Add lagged values
    X[i, ] <- x[i:(i + lag_order - 1)]
    
    # Add target value
    y[i] <- x[i + lag_order + horizon - 1]
  }
  
  # Create data frame
  colnames(X) <- paste0("lag", 1:lag_order)
  df <- data.frame(X)
  df$target <- y
  
  return(df)
}

#' Create a simple forecast from historical mean and standard deviation
#'
#' @param ts_obj Time series object
#' @param horizon Forecast horizon
#' @param confidence Confidence level for intervals
#'
#' @return List with mean forecast and confidence intervals
#' @export
naive_forecast <- function(ts_obj, horizon = FORECAST_HORIZON, confidence = 0.95) {
  # Calculate mean and standard deviation
  mean_value <- mean(ts_obj, na.rm = TRUE)
  sd_value <- sd(ts_obj, na.rm = TRUE)
  
  # Create forecast time series
  last_time <- time(ts_obj)[length(ts_obj)]
  freq <- frequency(ts_obj)
  
  # Calculate next time period
  if (freq == 4) {
    # Quarterly data
    year <- floor(last_time)
    quarter <- round((last_time - year) * 4) + 1
    
    # Move to next quarter
    quarter <- quarter + 1
    if (quarter > 4) {
      year <- year + 1
      quarter <- 1
    }
    
    # Create start time for forecast
    forecast_start <- c(year, quarter)
  } else if (freq == 12) {
    # Monthly data
    year <- floor(last_time)
    month <- round((last_time - year) * 12) + 1
    
    # Move to next month
    month <- month + 1
    if (month > 12) {
      year <- year + 1
      month <- 1
    }
    
    # Create start time for forecast
    forecast_start <- c(year, month)
  } else {
    # Other frequencies
    forecast_start <- last_time + 1/freq
  }
  
  # Create forecast time series
  forecast_ts <- ts(rep(mean_value, horizon), 
                    start = forecast_start, 
                    frequency = freq)
  
  # Calculate confidence intervals
  intervals <- calculate_forecast_intervals(forecast_ts, sd_value, confidence)
  
  return(list(
    mean = forecast_ts,
    lower = intervals$lower,
    upper = intervals$upper
  ))
}

#' Convert between different time frequencies (e.g., monthly to quarterly)
#'
#' @param ts_obj Time series object
#' @param target_frequency Target frequency (e.g., 4 for quarterly, 12 for monthly)
#' @param method Aggregation/disaggregation method
#'
#' @return Time series with the target frequency
#' @export
convert_frequency <- function(ts_obj, target_frequency, method = "mean") {
  # Get current frequency
  current_frequency <- frequency(ts_obj)
  
  # Check if conversion is needed
  if (current_frequency == target_frequency) {
    return(ts_obj)
  }
  
  # Determine if we're aggregating or disaggregating
  if (current_frequency > target_frequency) {
    # Aggregating (e.g., monthly to quarterly)
    if (method == "mean") {
      # Use stats::aggregate.ts
      result <- stats::aggregate(ts_obj, nfrequency = target_frequency, FUN = mean)
    } else if (method == "sum") {
      result <- stats::aggregate(ts_obj, nfrequency = target_frequency, FUN = sum)
    } else if (method == "first") {
      result <- stats::aggregate(ts_obj, nfrequency = target_frequency, FUN = function(x) x[1])
    } else if (method == "last") {
      result <- stats::aggregate(ts_obj, nfrequency = target_frequency, FUN = function(x) x[length(x)])
    } else {
      stop("Unknown aggregation method: ", method)
    }
  } else {
    # Disaggregating (e.g., quarterly to monthly)
    # This is more complex and less accurate
    if (method == "linear") {
      # Linear interpolation
      # Convert to ts with target frequency, filled with NAs
      time_points <- time(ts_obj)
      start_year <- floor(time_points[1])
      start_period <- round((time_points[1] - start_year) * current_frequency) + 1
      
      # Calculate start period for target frequency
      target_start_period <- (start_period - 1) * (target_frequency / current_frequency) + 1
      target_start <- c(start_year, target_period)
      
      # Create empty ts with target frequency
      n_periods <- length(ts_obj) * (target_frequency / current_frequency)
      result <- ts(rep(NA, n_periods), start = target_start, frequency = target_frequency)
      
      # Fill in known values
      for (i in 1:length(ts_obj)) {
        time_point <- time(ts_obj)[i]
        year <- floor(time_point)
        period <- round((time_point - year) * current_frequency) + 1
        
        # Calculate target period
        target_period <- (period - 1) * (target_frequency / current_frequency) + 1
        
        # Set value
        result[target_period] <- ts_obj[i]
      }
      
      # Interpolate missing values
      result <- zoo::na.approx(result)
    } else {
      stop("Unknown disaggregation method: ", method)
    }
  }
  
  return(result)
}

#' Find optimal ARIMA parameters for a time series
#'
#' @param ts_obj Time series object
#' @param max_p Maximum AR order
#' @param max_d Maximum differencing order
#' @param max_q Maximum MA order
#' @param max_P Maximum seasonal AR order
#' @param max_D Maximum seasonal differencing order
#' @param max_Q Maximum seasonal MA order
#' @param seasonal Logical indicating whether to include seasonal components
#' @param ic Information criterion to use
#'
#' @return List with optimal parameters
#' @export
find_optimal_arima <- function(ts_obj, max_p = 5, max_d = 2, max_q = 5,
                               max_P = 2, max_D = 1, max_Q = 2,
                               seasonal = TRUE, ic = "aic") {
  # Use auto.arima from forecast package
  model <- forecast::auto.arima(
    ts_obj,
    max.p = max_p,
    max.d = max_d,
    max.q = max_q,
    max.P = max_P,
    max.D = max_D,
    max.Q = max_Q,
    seasonal = seasonal,
    ic = ic,
    stepwise = TRUE,
    approximation = (length(ts_obj) > 100)
  )
  
  # Extract parameters
  params <- list(
    p = model$arma[1],
    d = model$arma[6],
    q = model$arma[2],
    P = model$arma[3],
    D = model$arma[7],
    Q = model$arma[4],
    order = c(model$arma[1], model$arma[6], model$arma[2]),
    seasonal = c(model$arma[3], model$arma[7], model$arma[4])
  )
  
  return(params)
}

#' Debug date column issues
#'
#' @param df Data frame containing a date column
#' @param date_col_index Index or name of the date column
#'
#' @return Invisibly returns NULL
#' @export
debug_date_column <- function(df, date_col_index = 1) {
  # Get the date column
  if (is.character(date_col_index)) {
    date_col <- df[[date_col_index]]
    col_name <- date_col_index
  } else {
    date_col <- df[[date_col_index]]
    col_name <- names(df)[date_col_index]
  }
  
  # Print summary information
  cat("=== Date Column Debug Information ===\n")
  cat("Column name:", col_name, "\n")
  cat("Class:", class(date_col), "\n")
  cat("First 10 values:", paste(head(date_col, 10), collapse = ", "), "\n")
  
  # If character or factor, show unique patterns
  if (is.character(date_col) || is.factor(date_col)) {
    date_col <- as.character(date_col)
    
    # Check for common patterns
    patterns <- list(
      "YYYY-MM-DD" = "^\\d{4}-\\d{2}-\\d{2}$",
      "YYYY/MM/DD" = "^\\d{4}/\\d{2}/\\d{2}$",
      "DD-MM-YYYY" = "^\\d{2}-\\d{2}-\\d{4}$",
      "DD/MM/YYYY" = "^\\d{2}/\\d{2}/\\d{4}$",
      "YYYY-MM" = "^\\d{4}-\\d{2}$",
      "YYYY/MM" = "^\\d{4}/\\d{2}$",
      "YYYY-Q#" = "^\\d{4}-[Qq]\\d$",
      "YYYY Q#" = "^\\d{4}\\s+[Qq]\\d$",
      "YYYY" = "^\\d{4}$"
    )
    
    cat("Pattern detection:\n")
    for (name in names(patterns)) {
      pattern <- patterns[[name]]
      matches <- sum(grepl(pattern, date_col))
      cat("  ", name, ":", matches, "of", length(date_col), "match\n")
    }
    
    # Try parsing as Date
    tryCatch({
      parsed_dates <- as.Date(date_col)
      cat("Parsed as Date:", sum(!is.na(parsed_dates)), "of", length(date_col), "valid\n")
    }, error = function(e) {
      cat("Could not parse as Date:", e$message, "\n")
    })
  }
  
  cat("=====================================\n")
  
  # Return invisibly
  invisible(NULL)
}

#' Calculate year-over-year growth rates for combined historical and forecast series
#'
#' This function combines historical and forecast time series and calculates
#' year-over-year growth rates. It handles both log-transformed and level data.
#' The result separates historical, forecast, and combined growth rates for
#' flexible visualization.
#'
#' @param historical_ts Time series object containing historical data
#' @param forecast_ts Time series object containing forecast data
#' @param log_transform Logical indicating whether the series are in log scale
#'
#' @return A list containing historical growth, forecast growth, and combined growth as time series
#' @export
calculate_combined_yoy_growth <- function(historical_ts, forecast_ts, log_transform = FALSE) {
  # Ensure same frequency
  if (frequency(historical_ts) != frequency(forecast_ts)) {
    stop("Historical and forecast series must have the same frequency")
  }
  
  # Combine the series
  combined_ts <- ts(
    c(as.vector(historical_ts), as.vector(forecast_ts)),
    start = start(historical_ts),
    frequency = frequency(historical_ts)
  )
  
  # Calculate YoY growth on combined series
  freq <- frequency(combined_ts)
  
  if (log_transform) {
    yoy_growth <- 100 * diff(combined_ts, lag = freq)
  } else {
    lagged <- stats::lag(combined_ts, -freq)
    yoy_growth <- 100 * (combined_ts / lagged - 1)
  }
  
  # Mark which parts are historical vs forecast
  n_hist <- length(historical_ts)
  n_hist_yoy <- n_hist - freq  # YoY reduces length by frequency
  
  # Split into historical and forecast portions
  hist_indices <- 1:min(n_hist_yoy, length(yoy_growth))
  if (length(hist_indices) > 0) {
    historical_growth <- window(yoy_growth, end = time(yoy_growth)[max(hist_indices)])
    forecast_growth <- window(yoy_growth, start = time(yoy_growth)[max(hist_indices) + 1])
  } else {
    historical_growth <- NULL
    forecast_growth <- yoy_growth
  }
  
  return(list(
    historical = historical_growth,
    forecast = forecast_growth,
    combined = yoy_growth
  ))
}
