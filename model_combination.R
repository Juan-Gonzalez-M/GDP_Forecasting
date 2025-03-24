# model_combination.R - Functions for combining forecasts
# This file contains functions to combine forecasts from different methodologies

#' Calculate inverse MSE weights for model combination
#'
#' @param error_metrics List of error metrics
#' @param variable_name Name of the variable for which to calculate weights
#' @param horizon Forecast horizon for which to calculate weights
#'
#' @return Named vector of weights
#' @export
calculate_weights <- function(error_metrics, variable_name = NULL, horizon = 1) {
  # Check structure of error_metrics to determine if it's univariate or multivariate
  is_multivariate <- FALSE
  if (!is.null(names(error_metrics)) && length(error_metrics) > 0) {
    first_model <- error_metrics[[1]]
    if (!is.null(names(first_model)) && length(first_model) > 0) {
      if (is.list(first_model[[1]]) && !is.null(names(first_model))) {
        # This appears to be a multivariate structure with variable names
        is_multivariate <- TRUE
      }
    }
  }
  
  # Extract MSE values for the specified horizon
  mse_values <- c()
  
  if (is_multivariate) {
    # Multivariate case (VAR, BVAR, FAVAR)
    if (is.null(variable_name)) {
      stop("variable_name must be provided for multivariate error metrics")
    }
    
    for (model_name in names(error_metrics)) {
      if (variable_name %in% names(error_metrics[[model_name]])) {
        var_metrics <- error_metrics[[model_name]][[variable_name]]
        if (horizon <= length(var_metrics)) {
          mse <- var_metrics[[horizon]]$MSE
          if (!is.null(mse) && !is.na(mse) && mse > 0) {
            mse_values <- c(mse_values, setNames(mse, model_name))
          }
        }
      }
    }
  } else {
    # Univariate case
    for (model_name in names(error_metrics)) {
      if (horizon <= length(error_metrics[[model_name]])) {
        mse <- error_metrics[[model_name]][[horizon]]$MSE
        if (!is.null(mse) && !is.na(mse) && mse > 0) {
          mse_values <- c(mse_values, setNames(mse, model_name))
        }
      }
    }
  }
  
  if (length(mse_values) == 0) {
    warning("No valid MSE values found for horizon ", horizon)
    return(structure(list(), names = character(0)))
  }
  
  # Calculate inverse MSE
  inv_mse <- 1 / mse_values
  
  # Normalize to get weights that sum to 1
  weights <- inv_mse / sum(inv_mse)
  
  return(weights)
}

#' Combine forecasts from multiple models based on inverse MSE weights
#'
#' @param forecasts List of forecast objects or forecast values
#' @param weights Named vector of weights (should sum to 1)
#' @param variable_name Optional variable name for multivariate forecasts
#'
#' @return Combined forecast
#' @export
combine_weighted_forecasts <- function(forecasts, weights, variable_name = NULL) {
  # Check if forecasts list is empty or weights are invalid
  if (length(forecasts) == 0 || is.null(weights) || sum(weights) == 0) {
    stop("Invalid forecasts or weights")
  }
  
  # Keep only forecasts with weights
  model_names <- names(weights)
  if (is.null(model_names)) {
    stop("Weights must be named")
  }
  
  # Determine what type of forecasts we're dealing with
  is_multivariate <- FALSE
  has_fcst <- FALSE
  has_list_structure <- FALSE
  
  # Check the structure of the first forecast if available
  if (length(forecasts) > 0 && length(model_names) > 0 && 
      model_names[1] %in% names(forecasts) && !is.null(forecasts[[model_names[1]]])) {
    
    first_forecast <- forecasts[[model_names[1]]]
    
    # Check if it's a VAR forecast with fcst component
    if (is.list(first_forecast) && !is.null(first_forecast$fcst)) {
      is_multivariate <- TRUE
      has_fcst <- TRUE
      
      # If multivariate, we need a variable name
      if (is.null(variable_name)) {
        # Before stopping, check if we can infer the variable name from the structure
        if (length(names(first_forecast$fcst)) == 1) {
          variable_name <- names(first_forecast$fcst)[1]
          message("Inferred variable_name: ", variable_name)
        } else {
          stop("variable_name must be provided for multivariate forecasts with fcst structure")
        }
      }
    }
    # Check if it's an FAVAR or other list-based forecast structure
    else if (is.list(first_forecast) && 
             !("mean" %in% names(first_forecast)) && 
             !is.null(names(first_forecast)) && 
             length(names(first_forecast)) > 0) {
      is_multivariate <- TRUE
      has_list_structure <- TRUE
      
      # If multivariate list, we need a variable name
      if (is.null(variable_name)) {
        # Before stopping, check if we can infer the variable name from the structure
        if (length(names(first_forecast)) == 1) {
          variable_name <- names(first_forecast)[1]
          message("Inferred variable_name: ", variable_name)
        } else {
          stop("variable_name must be provided for multivariate forecasts with list structure")
        }
      }
    }
  }
  
  # Extract forecast values
  forecast_values <- list()
  for (model_name in model_names) {
    if (model_name %in% names(forecasts)) {
      model_forecast <- forecasts[[model_name]]
      
      # Skip if the model forecast is NULL
      if (is.null(model_forecast)) {
        warning("Forecast for model ", model_name, " is NULL. Skipping.")
        next
      }
      
      if (is_multivariate) {
        if (has_fcst) {
          # VAR forecast structure
          if (!is.null(model_forecast$fcst) && variable_name %in% names(model_forecast$fcst)) {
            forecast_values[[model_name]] <- tryCatch({
              model_forecast$fcst[[variable_name]][, "fcst"]
            }, error = function(e) {
              warning("Error extracting forecast values for VAR model ", model_name, ": ", e$message)
              return(structure(list(), names = character(0)))
            })
          } else {
            warning("Could not extract forecast values for model ", model_name)
          }
        } else if (has_list_structure) {
          # FAVAR or other list-based forecast structure
          if (!is.null(model_forecast[[variable_name]])) {
            forecast_values[[model_name]] <- model_forecast[[variable_name]]
          } else {
            warning("Could not extract forecast values for model ", model_name)
          }
        }
      } else if ("mean" %in% names(model_forecast)) {
        # Univariate forecast object with 'mean' component
        forecast_values[[model_name]] <- as.numeric(model_forecast$mean)
      } else if (is.numeric(model_forecast)) {
        # Direct numeric values
        forecast_values[[model_name]] <- as.numeric(model_forecast)
      } else {
        # Try to extract predict from ARIMA/ETS models directly
        tryCatch({
          # Get h value for prediction (default to 4 quarters)
          h <- 4
          forecast_obj <- forecast::forecast(model_forecast, h = h)
          forecast_values[[model_name]] <- as.numeric(forecast_obj$mean)
        }, error = function(e) {
          warning("Could not generate forecast for model ", model_name, ": ", e$message)
        })
      }
    }
  }
  
  # Remove any NULL values
  forecast_values <- forecast_values[!sapply(forecast_values, is.null)]
  
  if (length(forecast_values) == 0) {
    stop("No valid forecasts to combine")
  }
  
  # Find the minimum length of all forecasts
  min_length <- min(sapply(forecast_values, length))
  
  # Trim all forecasts to this length
  for (model_name in names(forecast_values)) {
    forecast_values[[model_name]] <- forecast_values[[model_name]][1:min_length]
  }
  
  # Initialize combined forecast
  combined_forecast <- rep(0, min_length)
  
  # Add weighted forecasts
  for (model_name in names(forecast_values)) {
    if (model_name %in% names(weights)) {
      combined_forecast <- combined_forecast + weights[model_name] * forecast_values[[model_name]]
    }
  }
  
  return(combined_forecast)
}

#' Combine forecasts from different methodologies (univariate, monthly, multivariate)
#'
#' @param univariate_results Results from univariate models
#' @param monthly_results Results from monthly indicator models
#' @param multivariate_results Results from multivariate models
#' @param variable_name Name of the variable to forecast
#' @param horizon Forecast horizon
#'
#' @return Combined forecast
#' @export
combine_methodology_forecasts <- function(univariate_results = NULL, 
                                          monthly_results = NULL, 
                                          multivariate_results = NULL, 
                                          variable_name, 
                                          horizon = FORECAST_HORIZON) {
  message("Combining forecasts for variable '", variable_name, "' at horizon ", horizon)
  
  # Step 1: Get the best forecast from each methodology
  
  # Univariate best forecast
  uni_best <- NULL
  if (!is.null(univariate_results)) {
    # Get weights for univariate models
    uni_weights <- calculate_weights(univariate_results$error_metrics, horizon = horizon)
    if (!is.null(uni_weights)) {
      # Combine univariate forecasts
      uni_best <- combine_weighted_forecasts(univariate_results$forecasts[[length(univariate_results$forecasts)]], 
                                             uni_weights)
    }
  }
  
  # Monthly indicators best forecast
  monthly_best <- NULL
  if (!is.null(monthly_results)) {
    # Get weights for monthly models
    monthly_weights <- calculate_weights(monthly_results$error_metrics, horizon = horizon)
    if (!is.null(monthly_weights)) {
      # Combine monthly forecasts
      monthly_best <- combine_weighted_forecasts(monthly_results$quarterly_forecasts[[length(monthly_results$quarterly_forecasts)]], 
                                                 monthly_weights)
    }
  }
  
  # Multivariate best forecast
  multi_best <- NULL
  if (!is.null(multivariate_results)) {
    # Get weights for multivariate models
    multi_weights <- calculate_weights(multivariate_results$error_metrics, 
                                       variable_name = variable_name, 
                                       horizon = horizon)
    if (!is.null(multi_weights)) {
      # Combine multivariate forecasts
      multi_best <- combine_weighted_forecasts(multivariate_results$forecasts[[length(multivariate_results$forecasts)]], 
                                               multi_weights, 
                                               variable_name = variable_name)
    }
  }
  
  # Step 2: Calculate methodology weights based on MSE of best forecasts
  
  # Collect MSE values for each methodology's best forecast
  methodology_mse <- c()
  
  # Univariate MSE
  if (!is.null(uni_best) && !is.null(univariate_results$error_metrics)) {
    # Find the best model based on weights
    best_model <- names(which.max(uni_weights))
    if (horizon <= length(univariate_results$error_metrics[[best_model]])) {
      uni_mse <- univariate_results$error_metrics[[best_model]][[horizon]]$MSE
      if (!is.null(uni_mse) && !is.na(uni_mse) && uni_mse > 0) {
        methodology_mse <- c(methodology_mse, setNames(uni_mse, "univariate"))
      }
    }
  }
  
  # Monthly MSE
  if (!is.null(monthly_best) && !is.null(monthly_results$error_metrics)) {
    # Find the best model based on weights
    best_model <- names(which.max(monthly_weights))
    if (horizon <= length(monthly_results$error_metrics[[best_model]])) {
      monthly_mse <- monthly_results$error_metrics[[best_model]][[horizon]]$MSE
      if (!is.null(monthly_mse) && !is.na(monthly_mse) && monthly_mse > 0) {
        methodology_mse <- c(methodology_mse, setNames(monthly_mse, "monthly"))
      }
    }
  }
  
  # Multivariate MSE
  if (!is.null(multi_best) && !is.null(multivariate_results$error_metrics)) {
    # Find the best model based on weights
    best_model <- names(which.max(multi_weights))
    if (variable_name %in% names(multivariate_results$error_metrics[[best_model]])) {
      if (horizon <= length(multivariate_results$error_metrics[[best_model]][[variable_name]])) {
        multi_mse <- multivariate_results$error_metrics[[best_model]][[variable_name]][[horizon]]$MSE
        if (!is.null(multi_mse) && !is.na(multi_mse) && multi_mse > 0) {
          methodology_mse <- c(methodology_mse, setNames(multi_mse, "multivariate"))
        }
      }
    }
  }
  
  # Check if we have any valid MSE values
  if (length(methodology_mse) == 0) {
    warning("No valid methodology MSE values found")
    
    # Return the first available forecast as fallback
    if (!is.null(uni_best)) return(uni_best)
    if (!is.null(monthly_best)) return(monthly_best)
    if (!is.null(multi_best)) return(multi_best)
    
    stop("No valid forecasts available")
  }
  
  # Calculate inverse MSE weights for methodologies
  inv_mse <- 1 / methodology_mse
  methodology_weights <- inv_mse / sum(inv_mse)
  
  message("Methodology weights: ", paste(names(methodology_weights), round(methodology_weights, 3), 
                                         sep = " = ", collapse = ", "))
  
  # Step 3: Combine best forecasts from each methodology
  
  # Find minimum forecast length
  forecast_lengths <- c()
  if (!is.null(uni_best)) forecast_lengths <- c(forecast_lengths, length(uni_best))
  if (!is.null(monthly_best)) forecast_lengths <- c(forecast_lengths, length(monthly_best))
  if (!is.null(multi_best)) forecast_lengths <- c(forecast_lengths, length(multi_best))
  
  min_length <- min(forecast_lengths)
  
  # Initialize combined forecast
  combined_forecast <- rep(0, min_length)
  
  # Add weighted forecasts
  if ("univariate" %in% names(methodology_weights) && !is.null(uni_best)) {
    combined_forecast <- combined_forecast + 
      methodology_weights["univariate"] * uni_best[1:min_length]
  }
  
  if ("monthly" %in% names(methodology_weights) && !is.null(monthly_best)) {
    combined_forecast <- combined_forecast + 
      methodology_weights["monthly"] * monthly_best[1:min_length]
  }
  
  if ("multivariate" %in% names(methodology_weights) && !is.null(multi_best)) {
    combined_forecast <- combined_forecast + 
      methodology_weights["multivariate"] * multi_best[1:min_length]
  }
  
  return(combined_forecast)
}

#' Create a time series from a forecast
#'
#' @param forecast Numeric vector containing forecast values
#' @param start_time Time point to start the forecast (from the last observed value)
#' @param frequency Frequency of the time series
#'
#' @return A time series object
#' @export
forecast_to_ts <- function(forecast, start_time, frequency = 4) {
  # Log details for debugging
  message("Creating forecast time series with:")
  message("  - Length: ", length(forecast))
  message("  - Start time: ", paste(start_time, collapse = ", "))
  message("  - Frequency: ", frequency)
  
  # Ensure forecast is numeric vector
  if (!is.numeric(forecast)) {
    warning("Converting non-numeric forecast to numeric")
    forecast <- as.numeric(forecast)
  }
  
  # Handle empty or NA forecasts
  if (length(forecast) == 0 || all(is.na(forecast))) {
    warning("Empty or all-NA forecast. Creating minimal valid forecast.")
    forecast <- rep(0, 4)  # Create minimal forecast with 4 periods
  } else if (any(is.na(forecast))) {
    warning("Forecast contains NA values. Replacing with interpolated values.")
    # Use simple linear interpolation for internal NAs
    valid_indices <- which(!is.na(forecast))
    if (length(valid_indices) > 0) {
      for (i in 1:length(forecast)) {
        if (is.na(forecast[i])) {
          # Find nearest valid values before and after
          before_idx <- max(valid_indices[valid_indices < i], 0)
          after_idx <- min(valid_indices[valid_indices > i], length(forecast) + 1)
          
          if (before_idx > 0 && after_idx <= length(forecast)) {
            # Interpolate
            before_val <- forecast[before_idx]
            after_val <- forecast[after_idx]
            forecast[i] <- before_val + (after_val - before_val) * 
              (i - before_idx) / (after_idx - before_idx)
          } else if (before_idx > 0) {
            # Use last valid value
            forecast[i] <- forecast[before_idx]
          } else if (after_idx <= length(forecast)) {
            # Use first valid value
            forecast[i] <- forecast[after_idx]
          } else {
            # Default to zero if no valid values
            forecast[i] <- 0
          }
        }
      }
    } else {
      # All values are NA, replace with zeros
      forecast <- rep(0, length(forecast))
    }
  }
  
  # Validate start_time
  if (is.null(start_time) || length(start_time) != 2 || 
      any(is.na(start_time)) || !is.numeric(start_time)) {
    warning("Invalid start_time. Using c(2025, 1) as fallback.")
    start_time <- c(2025, 1)
  }
  
  # Create a time series with explicit frequency
  forecast_ts <- tryCatch({
    ts_obj <- ts(forecast, start = start_time, frequency = frequency)
    
    # Explicitly verify the frequency was set correctly
    if (stats::frequency(ts_obj) != frequency) {
      message("Frequency mismatch detected. Fixing.")
      attr(ts_obj, "tsp")[3] <- frequency
    }
    
    ts_obj
  }, error = function(e) {
    warning("Error creating time series: ", e$message, 
            ". Creating fallback time series.")
    # Create a fallback time series
    ts(forecast, start = c(2025, 1), frequency = frequency)
  })
  
  # Final verification
  if (frequency(forecast_ts) != frequency) {
    warning("Frequency still incorrect. Force-fixing.")
    # Create a new ts object with the correct frequency
    forecast_ts <- ts(as.numeric(forecast_ts), 
                      start = start(forecast_ts),
                      frequency = frequency)
  }
  
  return(forecast_ts)
}

#' Convert a forecast from log scale to original scale
#'
#' @param forecast_ts Forecast time series (in log scale)
#'
#' @return Forecast time series in original scale
#' @export
exp_forecast <- function(forecast_ts) {
  # Apply exponential to convert from logarithmic scale
  exp_forecast_ts <- ts(exp(forecast_ts), 
                        start = start(forecast_ts), 
                        frequency = frequency(forecast_ts))
  return(exp_forecast_ts)
}

#' Calculate year-over-year growth rates from a time series
#'
#' @param ts_obj Time series object
#' @param log_transform Logical indicating whether the series is in log scale
#'
#' @return Time series of year-over-year growth rates (as percentages)
#' @export
calculate_yoy_growth_rates <- function(ts_obj, log_transform = FALSE) {
  # Check that frequency is at least 1
  freq <- frequency(ts_obj)
  if (freq < 1) {
    stop("Time series must have a frequency of at least 1")
  }
  
  if (log_transform) {
    # For log-transformed data, differences approximate percentage changes
    yoy_growth <- 100 * diff(ts_obj, lag = freq)
  } else {
    # For regular data, calculate percentage change
    lagged <- stats::lag(ts_obj, -freq)
    yoy_growth <- 100 * (ts_obj / lagged - 1)
  }
  
  return(yoy_growth)
}