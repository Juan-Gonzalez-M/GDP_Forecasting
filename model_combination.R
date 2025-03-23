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
    return(NULL)
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
  
  # Check if we're dealing with multivariate forecasts that need a variable name
  is_multivariate <- FALSE
  if (!is.null(forecasts[[1]]$fcst) || !is.null(forecasts[[1]][[1]])) {
    is_multivariate <- TRUE
    if (is.null(variable_name)) {
      stop("variable_name must be provided for multivariate forecasts")
    }
  }
  
  # Extract forecast values
  forecast_values <- list()
  for (model_name in model_names) {
    if (model_name %in% names(forecasts)) {
      if (is_multivariate) {
        # Handle VAR forecasts
        if (!is.null(forecasts[[model_name]]$fcst)) {
          # VAR forecast structure
          if (variable_name %in% names(forecasts[[model_name]]$fcst)) {
            forecast_values[[model_name]] <- forecasts[[model_name]]$fcst[[variable_name]][, "fcst"]
          }
        } else if (!is.null(forecasts[[model_name]][[variable_name]])) {
          # FAVAR or other list-based forecast structure
          forecast_values[[model_name]] <- forecasts[[model_name]][[variable_name]]
        } else {
          warning("Could not extract forecast values for model ", model_name)
        }
      } else if ("mean" %in% names(forecasts[[model_name]])) {
        # Univariate forecast object with 'mean' component
        forecast_values[[model_name]] <- as.numeric(forecasts[[model_name]]$mean)
      } else {
        # Direct numeric values
        forecast_values[[model_name]] <- as.numeric(forecasts[[model_name]])
      }
    }
  }
  
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
  # Create a time series from the forecast
  forecast_ts <- ts(forecast, start = start_time, frequency = frequency)
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