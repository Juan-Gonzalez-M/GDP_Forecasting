# univariate_models.R - Functions for univariate time series modeling
# This file contains functions for fitting and evaluating univariate models

#' Fit and evaluate univariate models on a time series
#'
#' @param ts_obj Time series object
#' @param log_transform Logical indicating whether to apply log transformation
#' @param rolling_window Size of the rolling window for evaluation
#' @param forecast_horizon Number of periods to forecast ahead
#'
#' @return A list containing fitted models and evaluation results
#' @export
fit_univariate_models <- function(ts_obj, log_transform = LOG_TRANSFORM, 
                                  rolling_window = ROLLING_WINDOW_SIZE, 
                                  forecast_horizon = FORECAST_HORIZON) {
  # Series name (for logging)
  series_name <- deparse(substitute(ts_obj))
  message("Fitting univariate models for ", series_name)
  
  # Apply log transformation if specified
  if (log_transform && min(ts_obj, na.rm = TRUE) > 0) {
    message("Applying log transformation")
    ts_transformed <- log(ts_obj)
  } else if (log_transform && min(ts_obj, na.rm = TRUE) <= 0) {
    warning("Log transformation requested but series contains non-positive values. Skipping transformation.")
    ts_transformed <- ts_obj
    log_transform <- FALSE
  } else {
    ts_transformed <- ts_obj
  }
  
  # Determine the number of observations
  n_obs <- length(ts_transformed)
  
  # Ensure there are enough observations for the rolling window
  if (n_obs <= rolling_window) {
    stop("Not enough observations (", n_obs, ") for the specified rolling window size (", 
         rolling_window, ")")
  }
  
  # Initialize lists to store models and forecasts
  models <- list()
  forecasts <- list()
  
  # Define model types and specifications
  model_specs <- list(
    # ARIMA models with different information criteria
    "arima_aic" = function(x) forecast::auto.arima(x, 
                                                   ic = "aic", 
                                                   seasonal = FALSE,
                                                   max.p = ARIMA_MAX_P,
                                                   max.q = ARIMA_MAX_Q,
                                                   max.d = ARIMA_MAX_D,
                                                   stepwise = TRUE,
                                                   approximation = (length(x) > 100)),
    
    "arima_bic" = function(x) forecast::auto.arima(x, 
                                                   ic = "bic", 
                                                   seasonal = FALSE,
                                                   max.p = ARIMA_MAX_P,
                                                   max.q = ARIMA_MAX_Q,
                                                   max.d = ARIMA_MAX_D,
                                                   stepwise = TRUE,
                                                   approximation = (length(x) > 100)),
    
    "arima_aicc" = function(x) forecast::auto.arima(x, 
                                                    ic = "aicc", 
                                                    seasonal = FALSE,
                                                    max.p = ARIMA_MAX_P,
                                                    max.q = ARIMA_MAX_Q,
                                                    max.d = ARIMA_MAX_D,
                                                    stepwise = TRUE,
                                                    approximation = (length(x) > 100)),
    
    # SARIMA models with different information criteria
    "sarima_aic" = function(x) forecast::auto.arima(x, 
                                                    ic = "aic", 
                                                    seasonal = TRUE,
                                                    max.p = ARIMA_MAX_P,
                                                    max.q = ARIMA_MAX_Q,
                                                    max.d = ARIMA_MAX_D,
                                                    max.P = 2,
                                                    max.Q = 2,
                                                    max.D = 1,
                                                    stepwise = TRUE,
                                                    approximation = (length(x) > 100)),
    
    "sarima_bic" = function(x) forecast::auto.arima(x, 
                                                    ic = "bic", 
                                                    seasonal = TRUE,
                                                    max.p = ARIMA_MAX_P,
                                                    max.q = ARIMA_MAX_Q,
                                                    max.d = ARIMA_MAX_D,
                                                    max.P = 2,
                                                    max.Q = 2,
                                                    max.D = 1,
                                                    stepwise = TRUE,
                                                    approximation = (length(x) > 100)),
    
    "sarima_aicc" = function(x) forecast::auto.arima(x, 
                                                     ic = "aicc", 
                                                     seasonal = TRUE,
                                                     max.p = ARIMA_MAX_P,
                                                     max.q = ARIMA_MAX_Q,
                                                     max.d = ARIMA_MAX_D,
                                                     max.P = 2,
                                                     max.Q = 2,
                                                     max.D = 1,
                                                     stepwise = TRUE,
                                                     approximation = (length(x) > 100)),
    
    # ETS models with different information criteria
    "ets_aic" = function(x) forecast::ets(x, 
                                          ic = "aic", 
                                          opt.crit = "lik",
                                          allow.multiplicative.trend = TRUE),
    
    "ets_bic" = function(x) forecast::ets(x, 
                                          ic = "bic", 
                                          opt.crit = "lik",
                                          allow.multiplicative.trend = TRUE),
    
    "ets_aicc" = function(x) forecast::ets(x, 
                                           ic = "aicc", 
                                           opt.crit = "lik",
                                           allow.multiplicative.trend = TRUE),
    
    # Neural Network Autoregression
    "nnetar" = function(x) forecast::nnetar(x, 
                                            p = min(max(frequency(x), 1), 12),
                                            size = min(max(floor(length(x)/10), 1), 10),
                                            repeats = 20)
  )
  
  # Fit each model to the entire series
  message("Fitting models to full series...")
  for (name in names(model_specs)) {
    models[[name]] <- tryCatch({
      model <- model_specs[[name]](ts_transformed)
      message("  Successfully fit model: ", name)
      model
    }, error = function(e) {
      warning("Error fitting model ", name, ": ", e$message)
      return(NULL)
    })
  }
  
  # Perform rolling window forecasts
  message("Performing rolling window forecasts...")
  forecasts <- list()
  actual_values <- list()
  
  # Check that there's enough data for rolling windows
  if (n_obs <= rolling_window + forecast_horizon) {
    warning("Not enough observations (", n_obs, ") for rolling window evaluation. ",
            "Need at least ", rolling_window + forecast_horizon + 1, " observations.")
    return(list(
      original_series = ts_obj,
      transformed_series = ts_transformed,
      log_transform = log_transform,
      models = models,
      forecasts = list(),
      actual_values = list(),
      error_metrics = list()
    ))
  }
  
  # Make sure we don't try to create windows beyond the available data
  max_windows <- n_obs - rolling_window - forecast_horizon
  
  for (i in 0:max_windows) {
    # Define training and test periods
    train_end <- rolling_window + i
    test_start <- train_end + 1
    test_end <- min(train_end + forecast_horizon, n_obs)
    
    # Additional safety check
    if (test_end <= test_start || test_end > n_obs) {
      message("  Skipping window ", i+1, ": invalid test period")
      next
    }
    
    message("  Window ", i+1, ": training up to ", train_end, 
            ", testing from ", test_start, " to ", test_end)
    
    # Extract training data (with additional safety check)
    if (train_end > length(ts_transformed)) {
      warning("Training end index out of bounds: ", train_end, " > ", length(ts_transformed))
      next
    }
    
    train_data <- window(ts_transformed, end = time(ts_transformed)[train_end])
    
    # Extract actual values for test period (with safety checks)
    if (test_start > length(ts_transformed) || test_end > length(ts_transformed)) {
      warning("Test indices out of bounds: start=", test_start, ", end=", test_end, 
              ", max=", length(ts_transformed))
      next
    }
    
    # Store actual values for this window
    actual_values[[i+1]] <- window(ts_transformed, 
                                   start = time(ts_transformed)[test_start], 
                                   end = time(ts_transformed)[test_end])
    
    # Initialize list for this window's forecasts
    forecasts[[i+1]] <- list()
    
    # Fit models and generate forecasts
    for (name in names(model_specs)) {
      # Skip if the model could not be fitted to the entire series
      if (is.null(models[[name]])) {
        next
      }
      
      tryCatch({
        # Fit the model to the training data
        model <- model_specs[[name]](train_data)
        
        # Generate forecasts
        h <- test_end - test_start + 1
        fc <- forecast::forecast(model, h = h)
        
        # Store forecast
        forecasts[[i+1]][[name]] <- fc
      }, error = function(e) {
        warning("Error forecasting with model ", name, " at window ", i+1, ": ", e$message)
      })
    }
  }
  
  # Calculate error metrics for each model and horizon
  message("Calculating error metrics...")
  error_metrics <- calculate_forecast_errors(forecasts, actual_values, log_transform)
  
  # Return the results
  return(list(
    original_series = ts_obj,
    transformed_series = ts_transformed,
    log_transform = log_transform,
    models = models,
    forecasts = forecasts,
    actual_values = actual_values,
    error_metrics = error_metrics
  ))
}

#' Calculate error metrics for forecasts
#'
#' @param forecasts List of forecast objects
#' @param actual_values List of actual values
#' @param log_transform Logical indicating whether values were log-transformed
#'
#' @return A nested list of error metrics by model, horizon, and metric type
#' @export
calculate_forecast_errors <- function(forecasts, actual_values, log_transform = FALSE) {
  # Initialize error metrics structure
  error_metrics <- list()
  
  # Check if forecasts list is empty
  if (length(forecasts) == 0 || length(forecasts[[1]]) == 0) {
    warning("No forecasts available for error calculation")
    return(error_metrics)
  }
  
  # Safety check - ensure actual_values and forecasts have the same length
  min_length <- min(length(forecasts), length(actual_values))
  if (min_length == 0) {
    warning("No valid forecast-actual pairs found for error calculation")
    return(error_metrics)
  }
  
  model_names <- names(forecasts[[1]])
  
  # For each model
  for (model_name in model_names) {
    error_metrics[[model_name]] <- list()
    
    # Initialize containers for each error metric by horizon
    max_horizon <- 0
    for (i in seq_along(forecasts)) {
      if (model_name %in% names(forecasts[[i]])) {
        fc <- forecasts[[i]][[model_name]]
        max_horizon <- max(max_horizon, length(fc$mean))
      }
    }
    
    # Initialize error accumulators for each horizon
    for (h in 1:max_horizon) {
      error_metrics[[model_name]][[h]] <- list(
        errors = numeric(),      # Raw errors (actual - forecast)
        abs_errors = numeric(),  # Absolute errors
        sq_errors = numeric(),   # Squared errors
        perc_errors = numeric()  # Percentage errors
      )
    }
    
    # Collect errors for each forecast window
    for (i in seq_along(forecasts)) {
      # Safety check - skip if i is beyond actual_values length
      if (i > length(actual_values)) {
        warning("Skipping window ", i, ": actual values not available")
        next
      }
      
      if (!(model_name %in% names(forecasts[[i]]))) next
      
      fc <- forecasts[[i]][[model_name]]
      actual <- actual_values[[i]]
      
      # Convert from log space if necessary
      if (log_transform) {
        fc_values <- exp(fc$mean)
        actual_values_i <- exp(actual)
      } else {
        fc_values <- fc$mean
        actual_values_i <- actual
      }
      
      # Calculate errors for each horizon
      for (h in 1:min(length(fc_values), length(actual_values_i))) {
        # Raw error
        error <- actual_values_i[h] - fc_values[h]
        error_metrics[[model_name]][[h]]$errors <- 
          c(error_metrics[[model_name]][[h]]$errors, error)
        
        # Absolute error
        abs_error <- abs(error)
        error_metrics[[model_name]][[h]]$abs_errors <- 
          c(error_metrics[[model_name]][[h]]$abs_errors, abs_error)
        
        # Squared error
        sq_error <- error^2
        error_metrics[[model_name]][[h]]$sq_errors <- 
          c(error_metrics[[model_name]][[h]]$sq_errors, sq_error)
        
        # Percentage error (only if actual is not too close to zero)
        if (abs(actual_values_i[h]) > 1e-10) {
          perc_error <- 100 * error / actual_values_i[h]
          error_metrics[[model_name]][[h]]$perc_errors <- 
            c(error_metrics[[model_name]][[h]]$perc_errors, perc_error)
        }
      }
    }
    
    # Calculate summary metrics for each horizon
    for (h in 1:max_horizon) {
      # Mean Absolute Error (MAE)
      error_metrics[[model_name]][[h]]$MAE <- 
        mean(error_metrics[[model_name]][[h]]$abs_errors, na.rm = TRUE)
      
      # Root Mean Squared Error (RMSE)
      error_metrics[[model_name]][[h]]$RMSE <- 
        sqrt(mean(error_metrics[[model_name]][[h]]$sq_errors, na.rm = TRUE))
      
      # Mean Squared Error (MSE)
      error_metrics[[model_name]][[h]]$MSE <- 
        mean(error_metrics[[model_name]][[h]]$sq_errors, na.rm = TRUE)
      
      # Mean Absolute Percentage Error (MAPE)
      if (length(error_metrics[[model_name]][[h]]$perc_errors) > 0) {
        error_metrics[[model_name]][[h]]$MAPE <- 
          mean(abs(error_metrics[[model_name]][[h]]$perc_errors), na.rm = TRUE)
      } else {
        error_metrics[[model_name]][[h]]$MAPE <- NA
      }
    }
  }
  
  return(error_metrics)
}

#' Get the best model for each forecast horizon based on a specific error metric
#'
#' @param error_metrics List of error metrics as returned by calculate_forecast_errors()
#' @param metric Error metric to use for comparison ("MAE", "RMSE", "MSE", or "MAPE")
#'
#' @return A list with the best model for each horizon
#' @export
get_best_models <- function(error_metrics, metric = "RMSE") {
  # Validate input
  if (!metric %in% c("MAE", "RMSE", "MSE", "MAPE")) {
    stop("Invalid metric. Must be one of 'MAE', 'RMSE', 'MSE', or 'MAPE'.")
  }
  
  # Find the maximum horizon
  max_horizon <- 0
  for (model_name in names(error_metrics)) {
    max_horizon <- max(max_horizon, length(error_metrics[[model_name]]))
  }
  
  # Initialize result list
  best_models <- list()
  
  # For each horizon
  for (h in 1:max_horizon) {
    # Collect error values for this horizon from all models
    model_errors <- c()
    
    for (model_name in names(error_metrics)) {
      if (h <= length(error_metrics[[model_name]])) {
        error_value <- error_metrics[[model_name]][[h]][[metric]]
        if (!is.na(error_value)) {
          model_errors <- c(model_errors, setNames(error_value, model_name))
        }
      }
    }
    
    if (length(model_errors) > 0) {
      # Find the model with the minimum error
      best_model <- names(model_errors)[which.min(model_errors)]
      best_error <- min(model_errors, na.rm = TRUE)
      
      best_models[[h]] <- list(
        model = best_model,
        error = best_error
      )
    } else {
      best_models[[h]] <- list(
        model = NA,
        error = NA
      )
    }
  }
  
  return(best_models)
}

#' Calculate inverse MSE weights for model combination
#'
#' @param error_metrics List of error metrics as returned by calculate_forecast_errors()
#' @param horizon Forecast horizon to use for weight calculation
#'
#' @return A named vector of weights
#' @export
calculate_inverse_mse_weights <- function(error_metrics, horizon = 1) {
  # Collect MSE values for the specified horizon
  mse_values <- c()
  
  for (model_name in names(error_metrics)) {
    if (horizon <= length(error_metrics[[model_name]])) {
      mse <- error_metrics[[model_name]][[horizon]]$MSE
      if (!is.na(mse) && mse > 0) {
        mse_values <- c(mse_values, setNames(mse, model_name))
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

#' Combine forecasts from multiple models using inverse MSE weights
#'
#' @param models List of fitted models
#' @param error_metrics List of error metrics
#' @param newdata Optional new data for forecasting
#' @param horizon Forecast horizon
#'
#' @return A forecast object with the combined forecast
#' @export
combine_forecasts <- function(models, error_metrics, newdata = NULL, 
                              horizon = FORECAST_HORIZON) {
  # Calculate weights based on inverse MSE
  weights <- calculate_inverse_mse_weights(error_metrics, horizon = 1)
  
  if (is.null(weights)) {
    stop("Could not calculate weights for forecast combination")
  }
  
  # Keep only models with weights
  models <- models[names(weights)]
  
  # Generate individual forecasts
  forecasts <- list()
  for (model_name in names(models)) {
    if (!is.null(models[[model_name]])) {
      forecasts[[model_name]] <- forecast::forecast(models[[model_name]], h = horizon)
    }
  }
  
  if (length(forecasts) == 0) {
    stop("No valid forecasts to combine")
  }
  
  # Get the time points from the first forecast
  first_forecast <- forecasts[[1]]
  forecast_times <- time(first_forecast$mean)
  
  # Initialize combined forecast as a weighted sum
  combined_mean <- rep(0, horizon)
  
  # Add weighted forecasts
  for (model_name in names(forecasts)) {
    combined_mean <- combined_mean + weights[model_name] * as.numeric(forecasts[[model_name]]$mean)
  }
  
  # Create a time series for the combined mean
  combined_mean_ts <- ts(combined_mean, 
                         start = start(first_forecast$mean), 
                         frequency = frequency(first_forecast$mean))
  
  # Create a simple forecast object with the combined mean
  combined_forecast <- list(
    mean = combined_mean_ts,
    method = "Combined Forecast",
    x = first_forecast$x,
    series = first_forecast$series,
    fitted = first_forecast$fitted,  # Using fitted values from first model as placeholder
    residuals = first_forecast$residuals  # Using residuals from first model as placeholder
  )
  
  class(combined_forecast) <- "forecast"
  
  return(combined_forecast)
}

#' Calculate year-over-year growth rates
#'
#' @param ts_obj Time series object
#' @param log_transform Logical indicating whether the series is in logs
#'
#' @return Time series of year-over-year growth rates (as percentages)
#' @export
calculate_yoy_growth <- function(ts_obj, log_transform = FALSE) {
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

#' Plot actual vs. forecast values
#'
#' @param ts_obj Original time series
#' @param forecasts List of forecast objects
#' @param title Plot title
#'
#' @return ggplot object
#' @export
plot_forecasts <- function(ts_obj, forecasts, title = "Forecast Comparison") {
  # Create a data frame for the original series
  df_original <- data.frame(
    date = zoo::index(zoo::as.zoo(ts_obj)),
    value = as.numeric(ts_obj),
    series = "Actual"
  )
  
  # Create a list to store forecast data frames
  df_forecasts <- list()
  
  # Process each forecast
  for (name in names(forecasts)) {
    # Extract the forecast mean
    fc_mean <- forecasts[[name]]$mean
    
    # Create a data frame for this forecast
    df_forecasts[[name]] <- data.frame(
      date = zoo::index(zoo::as.zoo(fc_mean)),
      value = as.numeric(fc_mean),
      series = name
    )
  }
  
  # Combine all data frames
  df_combined <- rbind(df_original, do.call(rbind, df_forecasts))
  
  # Create the plot
  plot <- ggplot2::ggplot(df_combined, ggplot2::aes(x = date, y = value, color = series)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = title, x = "Date", y = "Value") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  
  return(plot)
}