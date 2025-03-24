# multivariate_models.R - Functions for multivariate time series modeling
# This file contains functions for VAR, BVAR, and FAVAR models

#' Fit and evaluate VAR models on multivariate time series
#'
#' @param ts_list List of time series objects (endogenous variables)
#' @param exog_ts_list Optional list of exogenous time series objects
#' @param log_transform Logical indicating whether to apply log transformation
#' @param rolling_window Size of the rolling window for evaluation
#' @param forecast_horizon Number of periods to forecast ahead
#' @param max_lags Maximum number of lags to consider
#' @param ic Information criterion for lag selection ("AIC", "BIC", "HQ", "FPE")
#'
#' @return A list containing fitted models and evaluation results
#' @export
fit_var_models <- function(ts_list, exog_ts_list = NULL, log_transform = LOG_TRANSFORM, 
                           rolling_window = ROLLING_WINDOW_SIZE, 
                           forecast_horizon = FORECAST_HORIZON,
                           max_lags = VAR_MAX_LAGS,
                           ic = VAR_IC) {
  message("Fitting VAR models for ", length(ts_list), " series")
  
  # Validate input
  if (length(ts_list) < 2) {
    stop("VAR models require at least 2 time series")
  }
  
  # Apply log transformation if specified
  transformed_list <- ts_list
  if (log_transform) {
    message("Applying log transformation to all series")
    for (name in names(ts_list)) {
      if (min(ts_list[[name]], na.rm = TRUE) > 0) {
        transformed_list[[name]] <- log(ts_list[[name]])
      } else {
        warning("Series '", name, "' contains non-positive values. Skipping log transformation.")
      }
    }
  }
  
  # Align all time series to have the same start and end dates
  aligned_list <- align_multiple_ts(transformed_list)
  
  # Also align exogenous variables if provided
  if (!is.null(exog_ts_list)) {
    exog_aligned_list <- align_multiple_ts(exog_ts_list)
    # Further align exogenous with endogenous variables
    common_range <- find_common_time_range(
      c(time(aligned_list[[1]])[1], time(aligned_list[[1]])[length(aligned_list[[1]])]),
      c(time(exog_aligned_list[[1]])[1], time(exog_aligned_list[[1]])[length(exog_aligned_list[[1]])])
    )
    
    # Rewindow all series
    for (name in names(aligned_list)) {
      aligned_list[[name]] <- window(aligned_list[[name]], 
                                     start = common_range[1], 
                                     end = common_range[2])
    }
    
    for (name in names(exog_aligned_list)) {
      exog_aligned_list[[name]] <- window(exog_aligned_list[[name]], 
                                          start = common_range[1], 
                                          end = common_range[2])
    }
  }
  
  # Convert list of time series to a matrix
  model_data <- ts_list_to_matrix(aligned_list)
  
  # Prepare exogenous data if provided
  exog_data <- NULL
  if (!is.null(exog_ts_list)) {
    exog_data <- ts_list_to_matrix(exog_aligned_list)
  }
  
  # Determine best lag order for VAR with fallbacks
  max_lag_attempts <- c(max_lags, 4, 2, 1)  # Try progressively simpler models
  var_select <- NULL
  optimal_lag <- 1  # Default fallback
  
  for (try_lag in max_lag_attempts) {
    var_select <- tryCatch({
      if (is.null(exog_data)) {
        vars::VARselect(model_data, lag.max = try_lag, type = "const")
      } else {
        vars::VARselect(model_data, lag.max = try_lag, type = "const", exogen = exog_data)
      }
    }, error = function(e) {
      warning("Error in VAR lag selection with max lag ", try_lag, ": ", e$message)
      return(NULL)
    })
    
    if (!is.null(var_select) && !is.na(var_select$selection[ic])) {
      optimal_lag <- var_select$selection[ic]
      message("Selected optimal lag order: ", optimal_lag, " based on ", ic, 
              " (attempt with max lag ", try_lag, ")")
      break
    }
  }
  
  if (is.null(var_select) || is.na(optimal_lag)) {
    optimal_lag <- min(4, nrow(model_data) %/% 10)  # Rule of thumb: 10% of sample size
    warning("VAR lag selection failed. Using fallback lag ", optimal_lag)
  }
  
  message("Selected optimal lag order: ", optimal_lag, " based on ", ic)
  
  # Initialize lists to store models and forecasts
  models <- list()
  forecasts <- list()
  actual_values <- list()
  
  # Define VAR model specifications
  model_specs <- list(
    # Regular VAR with constant
    "var_const" = function(data, p, exog = NULL) {
      if (is.null(exog)) {
        vars::VAR(data, p = p, type = "const")
      } else {
        vars::VAR(data, p = p, type = "const", exogen = exog)
      }
    },
    
    # VAR with trend
    "var_trend" = function(data, p, exog = NULL) {
      if (is.null(exog)) {
        vars::VAR(data, p = p, type = "trend")
      } else {
        vars::VAR(data, p = p, type = "trend", exogen = exog)
      }
    },
    
    # VAR with constant and trend
    "var_both" = function(data, p, exog = NULL) {
      if (is.null(exog)) {
        vars::VAR(data, p = p, type = "both")
      } else {
        vars::VAR(data, p = p, type = "both", exogen = exog)
      }
    }
  )
  
  # Fit each VAR model specification to the full dataset
  message("Fitting VAR models to full dataset...")
  for (name in names(model_specs)) {
    models[[name]] <- tryCatch({
      model <- model_specs[[name]](model_data, p = optimal_lag, exog = exog_data)
      message("  Successfully fit model: ", name)
      model
    }, error = function(e) {
      warning("Error fitting model ", name, ": ", e$message)
      return(NULL)
    })
  }
  
  # Check if all models failed to fit
  all_models_null <- all(sapply(models, is.null))
  if (all_models_null) {
    warning("All VAR model specifications failed to fit. Skipping rolling window evaluation.")
    return(list(
      original_series = ts_list,
      transformed_series = transformed_list,
      log_transform = log_transform,
      aligned_series = aligned_list,
      models = models,
      optimal_lag = optimal_lag,
      forecasts = list(),
      actual_values = list(),
      error_metrics = list()
    ))
  }
  
  # Perform rolling window forecasts
  message("Performing rolling window forecasts...")
  
  # Determine number of observations
  n_obs <- nrow(model_data)
  
  # Ensure there are enough observations for the rolling window
  if (n_obs <= rolling_window + optimal_lag) {
    warning("Not enough observations (", n_obs, ") for the specified rolling window size (", 
            rolling_window, ") and lag order (", optimal_lag, "). Skipping rolling window evaluation.")
    return(list(
      original_series = ts_list,
      transformed_series = transformed_list,
      log_transform = log_transform,
      aligned_series = aligned_list,
      models = models,
      optimal_lag = optimal_lag,
      forecasts = list(),
      actual_values = list(),
      error_metrics = list()
    ))
  }
  
  # Perform rolling window evaluation
  valid_windows <- 0  # Counter for successful windows
  
  # Calculate maximum possible windows
  max_windows <- n_obs - rolling_window - forecast_horizon - optimal_lag
  if (max_windows < 0) max_windows <- 0
  
  for (i in 0:max_windows) {
    # Define training and test indices
    train_end <- rolling_window + i
    test_start <- train_end + 1
    test_end <- min(train_end + forecast_horizon, n_obs)
    
    if (test_end <= test_start) {
      message("  Skipping window ", i+1, ": invalid test period")
      next
    }
    
    message("  Window ", i+1, ": training up to index ", train_end, 
            ", testing from ", test_start, " to ", test_end)
    
    # Extract training data
    train_data <- model_data[1:train_end, ]
    
    # Extract exogenous training data if available
    train_exog <- NULL
    test_exog <- NULL
    if (!is.null(exog_data)) {
      if (train_end <= nrow(exog_data)) {
        train_exog <- exog_data[1:train_end, ]
      }
      if (test_end <= nrow(exog_data)) {
        test_exog <- exog_data[test_start:test_end, ]
      }
    }
    
    # Store actual values for this window (with proper bounds checking)
    if (test_start <= nrow(model_data) && test_end <= nrow(model_data)) {
      actual_values[[valid_windows + 1]] <- model_data[test_start:test_end, ]
    } else {
      warning("Test indices out of bounds for window ", i+1, ". Skipping.")
      next
    }
    
    # Initialize list for this window's forecasts
    window_forecasts <- list()
    window_has_forecasts <- FALSE
    
    # Fit models and generate forecasts
    for (name in names(model_specs)) {
      # Skip if the model could not be fitted to the entire series
      if (is.null(models[[name]])) {
        next
      }
      
      tryCatch({
        # Fit the model to the training data
        model <- model_specs[[name]](train_data, p = optimal_lag, exog = train_exog)
        
        # Generate forecasts
        h <- test_end - test_start + 1
        fc <- predict(model, n.ahead = h, dumvar = test_exog)
        
        # Store forecast
        window_forecasts[[name]] <- fc
        window_has_forecasts <- TRUE
      }, error = function(e) {
        warning("Error forecasting with model ", name, " at window ", i+1, ": ", e$message)
      })
    }
    
    # Only count this window if we got at least one forecast
    if (window_has_forecasts) {
      forecasts[[valid_windows + 1]] <- window_forecasts
      valid_windows <- valid_windows + 1
    } else {
      # If no forecasts were successful, remove the corresponding actual values
      actual_values[[valid_windows + 1]] <- NULL
    }
  }
  
  # Check if we have any valid forecasts
  if (valid_windows == 0) {
    warning("No valid forecasts could be generated. Skipping error metrics calculation.")
    return(list(
      original_series = ts_list,
      transformed_series = transformed_list,
      log_transform = log_transform,
      aligned_series = aligned_list,
      models = models,
      optimal_lag = optimal_lag,
      forecasts = list(),
      actual_values = list(),
      error_metrics = list()
    ))
  }
  
  # Calculate error metrics for each model, variable, and horizon
  message("Calculating error metrics...")
  error_metrics <- tryCatch({
    calculate_var_forecast_errors(forecasts, actual_values, colnames(model_data))
  }, error = function(e) {
    warning("Error calculating forecast errors: ", e$message)
    list()  # Return empty list on error
  })
  
  # Return the results
  return(list(
    original_series = ts_list,
    transformed_series = transformed_list,
    log_transform = log_transform,
    aligned_series = aligned_list,
    models = models,
    optimal_lag = optimal_lag,
    forecasts = forecasts,
    actual_values = actual_values,
    error_metrics = error_metrics
  ))
}

#' Fit and evaluate Bayesian VAR (BVAR) models
#'
#' @param ts_list List of time series objects (endogenous variables)
#' @param exog_ts_list Optional list of exogenous time series objects
#' @param log_transform Logical indicating whether to apply log transformation
#' @param rolling_window Size of the rolling window for evaluation
#' @param forecast_horizon Number of periods to forecast ahead
#' @param lags Vector of lag orders to consider
#' @param prior Prior type for BVAR ("minnesota", "normal-wishart", "soc")
#'
#' @return A list containing fitted models and evaluation results
#' @export
fit_bvar_models <- function(ts_list, exog_ts_list = NULL, log_transform = LOG_TRANSFORM, 
                            rolling_window = ROLLING_WINDOW_SIZE, 
                            forecast_horizon = FORECAST_HORIZON,
                            lags = BVAR_LAGS,
                            prior = BVAR_PRIOR) {
  message("Fitting Bayesian VAR models for ", length(ts_list), " series")
  
  # Validate input
  if (length(ts_list) < 2) {
    stop("BVAR models require at least 2 time series")
  }
  
  # Apply log transformation if specified
  transformed_list <- ts_list
  if (log_transform) {
    message("Applying log transformation to all series")
    for (name in names(ts_list)) {
      if (min(ts_list[[name]], na.rm = TRUE) > 0) {
        transformed_list[[name]] <- log(ts_list[[name]])
      } else {
        warning("Series '", name, "' contains non-positive values. Skipping log transformation.")
      }
    }
  }
  
  # Align all time series to have the same start and end dates
  aligned_list <- align_multiple_ts(transformed_list)
  
  # Also align exogenous variables if provided
  if (!is.null(exog_ts_list)) {
    exog_aligned_list <- align_multiple_ts(exog_ts_list)
    # Further align exogenous with endogenous variables
    common_range <- find_common_time_range(
      c(time(aligned_list[[1]])[1], time(aligned_list[[1]])[length(aligned_list[[1]])]),
      c(time(exog_aligned_list[[1]])[1], time(exog_aligned_list[[1]])[length(exog_aligned_list[[1]])])
    )
    
    # Rewindow all series
    for (name in names(aligned_list)) {
      aligned_list[[name]] <- window(aligned_list[[name]], 
                                     start = common_range[1], 
                                     end = common_range[2])
    }
    
    for (name in names(exog_aligned_list)) {
      exog_aligned_list[[name]] <- window(exog_aligned_list[[name]], 
                                          start = common_range[1], 
                                          end = common_range[2])
    }
  }
  
  # Convert list of time series to a data frame
  model_data <- ts_list_to_df(aligned_list)
  
  # Prepare exogenous data if provided
  exog_data <- NULL
  if (!is.null(exog_ts_list)) {
    exog_data <- ts_list_to_df(exog_aligned_list)
  }
  
  # Initialize lists to store models and forecasts
  models <- list()
  forecasts <- list()
  actual_values <- list()
  
  # Define BVAR model specifications
  model_specs <- list()
  
  # Create specifications for each lag
  for (lag in lags) {
    if (prior == "minnesota") {
      model_name <- paste0("bvar_minnesota_lag", lag)
      model_specs[[model_name]] <- function(data, p = lag, exog = NULL) {
        # Setup Minnesota prior
        mn_prior <- BVAR::bv_minnesota(
          lambda = 0.2,         # Overall tightness
          alpha = 2,            # Lag decay
          var_level = TRUE,     # Include levels of variables?
          var_decay = 0.5,      # Prior on variance of coefficients
          horizon = forecast_horizon # Forecast horizon for hyperparameter optimization
        )
        
        # Setup exogenous variables
        if (!is.null(exog)) {
          mn_prior$exogenous <- exog
        }
        
        # Fit BVAR model
        BVAR::bvar(data, lags = p, priors = mn_prior)
      }
    } else if (prior == "normal-wishart") {
      model_name <- paste0("bvar_normalwishart_lag", lag)
      model_specs[[model_name]] <- function(data, p = lag, exog = NULL) {
        # Setup Normal-Wishart prior
        nw_prior <- BVAR::bv_nwishart(
          scale = 0.1,
          horizon = forecast_horizon
        )
        
        # Setup exogenous variables
        if (!is.null(exog)) {
          nw_prior$exogenous <- exog
        }
        
        # Fit BVAR model
        BVAR::bvar(data, lags = p, priors = nw_prior)
      }
    } else if (prior == "soc") {
      model_name <- paste0("bvar_soc_lag", lag)
      model_specs[[model_name]] <- function(data, p = lag, exog = NULL) {
        # Setup Sum-of-Coefficients prior
        soc_prior <- BVAR::bv_soc(
          lambda = 0.1,
          horizon = forecast_horizon
        )
        
        # Setup exogenous variables
        if (!is.null(exog)) {
          soc_prior$exogenous <- exog
        }
        
        # Fit BVAR model
        BVAR::bvar(data, lags = p, priors = soc_prior)
      }
    }
  }
  
  # Fit each BVAR model specification to the full dataset
  message("Fitting BVAR models to full dataset...")
  for (name in names(model_specs)) {
    models[[name]] <- tryCatch({
      model <- model_specs[[name]](model_data, exog = exog_data)
      message("  Successfully fit model: ", name)
      model
    }, error = function(e) {
      warning("Error fitting model ", name, ": ", e$message)
      return(NULL)
    })
  }
  
  # Perform rolling window forecasts
  message("Performing rolling window forecasts...")
  
  # Determine number of observations
  n_obs <- nrow(model_data)
  max_lag <- max(lags)
  
  # Ensure there are enough observations for the rolling window
  if (n_obs <= rolling_window + max_lag) {
    stop("Not enough observations (", n_obs, ") for the specified rolling window size (", 
         rolling_window, ") and maximum lag order (", max_lag, ")")
  }
  
  # Perform rolling window evaluation
  for (i in 0:(n_obs - rolling_window - forecast_horizon - max_lag)) {
    # Define training and test indices
    train_end <- rolling_window + i
    test_start <- train_end + 1
    test_end <- min(train_end + forecast_horizon, n_obs)
    
    if (test_end <= test_start) next
    
    message("  Window ", i+1, ": training up to index ", train_end, 
            ", testing from ", test_start, " to ", test_end)
    
    # Extract training data
    train_data <- model_data[1:train_end, ]
    
    # Extract exogenous training data if available
    train_exog <- NULL
    test_exog <- NULL
    if (!is.null(exog_data)) {
      train_exog <- exog_data[1:train_end, ]
      if (test_end <= nrow(exog_data)) {
        test_exog <- exog_data[test_start:test_end, ]
      }
    }
    
    # Store actual values for this window
    actual_values[[i+1]] <- as.matrix(model_data[test_start:test_end, ])
    
    # Initialize list for this window's forecasts
    forecasts[[i+1]] <- list()
    
    # Fit models and generate forecasts
    for (name in names(model_specs)) {
      # Skip if the model could not be fitted to the entire series
      if (is.null(models[[name]])) {
        next
      }
      
      tryCatch({
        # Extract lag from model name
        lag <- as.numeric(gsub(".*lag(\\d+)$", "\\1", name))
        
        # Fit the model to the training data
        model <- model_specs[[name]](train_data, p = lag, exog = train_exog)
        
        # Generate forecasts
        h <- test_end - test_start + 1
        
        # Use predict method for BVAR objects
        fc <- predict(model, horizon = h)
        
        # Extract the forecast means
        fc_means <- fc$fcast$mean
        
        # Store forecast
        forecasts[[i+1]][[name]] <- fc_means
      }, error = function(e) {
        warning("Error forecasting with model ", name, " at window ", i+1, ": ", e$message)
      })
    }
  }
  
  # Calculate error metrics for each model, variable, and horizon
  message("Calculating error metrics...")
  error_metrics <- calculate_bvar_forecast_errors(forecasts, actual_values, colnames(model_data))
  
  # Return the results
  return(list(
    original_series = ts_list,
    transformed_series = transformed_list,
    log_transform = log_transform,
    aligned_series = aligned_list,
    models = models,
    forecasts = forecasts,
    actual_values = actual_values,
    error_metrics = error_metrics
  ))
}

#' Fit and evaluate Factor-Augmented VAR (FAVAR) models
#'
#' @param ts_list List of time series objects (endogenous variables)
#' @param factor_ts_list List of time series for factor extraction
#' @param exog_ts_list Optional list of exogenous time series objects
#' @param log_transform Logical indicating whether to apply log transformation
#' @param rolling_window Size of the rolling window for evaluation
#' @param forecast_horizon Number of periods to forecast ahead
#' @param max_lags Maximum number of lags to consider
#' @param n_factors Number of factors to extract
#'
#' @return A list containing fitted models and evaluation results
#' @export
fit_favar_models <- function(ts_list, factor_ts_list, exog_ts_list = NULL, 
                             log_transform = LOG_TRANSFORM, 
                             rolling_window = ROLLING_WINDOW_SIZE, 
                             forecast_horizon = FORECAST_HORIZON,
                             max_lags = VAR_MAX_LAGS,
                             n_factors = 3) {
  message("Fitting FAVAR models for ", length(ts_list), " series with ", 
          n_factors, " factors from ", length(factor_ts_list), " input series")
  
  # Validate input
  if (length(ts_list) < 1) {
    stop("FAVAR models require at least 1 time series of interest")
  }
  
  if (length(factor_ts_list) < n_factors) {
    stop("Not enough series for factor extraction. Need at least ", n_factors, " series.")
  }
  
  # Apply log transformation if specified
  transformed_list <- ts_list
  transformed_factor_list <- factor_ts_list
  
  if (log_transform) {
    message("Applying log transformation to all series")
    
    # Transform main series
    for (name in names(ts_list)) {
      if (min(ts_list[[name]], na.rm = TRUE) > 0) {
        transformed_list[[name]] <- log(ts_list[[name]])
      } else {
        warning("Series '", name, "' contains non-positive values. Skipping log transformation.")
      }
    }
    
    # Transform factor series
    for (name in names(factor_ts_list)) {
      if (min(factor_ts_list[[name]], na.rm = TRUE) > 0) {
        transformed_factor_list[[name]] <- log(factor_ts_list[[name]])
      } else {
        warning("Factor series '", name, "' contains non-positive values. Skipping log transformation.")
      }
    }
  }
  
  # Align all time series to have the same start and end dates
  all_ts_list <- c(transformed_list, transformed_factor_list)
  aligned_all_list <- align_multiple_ts(all_ts_list)
  
  # Split back into main and factor series
  aligned_list <- aligned_all_list[names(transformed_list)]
  aligned_factor_list <- aligned_all_list[names(transformed_factor_list)]
  
  # Also align exogenous variables if provided
  if (!is.null(exog_ts_list)) {
    exog_aligned_list <- align_multiple_ts(exog_ts_list)
    # Further align exogenous with endogenous variables
    common_range <- find_common_time_range(
      c(time(aligned_list[[1]])[1], time(aligned_list[[1]])[length(aligned_list[[1]])]),
      c(time(exog_aligned_list[[1]])[1], time(exog_aligned_list[[1]])[length(exog_aligned_list[[1]])])
    )
    
    # Rewindow all series
    for (name in names(aligned_all_list)) {
      aligned_all_list[[name]] <- window(aligned_all_list[[name]], 
                                         start = common_range[1], 
                                         end = common_range[2])
    }
    
    for (name in names(exog_aligned_list)) {
      exog_aligned_list[[name]] <- window(exog_aligned_list[[name]], 
                                          start = common_range[1], 
                                          end = common_range[2])
    }
    
    # Update the split lists
    aligned_list <- aligned_all_list[names(transformed_list)]
    aligned_factor_list <- aligned_all_list[names(transformed_factor_list)]
  }
  
  # Convert lists of time series to matrices
  main_data <- ts_list_to_matrix(aligned_list)
  factor_data <- ts_list_to_matrix(aligned_factor_list)
  
  # Prepare exogenous data if provided
  exog_data <- NULL
  if (!is.null(exog_ts_list)) {
    exog_data <- ts_list_to_matrix(exog_aligned_list)
  }
  
  # Initialize lists to store models and forecasts
  models <- list()
  factor_models <- list()
  forecasts <- list()
  actual_values <- list()
  
  # Extract factors using principal component analysis
  message("Extracting factors using PCA...")
  pca_result <- tryCatch({
    # Standardize the data for PCA
    factor_data_scaled <- scale(factor_data)
    
    # Perform PCA
    prcomp(factor_data_scaled, center = TRUE, scale. = TRUE)
  }, error = function(e) {
    warning("Error in PCA: ", e$message)
    return(NULL)
  })
  
  if (is.null(pca_result)) {
    stop("Failed to extract factors using PCA")
  }
  
  # Extract the first n_factors principal components
  factor_scores <- pca_result$x[, 1:n_factors]
  
  # Combine the original series with the extracted factors
  favar_data <- cbind(main_data, factor_scores)
  colnames(favar_data) <- c(colnames(main_data), 
                            paste0("Factor", 1:n_factors))
  
  # Create ts object for the combined data
  favar_ts <- ts(favar_data, 
                 start = start(aligned_list[[1]]), 
                 frequency = frequency(aligned_list[[1]]))
  
  # Determine the best VAR lag order for FAVAR
  var_select <- tryCatch({
    # Use vars::VARselect to determine optimal lag order
    if (is.null(exog_data)) {
      vars::VARselect(favar_data, lag.max = max_lags, type = "const")
    } else {
      vars::VARselect(favar_data, lag.max = max_lags, type = "const", exogen = exog_data)
    }
  }, error = function(e) {
    warning("Error in VAR lag selection: ", e$message, ". Using lag 1.")
    list(selection = setNames(rep(1, 4), c("AIC", "HQ", "SC", "FPE")))
  })
  
  # Extract optimal lag order based on AIC
  optimal_lag <- var_select$selection["AIC"]
  message("Selected optimal lag order: ", optimal_lag, " based on AIC")
  
  # Define FAVAR model specifications
  model_specs <- list(
    # FAVAR with constant
    "favar_const" = function(data, p, exog = NULL) {
      if (is.null(exog)) {
        vars::VAR(data, p = p, type = "const")
      } else {
        vars::VAR(data, p = p, type = "const", exogen = exog)
      }
    },
    
    # FAVAR with trend
    "favar_trend" = function(data, p, exog = NULL) {
      if (is.null(exog)) {
        vars::VAR(data, p = p, type = "trend")
      } else {
        vars::VAR(data, p = p, type = "trend", exogen = exog)
      }
    },
    
    # FAVAR with constant and trend
    "favar_both" = function(data, p, exog = NULL) {
      if (is.null(exog)) {
        vars::VAR(data, p = p, type = "both")
      } else {
        vars::VAR(data, p = p, type = "both", exogen = exog)
      }
    }
  )
  
  # Fit each FAVAR model specification to the full dataset
  message("Fitting FAVAR models to full dataset...")
  for (name in names(model_specs)) {
    models[[name]] <- tryCatch({
      model <- model_specs[[name]](favar_data, p = optimal_lag, exog = exog_data)
      message("  Successfully fit model: ", name)
      model
    }, error = function(e) {
      warning("Error fitting model ", name, ": ", e$message)
      return(NULL)
    })
  }
  
  # Store factor extraction model
  factor_models$pca <- pca_result
  
  # Perform rolling window forecasts
  message("Performing rolling window forecasts...")
  
  # Determine number of observations
  n_obs <- nrow(favar_data)
  
  # Ensure there are enough observations for the rolling window
  if (n_obs <= rolling_window + optimal_lag) {
    stop("Not enough observations (", n_obs, ") for the specified rolling window size (", 
         rolling_window, ") and lag order (", optimal_lag, ")")
  }
  
  # Perform rolling window evaluation
  for (i in 0:(n_obs - rolling_window - forecast_horizon - optimal_lag)) {
    # Define training and test indices
    train_end <- rolling_window + i
    test_start <- train_end + 1
    test_end <- min(train_end + forecast_horizon, n_obs)
    
    if (test_end <= test_start) next
    
    message("  Window ", i+1, ": training up to index ", train_end, 
            ", testing from ", test_start, " to ", test_end)
    
    # Extract training data for main series and factor series
    train_main <- main_data[1:train_end, ]
    train_factor <- factor_data[1:train_end, ]
    
    # Extract factors from training data
    train_factor_scaled <- scale(train_factor)
    train_pca <- prcomp(train_factor_scaled, center = TRUE, scale. = TRUE)
    train_factor_scores <- train_pca$x[, 1:n_factors]
    
    # Combine main series with extracted factors
    train_favar <- cbind(train_main, train_factor_scores)
    colnames(train_favar) <- colnames(favar_data)
    
    # Extract exogenous training data if available
    train_exog <- NULL
    test_exog <- NULL
    if (!is.null(exog_data)) {
      train_exog <- exog_data[1:train_end, ]
      if (test_end <= nrow(exog_data)) {
        test_exog <- exog_data[test_start:test_end, ]
      }
    }
    
    # Store actual values for this window (only the main series, not factors)
    actual_values[[i+1]] <- main_data[test_start:test_end, ]
    
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
        model <- model_specs[[name]](train_favar, p = optimal_lag, exog = train_exog)
        
        # Generate forecasts
        h <- test_end - test_start + 1
        fc <- predict(model, n.ahead = h, dumvar = test_exog)
        
        # Store forecast (only for the main series, not factors)
        main_vars <- colnames(main_data)
        main_fc <- list()
        
        for (var in main_vars) {
          main_fc[[var]] <- fc$fcst[[var]][, "fcst"]
        }
        
        forecasts[[i+1]][[name]] <- main_fc
      }, error = function(e) {
        warning("Error forecasting with model ", name, " at window ", i+1, ": ", e$message)
      })
    }
  }
  
  # Calculate error metrics for each model, variable, and horizon
  message("Calculating error metrics...")
  error_metrics <- calculate_favar_forecast_errors(forecasts, actual_values, colnames(main_data))
  
  # Return the results
  return(list(
    original_series = ts_list,
    transformed_series = transformed_list,
    log_transform = log_transform,
    aligned_series = aligned_list,
    factor_series = aligned_factor_list,
    factors = factor_scores,
    n_factors = n_factors,
    factor_models = factor_models,
    models = models,
    optimal_lag = optimal_lag,
    forecasts = forecasts,
    actual_values = actual_values,
    error_metrics = error_metrics
  ))
}

#' Calculate error metrics for VAR forecasts
#'
#' @param forecasts List of VAR forecast objects
#' @param actual_values List of actual values matrices
#' @param variable_names Vector of variable names
#'
#' @return A nested list of error metrics by model, variable, horizon, and metric type
#' @export
calculate_var_forecast_errors <- function(forecasts, actual_values, variable_names) {
  # Initialize error metrics structure
  error_metrics <- list()
  
  # Get all model names from the first forecast window
  if (length(forecasts) == 0 || length(forecasts[[1]]) == 0) {
    warning("No forecasts available for error calculation")
    return(error_metrics)
  }
  
  model_names <- names(forecasts[[1]])
  
  # For each model
  for (model_name in model_names) {
    error_metrics[[model_name]] <- list()
    
    # For each variable
    for (var_name in variable_names) {
      error_metrics[[model_name]][[var_name]] <- list()
      
      # Find maximum forecast horizon
      max_horizon <- 0
      for (i in seq_along(forecasts)) {
        if (model_name %in% names(forecasts[[i]])) {
          fc <- forecasts[[i]][[model_name]]
          if (!is.null(fc) && !is.null(fc$fcst) && var_name %in% names(fc$fcst)) {
            # Check if fcst has rows (safe access)
            if (is.matrix(fc$fcst[[var_name]]) && nrow(fc$fcst[[var_name]]) > 0) {
              max_horizon <- max(max_horizon, nrow(fc$fcst[[var_name]]))
            }
          }
        }
      }
      
      # Skip if no valid horizons found
      if (max_horizon == 0) {
        message("No valid forecast horizons found for model ", model_name, ", variable ", var_name)
        next
      }
      
      # Initialize error accumulators for each horizon
      for (h in 1:max_horizon) {
        error_metrics[[model_name]][[var_name]][[h]] <- list(
          errors = numeric(),      # Raw errors (actual - forecast)
          abs_errors = numeric(),  # Absolute errors
          sq_errors = numeric(),   # Squared errors
          perc_errors = numeric()  # Percentage errors
        )
      }
      
      # Collect errors for each forecast window
      for (i in seq_along(forecasts)) {
        # Skip if out of bounds
        if (i > length(actual_values)) {
          message("Skipping window ", i, ": actual values not available")
          next
        }
        
        # Skip if model not in this window
        if (!(model_name %in% names(forecasts[[i]]))) next
        
        # Skip if variable not in actual values
        if (!(var_name %in% colnames(actual_values[[i]]))) {
          message("Variable ", var_name, " not found in actual values for window ", i)
          next
        }
        
        fc <- forecasts[[i]][[model_name]]
        
        # Skip if forecast structure invalid
        if (is.null(fc) || is.null(fc$fcst)) {
          message("Invalid forecast structure for model ", model_name, " in window ", i)
          next
        }
        
        # Skip if variable not in forecast
        if (!(var_name %in% names(fc$fcst))) {
          message("Variable ", var_name, " not found in forecasts for window ", i)
          next
        }
        
        # Get actual values and forecasts
        actual <- actual_values[[i]][, var_name]
        fc_values <- NULL
        
        # Safely extract forecast values
        tryCatch({
          fc_values <- fc$fcst[[var_name]][, "fcst"]
        }, error = function(e) {
          message("Error extracting forecast values for model ", model_name, 
                  ", variable ", var_name, " in window ", i, ": ", e$message)
        })
        
        # Skip if forecast extraction failed
        if (is.null(fc_values)) next
        
        # Calculate errors for each horizon (with bounds checking)
        for (h in 1:min(length(fc_values), length(actual), max_horizon)) {
          # Skip if either value is NA
          if (is.na(actual[h]) || is.na(fc_values[h])) {
            message("NA values found for horizon ", h, " in window ", i)
            next
          }
          
          # Raw error
          error <- actual[h] - fc_values[h]
          error_metrics[[model_name]][[var_name]][[h]]$errors <- 
            c(error_metrics[[model_name]][[var_name]][[h]]$errors, error)
          
          # Absolute error
          abs_error <- abs(error)
          error_metrics[[model_name]][[var_name]][[h]]$abs_errors <- 
            c(error_metrics[[model_name]][[var_name]][[h]]$abs_errors, abs_error)
          
          # Squared error
          sq_error <- error^2
          error_metrics[[model_name]][[var_name]][[h]]$sq_errors <- 
            c(error_metrics[[model_name]][[var_name]][[h]]$sq_errors, sq_error)
          
          # Percentage error (only if actual is not too close to zero)
          if (abs(actual[h]) > 1e-10) {
            perc_error <- 100 * error / actual[h]
            error_metrics[[model_name]][[var_name]][[h]]$perc_errors <- 
              c(error_metrics[[model_name]][[var_name]][[h]]$perc_errors, perc_error)
          }
        }
      }
      
      # Calculate summary metrics for each horizon
      for (h in 1:max_horizon) {
        # Check if we have any errors for this horizon
        if (length(error_metrics[[model_name]][[var_name]][[h]]$errors) == 0) {
          # Set all metrics to NA if no errors
          error_metrics[[model_name]][[var_name]][[h]]$MAE <- NA
          error_metrics[[model_name]][[var_name]][[h]]$RMSE <- NA
          error_metrics[[model_name]][[var_name]][[h]]$MSE <- NA
          error_metrics[[model_name]][[var_name]][[h]]$MAPE <- NA
          next
        }
        
        # Mean Absolute Error (MAE)
        error_metrics[[model_name]][[var_name]][[h]]$MAE <- 
          mean(error_metrics[[model_name]][[var_name]][[h]]$abs_errors, na.rm = TRUE)
        
        # Root Mean Squared Error (RMSE)
        error_metrics[[model_name]][[var_name]][[h]]$RMSE <- 
          sqrt(mean(error_metrics[[model_name]][[var_name]][[h]]$sq_errors, na.rm = TRUE))
        
        # Mean Squared Error (MSE)
        error_metrics[[model_name]][[var_name]][[h]]$MSE <- 
          mean(error_metrics[[model_name]][[var_name]][[h]]$sq_errors, na.rm = TRUE)
        
        # Mean Absolute Percentage Error (MAPE)
        if (length(error_metrics[[model_name]][[var_name]][[h]]$perc_errors) > 0) {
          error_metrics[[model_name]][[var_name]][[h]]$MAPE <- 
            mean(abs(error_metrics[[model_name]][[var_name]][[h]]$perc_errors), na.rm = TRUE)
        } else {
          error_metrics[[model_name]][[var_name]][[h]]$MAPE <- NA
        }
      }
    }
  }
  
  return(error_metrics)
}

#' Calculate error metrics for BVAR forecasts
#'
#' @param forecasts List of BVAR forecast matrices
#' @param actual_values List of actual values matrices
#' @param variable_names Vector of variable names
#'
#' @return A nested list of error metrics by model, variable, horizon, and metric type
#' @export
calculate_bvar_forecast_errors <- function(forecasts, actual_values, variable_names) {
  # Initialize error metrics structure
  error_metrics <- list()
  
  # Get all model names from the first forecast window
  if (length(forecasts) == 0 || length(forecasts[[1]]) == 0) {
    warning("No forecasts available for error calculation")
    return(error_metrics)
  }
  
  model_names <- names(forecasts[[1]])
  
  # For each model
  for (model_name in model_names) {
    error_metrics[[model_name]] <- list()
    
    # For each variable
    for (var_name in variable_names) {
      error_metrics[[model_name]][[var_name]] <- list()
      
      # Find maximum forecast horizon
      max_horizon <- 0
      for (i in seq_along(forecasts)) {
        if (model_name %in% names(forecasts[[i]])) {
          fc <- forecasts[[i]][[model_name]]
          if (var_name %in% colnames(fc)) {
            max_horizon <- max(max_horizon, nrow(fc))
          }
        }
      }
      
      # Initialize error accumulators for each horizon
      for (h in 1:max_horizon) {
        error_metrics[[model_name]][[var_name]][[h]] <- list(
          errors = numeric(),      # Raw errors (actual - forecast)
          abs_errors = numeric(),  # Absolute errors
          sq_errors = numeric(),   # Squared errors
          perc_errors = numeric()  # Percentage errors
        )
      }
      
      # Collect errors for each forecast window
      for (i in seq_along(forecasts)) {
        if (!(model_name %in% names(forecasts[[i]]))) next
        if (!(var_name %in% colnames(actual_values[[i]]))) next
        
        fc <- forecasts[[i]][[model_name]]
        actual <- actual_values[[i]][, var_name]
        
        if (!(var_name %in% colnames(fc))) next
        
        fc_values <- fc[, var_name]
        
        # Calculate errors for each horizon
        for (h in 1:min(length(fc_values), length(actual))) {
          # Raw error
          error <- actual[h] - fc_values[h]
          error_metrics[[model_name]][[var_name]][[h]]$errors <- 
            c(error_metrics[[model_name]][[var_name]][[h]]$errors, error)
          
          # Absolute error
          abs_error <- abs(error)
          error_metrics[[model_name]][[var_name]][[h]]$abs_errors <- 
            c(error_metrics[[model_name]][[var_name]][[h]]$abs_errors, abs_error)
          
          # Squared error
          sq_error <- error^2
          error_metrics[[model_name]][[var_name]][[h]]$sq_errors <- 
            c(error_metrics[[model_name]][[var_name]][[h]]$sq_errors, sq_error)
          
          # Percentage error (only if actual is not too close to zero)
          if (abs(actual[h]) > 1e-10) {
            perc_error <- 100 * error / actual[h]
            error_metrics[[model_name]][[var_name]][[h]]$perc_errors <- 
              c(error_metrics[[model_name]][[var_name]][[h]]$perc_errors, perc_error)
          }
        }
      }
      
      # Calculate summary metrics for each horizon
      for (h in 1:max_horizon) {
        # Mean Absolute Error (MAE)
        error_metrics[[model_name]][[var_name]][[h]]$MAE <- 
          mean(error_metrics[[model_name]][[var_name]][[h]]$abs_errors, na.rm = TRUE)
        
        # Root Mean Squared Error (RMSE)
        error_metrics[[model_name]][[var_name]][[h]]$RMSE <- 
          sqrt(mean(error_metrics[[model_name]][[var_name]][[h]]$sq_errors, na.rm = TRUE))
        
        # Mean Squared Error (MSE)
        error_metrics[[model_name]][[var_name]][[h]]$MSE <- 
          mean(error_metrics[[model_name]][[var_name]][[h]]$sq_errors, na.rm = TRUE)
        
        # Mean Absolute Percentage Error (MAPE)
        if (length(error_metrics[[model_name]][[var_name]][[h]]$perc_errors) > 0) {
          error_metrics[[model_name]][[var_name]][[h]]$MAPE <- 
            mean(abs(error_metrics[[model_name]][[var_name]][[h]]$perc_errors), na.rm = TRUE)
        } else {
          error_metrics[[model_name]][[var_name]][[h]]$MAPE <- NA
        }
      }
    }
  }
  
  return(error_metrics)
}

#' Calculate error metrics for FAVAR forecasts
#'
#' @param forecasts List of FAVAR forecast lists
#' @param actual_values List of actual values matrices
#' @param variable_names Vector of variable names
#'
#' @return A nested list of error metrics by model, variable, horizon, and metric type
#' @export
calculate_favar_forecast_errors <- function(forecasts, actual_values, variable_names) {
  # Initialize error metrics structure
  error_metrics <- list()
  
  # Get all model names from the first forecast window
  if (length(forecasts) == 0 || length(forecasts[[1]]) == 0) {
    warning("No forecasts available for error calculation")
    return(error_metrics)
  }
  
  model_names <- names(forecasts[[1]])
  
  # For each model
  for (model_name in model_names) {
    error_metrics[[model_name]] <- list()
    
    # For each variable
    for (var_name in variable_names) {
      error_metrics[[model_name]][[var_name]] <- list()
      
      # Find maximum forecast horizon
      max_horizon <- 0
      for (i in seq_along(forecasts)) {
        if (model_name %in% names(forecasts[[i]])) {
          fc <- forecasts[[i]][[model_name]]
          if (var_name %in% names(fc)) {
            max_horizon <- max(max_horizon, length(fc[[var_name]]))
          }
        }
      }
      
      # Initialize error accumulators for each horizon
      for (h in 1:max_horizon) {
        error_metrics[[model_name]][[var_name]][[h]] <- list(
          errors = numeric(),      # Raw errors (actual - forecast)
          abs_errors = numeric(),  # Absolute errors
          sq_errors = numeric(),   # Squared errors
          perc_errors = numeric()  # Percentage errors
        )
      }
      
      # Collect errors for each forecast window
      for (i in seq_along(forecasts)) {
        if (!(model_name %in% names(forecasts[[i]]))) next
        if (!(var_name %in% colnames(actual_values[[i]]))) next
        
        fc <- forecasts[[i]][[model_name]]
        actual <- actual_values[[i]][, var_name]
        
        if (!(var_name %in% names(fc))) next
        
        fc_values <- fc[[var_name]]
        
        # Calculate errors for each horizon
        for (h in 1:min(length(fc_values), length(actual))) {
          # Raw error
          error <- actual[h] - fc_values[h]
          error_metrics[[model_name]][[var_name]][[h]]$errors <- 
            c(error_metrics[[model_name]][[var_name]][[h]]$errors, error)
          
          # Absolute error
          abs_error <- abs(error)
          error_metrics[[model_name]][[var_name]][[h]]$abs_errors <- 
            c(error_metrics[[model_name]][[var_name]][[h]]$abs_errors, abs_error)
          
          # Squared error
          sq_error <- error^2
          error_metrics[[model_name]][[var_name]][[h]]$sq_errors <- 
            c(error_metrics[[model_name]][[var_name]][[h]]$sq_errors, sq_error)
          
          # Percentage error (only if actual is not too close to zero)
          if (abs(actual[h]) > 1e-10) {
            perc_error <- 100 * error / actual[h]
            error_metrics[[model_name]][[var_name]][[h]]$perc_errors <- 
              c(error_metrics[[model_name]][[var_name]][[h]]$perc_errors, perc_error)
          }
        }
      }
      
      # Calculate summary metrics for each horizon
      for (h in 1:max_horizon) {
        # Mean Absolute Error (MAE)
        error_metrics[[model_name]][[var_name]][[h]]$MAE <- 
          mean(error_metrics[[model_name]][[var_name]][[h]]$abs_errors, na.rm = TRUE)
        
        # Root Mean Squared Error (RMSE)
        error_metrics[[model_name]][[var_name]][[h]]$RMSE <- 
          sqrt(mean(error_metrics[[model_name]][[var_name]][[h]]$sq_errors, na.rm = TRUE))
        
        # Mean Squared Error (MSE)
        error_metrics[[model_name]][[var_name]][[h]]$MSE <- 
          mean(error_metrics[[model_name]][[var_name]][[h]]$sq_errors, na.rm = TRUE)
        
        # Mean Absolute Percentage Error (MAPE)
        if (length(error_metrics[[model_name]][[var_name]][[h]]$perc_errors) > 0) {
          error_metrics[[model_name]][[var_name]][[h]]$MAPE <- 
            mean(abs(error_metrics[[model_name]][[var_name]][[h]]$perc_errors), na.rm = TRUE)
        } else {
          error_metrics[[model_name]][[var_name]][[h]]$MAPE <- NA
        }
      }
    }
  }
  
  return(error_metrics)
}

#' Align multiple time series to have the same time window
#'
#' @param ts_list List of time series objects
#'
#' @return List of aligned time series objects
#' @export
align_multiple_ts <- function(ts_list) {
  # Find common time range for all series
  if (length(ts_list) < 1) {
    stop("Empty time series list")
  }
  
  # Initialize common range with the first series
  start_time <- time(ts_list[[1]])[1]
  end_time <- time(ts_list[[1]])[length(ts_list[[1]])]
  
  # Update common range with each series
  for (name in names(ts_list)) {
    series <- ts_list[[name]]
    start_time <- max(start_time, time(series)[1])
    end_time <- min(end_time, time(series)[length(series)])
  }
  
  # Check if there is a valid common range
  if (start_time > end_time) {
    stop("No common time range for all series")
  }
  
  # Align all series to the common range
  aligned_list <- list()
  for (name in names(ts_list)) {
    aligned_list[[name]] <- window(ts_list[[name]], start = start_time, end = end_time)
  }
  
  return(aligned_list)
}

#' Find common time range between two ranges
#'
#' @param range1 Vector with start and end times of first range
#' @param range2 Vector with start and end times of second range
#'
#' @return Vector with common start and end times
#' @export
find_common_time_range <- function(range1, range2) {
  common_start <- max(range1[1], range2[1])
  common_end <- min(range1[2], range2[2])
  
  if (common_start > common_end) {
    stop("No common time range")
  }
  
  return(c(common_start, common_end))
}

#' Convert a list of time series to a matrix
#'
#' @param ts_list List of time series objects
#'
#' @return Matrix with columns for each series
#' @export
ts_list_to_matrix <- function(ts_list) {
  # Ensure all series have the same length
  lengths <- sapply(ts_list, length)
  if (length(unique(lengths)) > 1) {
    stop("All time series must have the same length")
  }
  
  # Create a matrix
  matrix_data <- do.call(cbind, lapply(ts_list, as.numeric))
  colnames(matrix_data) <- names(ts_list)
  
  return(matrix_data)
}

#' Convert a list of time series to a data frame
#'
#' @param ts_list List of time series objects
#'
#' @return Data frame with columns for each series
#' @export
ts_list_to_df <- function(ts_list) {
  # Ensure all series have the same length
  lengths <- sapply(ts_list, length)
  if (length(unique(lengths)) > 1) {
    stop("All time series must have the same length")
  }
  
  # Get the time index from the first series
  time_index <- time(ts_list[[1]])
  
  # Create a data frame
  df <- data.frame(
    time = time_index,
    matrix(unlist(lapply(ts_list, as.numeric)), ncol = length(ts_list))
  )
  
  # Set column names
  colnames(df)[-1] <- names(ts_list)
  
  return(df)
}