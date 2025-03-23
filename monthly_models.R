# monthly_models.R - Functions for handling monthly indicators
# This file contains functions to use monthly data for quarterly forecasting

#' Aggregate monthly data to quarterly frequency
#'
#' @param monthly_ts Monthly time series object
#' @param aggregation_method Method used for aggregation ("average", "sum", "last", "first")
#'
#' @return Quarterly time series object
#' @export
aggregate_to_quarterly <- function(monthly_ts, aggregation_method = "average") {
  # Validate input
  if (frequency(monthly_ts) != 12) {
    warning("Input time series does not have monthly frequency (frequency = 12)")
  }
  
  # Determine appropriate aggregation function
  agg_func <- switch(aggregation_method,
                     "average" = mean,
                     "sum" = sum,
                     "last" = function(x) tail(x, 1),
                     "first" = function(x) head(x, 1),
                     mean)  # Default to average if method not recognized
  
  # Convert to ts object if it's not already
  if (!is.ts(monthly_ts)) {
    monthly_ts <- ts(as.numeric(monthly_ts), frequency = 12)
  }
  
  # Extract start year and month
  start_year <- start(monthly_ts)[1]
  start_month <- start(monthly_ts)[2]
  
  # Calculate the quarter of the start month
  start_quarter <- ceiling(start_month / 3)
  
  # Adjust start date to align with quarters
  adjusted_monthly_ts <- window(monthly_ts, 
                                start = c(start_year, start_month),
                                frequency = 12)
  
  # Convert to zoo for easier aggregation
  monthly_zoo <- zoo::as.zoo(adjusted_monthly_ts)
  
  # Create yearqtr index for quarterly aggregation
  index_yearqtr <- zoo::as.yearqtr(time(adjusted_monthly_ts))
  
  # Aggregate to quarterly - using proper namespace
  # This is the line that needs fixing
  quarterly_zoo <- zoo::aggregate.zoo(monthly_zoo, index_yearqtr, agg_func)
  
  # Convert back to ts
  quarterly_ts <- ts(as.numeric(quarterly_zoo), 
                     start = c(start_year, start_quarter), 
                     frequency = 4)
  
  return(quarterly_ts)
}

#' Fit and evaluate models on monthly data for quarterly forecasting
#'
#' @param monthly_ts Monthly time series object
#' @param quarterly_ts Reference quarterly time series for comparison
#' @param log_transform Logical indicating whether to apply log transformation
#' @param rolling_window Size of the rolling window for evaluation
#' @param forecast_horizon Number of quarters to forecast ahead
#' @param aggregation_method Method used to aggregate monthly to quarterly ("average", "sum", "last", "first")
#'
#' @return A list containing fitted models and evaluation results
#' @export
fit_monthly_models <- function(monthly_ts, quarterly_ts, log_transform = LOG_TRANSFORM, 
                               rolling_window = ROLLING_WINDOW_SIZE, 
                               forecast_horizon = FORECAST_HORIZON,
                               aggregation_method = "average") {
  # Series names (for logging)
  monthly_name <- deparse(substitute(monthly_ts))
  quarterly_name <- deparse(substitute(quarterly_ts))
  
  message("Fitting monthly models for ", monthly_name, " to forecast ", quarterly_name)
  
  # Apply log transformation if specified
  if (log_transform && min(monthly_ts, na.rm = TRUE) > 0 && min(quarterly_ts, na.rm = TRUE) > 0) {
    message("Applying log transformation")
    monthly_transformed <- log(monthly_ts)
    quarterly_transformed <- log(quarterly_ts)
  } else if (log_transform && (min(monthly_ts, na.rm = TRUE) <= 0 || min(quarterly_ts, na.rm = TRUE) <= 0)) {
    warning("Log transformation requested but series contains non-positive values. Skipping transformation.")
    monthly_transformed <- monthly_ts
    quarterly_transformed <- quarterly_ts
    log_transform <- FALSE
  } else {
    monthly_transformed <- monthly_ts
    quarterly_transformed <- quarterly_ts
  }
  
  # Determine the number of observations
  n_monthly <- length(monthly_transformed)
  n_quarterly <- length(quarterly_transformed)
  
  # Check for sufficient data
  if (n_monthly < 3 * rolling_window) {
    warning("Monthly series has fewer observations (", n_monthly, 
            ") than recommended (", 3 * rolling_window, ")")
  }
  
  if (n_quarterly <= rolling_window) {
    stop("Not enough quarterly observations (", n_quarterly, 
         ") for the specified rolling window size (", rolling_window, ")")
  }
  
  # Initialize lists to store models and forecasts
  models <- list()
  forecasts <- list()
  quarterly_forecasts <- list()
  
  # Define model types and specifications for monthly data
  model_specs <- list(
    # ARIMA models with different information criteria
    "monthly_arima_aic" = function(x) forecast::auto.arima(x, 
                                                           ic = "aic", 
                                                           seasonal = FALSE,
                                                           max.p = ARIMA_MAX_P,
                                                           max.q = ARIMA_MAX_Q,
                                                           max.d = ARIMA_MAX_D,
                                                           stepwise = TRUE,
                                                           approximation = (length(x) > 100)),
    
    "monthly_arima_bic" = function(x) forecast::auto.arima(x, 
                                                           ic = "bic", 
                                                           seasonal = FALSE,
                                                           max.p = ARIMA_MAX_P,
                                                           max.q = ARIMA_MAX_Q,
                                                           max.d = ARIMA_MAX_D,
                                                           stepwise = TRUE,
                                                           approximation = (length(x) > 100)),
    
    "monthly_arima_aicc" = function(x) forecast::auto.arima(x, 
                                                            ic = "aicc", 
                                                            seasonal = FALSE,
                                                            max.p = ARIMA_MAX_P,
                                                            max.q = ARIMA_MAX_Q,
                                                            max.d = ARIMA_MAX_D,
                                                            stepwise = TRUE,
                                                            approximation = (length(x) > 100)),
    
    # SARIMA models with different information criteria
    "monthly_sarima_aic" = function(x) forecast::auto.arima(x, 
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
    
    "monthly_sarima_bic" = function(x) forecast::auto.arima(x, 
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
    
    "monthly_sarima_aicc" = function(x) forecast::auto.arima(x, 
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
    "monthly_ets_aic" = function(x) forecast::ets(x, 
                                                  ic = "aic", 
                                                  opt.crit = "lik",
                                                  allow.multiplicative.trend = TRUE),
    
    "monthly_ets_bic" = function(x) forecast::ets(x, 
                                                  ic = "bic", 
                                                  opt.crit = "lik",
                                                  allow.multiplicative.trend = TRUE),
    
    "monthly_ets_aicc" = function(x) forecast::ets(x, 
                                                   ic = "aicc", 
                                                   opt.crit = "lik",
                                                   allow.multiplicative.trend = TRUE),
    
    # Neural Network Autoregression
    "monthly_nnetar" = function(x) forecast::nnetar(x, 
                                                    p = min(max(frequency(x), 1), 12),
                                                    size = min(max(floor(length(x)/10), 1), 10),
                                                    repeats = 20)
  )
  
  # Fit each model to the entire monthly series
  message("Fitting models to full monthly series...")
  for (name in names(model_specs)) {
    models[[name]] <- tryCatch({
      model <- model_specs[[name]](monthly_transformed)
      message("  Successfully fit model: ", name)
      model
    }, error = function(e) {
      warning("Error fitting model ", name, ": ", e$message)
      return(NULL)
    })
  }
  
  # Helper function to forecast monthly data and aggregate to quarterly
  forecast_and_aggregate <- function(model, h_monthly, start_year, start_month, aggregation_method) {
    # Generate monthly forecasts
    h_monthly <- max(h_monthly, 3) # Ensure at least one full quarter
    monthly_fc <- forecast::forecast(model, h = h_monthly)
    
    # Create a time series from the forecast
    monthly_fc_ts <- ts(as.numeric(monthly_fc$mean), 
                        start = c(start_year, start_month), 
                        frequency = 12)
    
    # Aggregate to quarterly
    quarterly_fc_ts <- aggregate_to_quarterly(monthly_fc_ts, aggregation_method)
    
    return(quarterly_fc_ts)
  }
  
  # Determine alignment between quarterly and monthly series
  # This assumes that the quarterly series starts at the same point or after the monthly series
  q_start_year <- start(quarterly_transformed)[1]
  q_start_quarter <- start(quarterly_transformed)[2]
  m_start_year <- start(monthly_transformed)[1]
  m_start_month <- start(monthly_transformed)[2]
  
  # Align quarterly and monthly time indices
  q_time_points <- time(quarterly_transformed)
  m_time_points <- time(monthly_transformed)
  
  # Convert monthly time points to quarterly (using ceiling division by 3)
  m_quarter_points <- floor(m_start_year + (m_start_month - 1) / 3) + 
    (ceiling((m_start_month - 1) %% 3 + 1) / 4)
  
  # Perform rolling window forecasts
  message("Performing rolling window forecasts with monthly data...")
  monthly_forecasts <- list()
  quarterly_forecasts <- list()
  actual_quarterly_values <- list()
  
  for (i in 0:(n_quarterly - rolling_window - forecast_horizon)) {
    # Define training and test periods for quarterly data
    q_train_end_idx <- rolling_window + i
    q_test_start_idx <- q_train_end_idx + 1
    q_test_end_idx <- min(q_train_end_idx + forecast_horizon, n_quarterly)
    
    if (q_test_end_idx <= q_test_start_idx) next
    
    # Get corresponding time points
    q_train_end_time <- q_time_points[q_train_end_idx]
    q_test_start_time <- q_time_points[q_test_start_idx]
    q_test_end_time <- q_time_points[q_test_end_idx]
    
    message("  Window ", i+1, ": quarterly training up to ", q_train_end_time, 
            ", testing from ", q_test_start_time, " to ", q_test_end_time)
    
    # Find corresponding monthly data cutoff point
    # Convert quarterly time to year and quarter
    q_train_end_year <- floor(q_train_end_time)
    q_train_end_quarter <- round((q_train_end_time - q_train_end_year) * 4) + 1
    
    # Convert to monthly cutoff (last month of the quarter)
    m_train_end_year <- q_train_end_year
    m_train_end_month <- q_train_end_quarter * 3
    
    # Find the index in the monthly series
    m_time_diff <- (m_train_end_year - m_start_year) * 12 + 
      (m_train_end_month - m_start_month)
    m_train_end_idx <- min(m_time_diff + 1, n_monthly)
    
    # Extract monthly training data
    m_train_data <- window(monthly_transformed, end = m_time_points[m_train_end_idx])
    
    # Store actual quarterly values for this window
    actual_quarterly_values[[i+1]] <- window(quarterly_transformed, 
                                             start = q_test_start_time, 
                                             end = q_test_end_time)
    
    # Initialize lists for this window's forecasts
    monthly_forecasts[[i+1]] <- list()
    quarterly_forecasts[[i+1]] <- list()
    
    # Calculate how many months we need to forecast to cover the quarterly forecast horizon
    months_to_forecast <- (q_test_end_idx - q_test_start_idx + 1) * 3
    
    # Fit models and generate forecasts
    for (name in names(model_specs)) {
      # Skip if the model could not be fitted to the entire series
      if (is.null(models[[name]])) {
        next
      }
      
      tryCatch({
        # Fit the model to the monthly training data
        model <- model_specs[[name]](m_train_data)
        
        # Generate monthly forecasts and aggregate to quarterly
        quarterly_fc <- forecast_and_aggregate(model, 
                                               h_monthly = months_to_forecast,
                                               start_year = m_train_end_year,
                                               start_month = m_train_end_month + 1,
                                               aggregation_method = aggregation_method)
        
        # Store the quarterly forecast
        quarterly_forecasts[[i+1]][[name]] <- quarterly_fc
      }, error = function(e) {
        warning("Error forecasting with model ", name, " at window ", i+1, ": ", e$message)
      })
    }
  }
  
  # Calculate error metrics for quarterly forecasts
  message("Calculating error metrics for quarterly forecasts derived from monthly models...")
  
  # Convert quarterly_forecasts and actual_quarterly_values to the format expected by calculate_forecast_errors
  forecast_objects <- list()
  
  for (i in seq_along(quarterly_forecasts)) {
    forecast_objects[[i]] <- list()
    for (name in names(quarterly_forecasts[[i]])) {
      # Create a simple forecast object
      fc_obj <- list(
        mean = quarterly_forecasts[[i]][[name]],
        method = name
      )
      class(fc_obj) <- "forecast"
      
      forecast_objects[[i]][[name]] <- fc_obj
    }
  }
  
  error_metrics <- calculate_forecast_errors(forecast_objects, actual_quarterly_values, log_transform)
  
  # Return the results
  return(list(
    monthly_series = monthly_ts,
    quarterly_series = quarterly_ts,
    transformed_monthly = monthly_transformed,
    transformed_quarterly = quarterly_transformed,
    log_transform = log_transform,
    models = models,
    quarterly_forecasts = quarterly_forecasts,
    actual_quarterly_values = actual_quarterly_values,
    error_metrics = error_metrics
  ))
}

#' Create a mapping between monthly and quarterly series based on correlations
#'
#' @param monthly_ts_list List of monthly time series
#' @param quarterly_ts_list List of quarterly time series
#' @param top_n Number of top correlations to keep for each quarterly series
#'
#' @return A data frame mapping quarterly series to their most correlated monthly indicators
#' @export
create_monthly_quarterly_mapping <- function(monthly_ts_list, quarterly_ts_list, top_n = 3) {
  # Initialize a list to store correlations
  correlations <- list()
  
  # Loop through each quarterly series
  for (q_name in names(quarterly_ts_list)) {
    q_series <- quarterly_ts_list[[q_name]]
    correlations[[q_name]] <- numeric()
    
    # Loop through each monthly series
    for (m_name in names(monthly_ts_list)) {
      m_series <- monthly_ts_list[[m_name]]
      
      # Aggregate monthly to quarterly
      m_quarterly <- aggregate_to_quarterly(m_series)
      
      # Align the series
      aligned <- window_align(m_quarterly, q_series)
      
      # Calculate correlation
      cor_val <- cor(aligned$x, aligned$y, use = "pairwise.complete.obs")
      
      correlations[[q_name]] <- c(correlations[[q_name]], setNames(cor_val, m_name))
    }
    
    # Sort correlations by absolute value
    correlations[[q_name]] <- correlations[[q_name]][order(abs(correlations[[q_name]]), 
                                                           decreasing = TRUE)]
  }
  
  # Create a data frame with the top_n correlations for each quarterly series
  mapping <- data.frame(
    quarterly_series = character(),
    monthly_indicator = character(),
    correlation = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (q_name in names(correlations)) {
    # Get top_n correlations
    top_correlations <- head(correlations[[q_name]], top_n)
    
    for (m_name in names(top_correlations)) {
      mapping <- rbind(mapping, data.frame(
        quarterly_series = q_name,
        monthly_indicator = m_name,
        correlation = top_correlations[[m_name]],
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(mapping)
}

#' Align two time series to have the same time window
#'
#' @param x First time series
#' @param y Second time series
#'
#' @return A list with aligned time series
#' @export
window_align <- function(x, y) {
  # Determine common time window
  start_x <- time(x)[1]
  end_x <- time(x)[length(x)]
  start_y <- time(y)[1]
  end_y <- time(y)[length(y)]
  
  common_start <- max(start_x, start_y)
  common_end <- min(end_x, end_y)
  
  # Check if there is overlap
  if (common_start > common_end) {
    stop("No overlap between time series")
  }
  
  # Window both series to the common time frame
  x_aligned <- window(x, start = common_start, end = common_end)
  y_aligned <- window(y, start = common_start, end = common_end)
  
  return(list(x = x_aligned, y = y_aligned))
}

#' Plot comparison of actual quarterly values vs. aggregated monthly forecasts
#'
#' @param quarterly_ts Quarterly time series
#' @param forecasts List of aggregated monthly forecasts
#' @param title Plot title
#'
#' @return ggplot object
#' @export
plot_monthly_quarterly_forecasts <- function(quarterly_ts, forecasts, title = "Monthly to Quarterly Forecast Comparison") {
  # Create a data frame for the original quarterly series
  df_quarterly <- data.frame(
    date = zoo::index(zoo::as.zoo(quarterly_ts)),
    value = as.numeric(quarterly_ts),
    series = "Actual Quarterly"
  )
  
  # Create a list to store forecast data frames
  df_forecasts <- list()
  
  # Process each forecast
  for (name in names(forecasts)) {
    # Extract the forecast
    fc <- forecasts[[name]]
    
    # Create a data frame for this forecast
    df_forecasts[[name]] <- data.frame(
      date = zoo::index(zoo::as.zoo(fc)),
      value = as.numeric(fc),
      series = paste("Monthly Aggregated:", name)
    )
  }
  
  # Combine all data frames
  df_combined <- rbind(df_quarterly, do.call(rbind, df_forecasts))
  
  # Create the plot
  plot <- ggplot2::ggplot(df_combined, ggplot2::aes(x = date, y = value, color = series)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = title, x = "Date", y = "Value") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  
  return(plot)
}