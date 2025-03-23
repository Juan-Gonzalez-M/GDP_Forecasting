# main.R - Main script for GDP forecasting system
# This script orchestrates the entire GDP forecasting workflow

# Load configuration and source all component files
source("R/config.R")
source("R/data_import.R")
source("R/seasonal_adjustment.R")
source("R/univariate_models.R")
source("R/monthly_models.R")
source("R/multivariate_models.R")
source("R/model_combination.R")
source("R/visualization.R")
source("R/utils.R")

#' Run the GDP forecasting system
#'
#' @param data_file Path to the Excel data file
#' @param output_dir Directory to save output files
#' @param cache_dir Directory for caching intermediate results
#' @param refresh_cache Logical indicating whether to refresh cached results
#' @param log_file Path to log file (NULL for no logging)
#'
#' @return Invisibly returns a list with all results
#' @export
run_gdp_forecasting <- function(data_file = DATA_FILE,
                                output_dir = OUTPUT_DIR,
                                cache_dir = "cache",
                                refresh_cache = FALSE,
                                log_file = "gdp_forecast.log") {
  # Start logging
  if (!is.null(log_file)) {
    if (file.exists(log_file)) file.remove(log_file)
    log_message("Starting GDP forecasting system", file = log_file)
    log_message("Using data file: ", data_file, file = log_file)
  }
  
  # Create output directories
  create_directories()
  
  # Create cache directory
  create_cache_dir(cache_dir)
  
  # Check and install required packages
  check_and_install_packages()
  
  #------------------------------------------------------------------------------
  # Step 1: Import data from Excel
  #------------------------------------------------------------------------------
  log_message("Step 1: Importing data from Excel", file = log_file)
  
  # Try to load from cache
  data_cache_file <- "imported_data.rds"
  data_list <- NULL
  
  if (!refresh_cache) {
    data_list <- cache_load(data_cache_file, cache_dir)
  }
  
  if (is.null(data_list)) {
    # Import data
    data_list <- time_operation(
      import_data(data_file),
      "Data import"
    )
    
    # Save to cache
    cache_save(data_list, data_cache_file, cache_dir)
  }
  
  #------------------------------------------------------------------------------
  # Step 2: Prepare time series
  #------------------------------------------------------------------------------
  log_message("Step 2: Preparing time series", file = log_file)
  
  # Try to load from cache
  ts_cache_file <- "prepared_ts.rds"
  ts_list <- NULL
  
  if (!refresh_cache) {
    ts_list <- cache_load(ts_cache_file, cache_dir)
  }
  
  if (is.null(ts_list)) {
    # Prepare time series
    ts_list <- time_operation(
      prepare_time_series(data_list),
      "Time series preparation"
    )
    
    # Save to cache
    cache_save(ts_list, ts_cache_file, cache_dir)
  }
  
  #------------------------------------------------------------------------------
  # Step 3: Apply seasonal adjustment
  #------------------------------------------------------------------------------
  log_message("Step 3: Applying seasonal adjustment", file = log_file)
  
  # Try to load from cache
  sa_cache_file <- "seasonally_adjusted_ts.rds"
  sa_list <- NULL
  
  if (!refresh_cache) {
    sa_list <- cache_load(sa_cache_file, cache_dir)
  }
  
  if (is.null(sa_list)) {
    # Apply seasonal adjustment
    sa_list <- list()
    
    # Quarterly data
    sa_list$quarterly <- time_operation(
      apply_seasonal_adjustment_multiple(ts_list$quarterly),
      "Quarterly seasonal adjustment"
    )
    
    # Monthly data
    sa_list$monthly <- time_operation(
      apply_seasonal_adjustment_multiple(ts_list$monthly),
      "Monthly seasonal adjustment"
    )
    
    # Save to cache
    cache_save(sa_list, sa_cache_file, cache_dir)
  }
  
  #------------------------------------------------------------------------------
  # Step 4: Fit univariate models
  #------------------------------------------------------------------------------
  log_message("Step 4: Fitting univariate models", file = log_file)
  
  # Initialize results list
  results <- list()
  
  # Process each GDP component in the quarterly data
  for (series_name in names(sa_list$quarterly)) {
    log_message("Processing series: ", series_name, file = log_file)
    
    # Initialize results for this series
    results[[series_name]] <- list()
    
    # Try to load univariate results from cache
    uni_cache_file <- paste0("univariate_", gsub("[^a-zA-Z0-9]", "_", series_name), ".rds")
    univariate_results <- NULL
    
    if (!refresh_cache) {
      univariate_results <- cache_load(uni_cache_file, cache_dir)
    }
    
    if (is.null(univariate_results)) {
      # Fit univariate models
      univariate_results <- time_operation(
        fit_univariate_models(
          sa_list$quarterly[[series_name]],
          log_transform = LOG_TRANSFORM,
          rolling_window = ROLLING_WINDOW_SIZE,
          forecast_horizon = FORECAST_HORIZON
        ),
        paste("Univariate models for", series_name)
      )
      
      # Save to cache
      cache_save(univariate_results, uni_cache_file, cache_dir)
    }
    
    # Store results
    results[[series_name]]$univariate <- univariate_results
    
    #------------------------------------------------------------------------------
    # Step 5: Find matching monthly indicators
    #------------------------------------------------------------------------------
    log_message("Step 5: Finding matching monthly indicators for ", series_name, file = log_file)
    
    # Try to load monthly-quarterly mapping from cache
    map_cache_file <- paste0("mapping_", gsub("[^a-zA-Z0-9]", "_", series_name), ".rds")
    monthly_mapping <- NULL
    
    if (!refresh_cache) {
      monthly_mapping <- cache_load(map_cache_file, cache_dir)
    }
    
    if (is.null(monthly_mapping)) {
      # Create temporary list with just this quarterly series
      single_q_list <- list()
      single_q_list[[series_name]] <- sa_list$quarterly[[series_name]]
      
      # Find matching monthly indicators
      monthly_mapping <- time_operation(
        create_monthly_quarterly_mapping(
          sa_list$monthly,
          single_q_list,
          top_n = 3
        ),
        paste("Monthly indicator mapping for", series_name)
      )
      
      # Save to cache
      cache_save(monthly_mapping, map_cache_file, cache_dir)
    }
    
    # Store mapping
    results[[series_name]]$monthly_mapping <- monthly_mapping
    
    #------------------------------------------------------------------------------
    # Step 6: Fit models with monthly indicators
    #------------------------------------------------------------------------------
    log_message("Step 6: Fitting models with monthly indicators for ", series_name, file = log_file)
    
    # Get top monthly indicator
    if (nrow(monthly_mapping) > 0) {
      top_indicator <- monthly_mapping$monthly_indicator[1]
      
      # Try to load monthly results from cache
      monthly_cache_file <- paste0(
        "monthly_", 
        gsub("[^a-zA-Z0-9]", "_", series_name), 
        "_", 
        gsub("[^a-zA-Z0-9]", "_", top_indicator), 
        ".rds"
      )
      monthly_results <- NULL
      
      if (!refresh_cache) {
        monthly_results <- cache_load(monthly_cache_file, cache_dir)
      }
      
      if (is.null(monthly_results)) {
        # Fit models with monthly indicators
        monthly_results <- time_operation(
          fit_monthly_models(
            sa_list$monthly[[top_indicator]],
            sa_list$quarterly[[series_name]],
            log_transform = LOG_TRANSFORM,
            rolling_window = ROLLING_WINDOW_SIZE,
            forecast_horizon = FORECAST_HORIZON,
            aggregation_method = "average"
          ),
          paste("Monthly indicator models for", series_name, "using", top_indicator)
        )
        
        # Save to cache
        cache_save(monthly_results, monthly_cache_file, cache_dir)
      }
      
      # Store results
      results[[series_name]]$monthly <- monthly_results
    } else {
      log_message("No suitable monthly indicators found for ", series_name, file = log_file)
    }
    
    #------------------------------------------------------------------------------
    # Step 7: Fit multivariate models (if enough series available)
    #------------------------------------------------------------------------------
    if (length(sa_list$quarterly) >= 2) {
      log_message("Step 7: Fitting multivariate models including ", series_name, file = log_file)
      
      # Try to load VAR results from cache
      var_cache_file <- paste0("var_", gsub("[^a-zA-Z0-9]", "_", series_name), ".rds")
      var_results <- NULL
      
      if (!refresh_cache) {
        var_results <- cache_load(var_cache_file, cache_dir)
      }
      
      if (is.null(var_results)) {
        # Fit VAR models
        # Include this series and a few related ones
        related_series <- names(sa_list$quarterly)[1:min(5, length(sa_list$quarterly))]
        var_series <- unique(c(series_name, related_series))
        var_data <- sa_list$quarterly[var_series]
        
        var_results <- time_operation(
          fit_var_models(
            var_data,
            log_transform = LOG_TRANSFORM,
            rolling_window = ROLLING_WINDOW_SIZE,
            forecast_horizon = FORECAST_HORIZON,
            max_lags = VAR_MAX_LAGS,
            ic = VAR_IC
          ),
          paste("VAR models including", series_name)
        )
        
        # Save to cache
        cache_save(var_results, var_cache_file, cache_dir)
      }
      
      # Store results
      results[[series_name]]$multivariate <- var_results
    } else {
      log_message("Not enough series for multivariate models", file = log_file)
    }
    
    #------------------------------------------------------------------------------
    # Step 8: Combine forecasts
    #------------------------------------------------------------------------------
    log_message("Step 8: Combining forecasts for ", series_name, file = log_file)
    
    # Generate individual forecasts from each methodology
    
    # Univariate forecast
    uni_weights <- calculate_weights(results[[series_name]]$univariate$error_metrics)
    uni_models <- results[[series_name]]$univariate$models
    uni_forecast <- combine_weighted_forecasts(uni_models, uni_weights)
    
    # Monthly forecast (if available)
    monthly_forecast <- NULL
    monthly_weights <- NULL
    if (!is.null(results[[series_name]]$monthly)) {
      monthly_weights <- calculate_weights(results[[series_name]]$monthly$error_metrics)
      if (!is.null(monthly_weights)) {
        monthly_models <- results[[series_name]]$monthly$models
        monthly_forecast <- combine_weighted_forecasts(
          results[[series_name]]$monthly$quarterly_forecasts[[
            length(results[[series_name]]$monthly$quarterly_forecasts)
          ]],
          monthly_weights
        )
      }
    }
    
    # Multivariate forecast (if available)
    multivariate_forecast <- NULL
    multivariate_weights <- NULL
    if (!is.null(results[[series_name]]$multivariate)) {
      multivariate_weights <- calculate_weights(
        results[[series_name]]$multivariate$error_metrics,
        variable_name = series_name
      )
      if (!is.null(multivariate_weights)) {
        last_forecasts <- results[[series_name]]$multivariate$forecasts[[
          length(results[[series_name]]$multivariate$forecasts)
        ]]
        multivariate_forecast <- combine_weighted_forecasts(
          last_forecasts,
          multivariate_weights,
          variable_name = series_name
        )
      }
    }
    
    # Combine forecasts from different methodologies
    log_message("Creating final combined forecast for ", series_name, file = log_file)
    
    final_forecast <- combine_methodology_forecasts(
      univariate_results = results[[series_name]]$univariate,
      monthly_results = results[[series_name]]$monthly,
      multivariate_results = results[[series_name]]$multivariate,
      variable_name = series_name,
      horizon = FORECAST_HORIZON
    )
    
    # Convert to time series
    last_obs_time <- time(sa_list$quarterly[[series_name]])[length(sa_list$quarterly[[series_name]])]
    last_obs_year <- floor(last_obs_time)
    last_obs_quarter <- round((last_obs_time - last_obs_year) * 4) + 1
    
    # Move to next quarter
    next_quarter <- last_obs_quarter + 1
    next_year <- last_obs_year
    if (next_quarter > 4) {
      next_year <- next_year + 1
      next_quarter <- 1
    }
    
    final_forecast_ts <- forecast_to_ts(
      final_forecast,
      start_time = c(next_year, next_quarter),
      frequency = 4
    )
    
    # If log-transformed, convert back to original scale
    if (LOG_TRANSFORM) {
      final_forecast_ts <- exp_forecast(final_forecast_ts)
    }
    
    # Store final forecast
    results[[series_name]]$final_forecast <- final_forecast_ts
    results[[series_name]]$methodology_weights <- list(
      univariate = uni_weights,
      monthly = monthly_weights,
      multivariate = multivariate_weights
    )
    
    #------------------------------------------------------------------------------
    # Step 9: Generate visualizations and reports
    #------------------------------------------------------------------------------
    log_message("Step 9: Generating visualizations and reports for ", series_name, file = log_file)
    
    # Generate comprehensive forecast report
    report_file <- generate_forecast_report(
      series_name = series_name,
      historical_ts = sa_list$quarterly[[series_name]],
      forecast_ts = final_forecast_ts,
      weights = uni_weights,  # Using univariate weights for visualization
      error_metrics = results[[series_name]]$univariate$error_metrics,
      methodology_weights = c(
        univariate = 1/3,
        monthly = 1/3,
        multivariate = 1/3
      ),  # TODO: Calculate actual weights
      output_dir = file.path(output_dir, "reports"),
      log_transform = LOG_TRANSFORM
    )
    
    log_message("Generated report for ", series_name, ": ", report_file, file = log_file)
    
    # Create additional visualizations if needed
    # ... [add custom visualization code here]
  }
  
  #------------------------------------------------------------------------------
  # Step 10: Clean up and finalize
  #------------------------------------------------------------------------------
  log_message("Step 10: Finalizing", file = log_file)
  
  # Return results
  return(invisible(results))
}

# Run the forecasting system if script is run directly
if (!interactive()) {
  run_gdp_forecasting()
}