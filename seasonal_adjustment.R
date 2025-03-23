# seasonal_adjustment.R - Functions for seasonal adjustment
# This file contains functions to test for seasonality and apply seasonal adjustment

#' Test for seasonality in a time series
#'
#' @param ts_obj Time series object
#' @param significance Significance level for the test
#'
#' @return Logical indicating whether the series has significant seasonality
#' @export
test_seasonality <- function(ts_obj, significance = SEASONALITY_SIGNIFICANCE) {
  # Check if frequency is at least 2
  if (frequency(ts_obj) < 2) {
    return(FALSE)
  }
  
  # Multiple tests for seasonality
  has_seasonality <- FALSE
  
  # 1. Use seasonal::seas to test for identifiable seasonality
  tryCatch({
    # Check if the seasonal package is available
    if (requireNamespace("seasonal", quietly = TRUE)) {
      x13_result <- seasonal::seas(ts_obj, x11 = "", seats = NULL)
      
      # Try different methods to extract QS test results
      if (exists("summary", where = asNamespace("seasonal"))) {
        # Use the summary function directly from the namespace
        qstest <- get("summary", asNamespace("seasonal"))(x13_result)$qstest
        if (!is.null(qstest) && qstest$pvalue < significance) {
          has_seasonality <- TRUE
        }
      } else if (exists("series", where = asNamespace("seasonal"))) {
        # Try using series function
        qstest_data <- seasonal::series(x13_result, "qstest")
        if (!is.null(qstest_data) && length(qstest_data) >= 2) {
          qs_pvalue <- qstest_data[2]  # Assuming second element is p-value
          if (!is.null(qs_pvalue) && qs_pvalue < significance) {
            has_seasonality <- TRUE
          }
        }
      } else {
        # Fallback: use the captured output
        output <- capture.output(print(x13_result))
        qs_line <- grep("QS", output, value = TRUE)
        if (length(qs_line) > 0) {
          qs_parts <- strsplit(trimws(qs_line), "\\s+")[[1]]
          if (length(qs_parts) >= 2) {
            qs_pvalue <- as.numeric(tail(qs_parts, 1))
            if (!is.na(qs_pvalue) && qs_pvalue < significance) {
              has_seasonality <- TRUE
            }
          }
        }
      }
    }
  }, error = function(e) {
    warning("Error in X-13 seasonality test: ", e$message, 
            ". Trying alternative methods.")
  })
  
  # 2. Use forecast::nsdiffs as an alternative
  if (!has_seasonality) {
    tryCatch({
      if (frequency(ts_obj) > 1) {
        # OCSB test for seasonal unit roots
        seasonal_diffs <- forecast::nsdiffs(ts_obj, test = "ocsb", max.D = 1)
        if (seasonal_diffs > 0) {
          has_seasonality <- TRUE
        }
        
        # CH test as an alternative
        if (!has_seasonality) {
          seasonal_diffs <- forecast::nsdiffs(ts_obj, test = "ch", max.D = 1)
          if (seasonal_diffs > 0) {
            has_seasonality <- TRUE
          }
        }
      }
    }, error = function(e) {
      warning("Error in nsdiffs test: ", e$message)
    })
  }
  
  # 3. Check for significant seasonal dummies as a last resort
  if (!has_seasonality) {
    tryCatch({
      if (frequency(ts_obj) > 1) {
        # Create seasonal dummies
        n <- length(ts_obj)
        seas <- as.factor(cycle(ts_obj))
        time_index <- 1:n
        
        # Fit a linear model with seasonal dummies
        df <- data.frame(y = as.numeric(ts_obj), time = time_index, seas = seas)
        model <- lm(y ~ time + seas, data = df)
        
        # Check if seasonal dummies are significant
        anova_result <- anova(model)
        if ("seas" %in% rownames(anova_result) && anova_result["seas", "Pr(>F)"] < significance) {
          has_seasonality <- TRUE
        }
      }
    }, error = function(e) {
      warning("Error in seasonal dummy test: ", e$message)
    })
  }
  
  return(has_seasonality)
}

#' Apply X-13-ARIMA-SEATS seasonal adjustment to a time series
#'
#' @param ts_obj Time series object
#' @param force Logical indicating whether to force seasonal adjustment
#'
#' @return Seasonally adjusted time series
#' @export
apply_seasonal_adjustment <- function(ts_obj, force = FALSE) {
  # Series name (for logging)
  series_name <- deparse(substitute(ts_obj))
  
  # Check if seasonality is present
  has_seasonality <- test_seasonality(ts_obj)
  
  if (has_seasonality || force) {
    message(ifelse(force, "Forcing seasonal adjustment for ", 
                   "Detected seasonality in "), series_name)
    
    # Check for enough observations
    min_obs <- max(2 * frequency(ts_obj), 6)
    if (length(ts_obj) < min_obs) {
      warning("Not enough observations for reliable seasonal adjustment in ", 
              series_name, ". Returning original series.")
      return(ts_obj)
    }
    
    # Apply X-13-ARIMA-SEATS seasonal adjustment
    tryCatch({
      x13_obj <- seasonal::seas(ts_obj, 
                                transform.function = "auto",  # Auto-select transformation
                                regression.aictest = NULL,   # No automatic outlier detection
                                outlier = NULL,              # No automatic outlier detection
                                arima.model = NULL,          # Auto-select ARIMA model
                                regression.variables = NULL) # No specific regression vars
      
      # Extract the seasonally adjusted series
      adjusted_ts <- seasonal::final(x13_obj)
      
      # Ensure the adjusted series has the same properties as the original
      adjusted_ts <- ts(as.numeric(adjusted_ts), 
                        start = start(ts_obj), 
                        frequency = frequency(ts_obj))
      
      # Log results
      message("Successfully applied X-13 seasonal adjustment to ", series_name)
      
      return(adjusted_ts)
    }, error = function(e) {
      warning("Error in X-13 seasonal adjustment for ", series_name, ": ", e$message,
              ". Falling back to STL decomposition.")
      
      # Fall back to STL decomposition
      tryCatch({
        stl_obj <- stats::stl(ts_obj, s.window = "periodic", robust = TRUE)
        adjusted_ts <- ts_obj - stl_obj$time.series[, "seasonal"]
        
        # Ensure the adjusted series has the same properties as the original
        adjusted_ts <- ts(as.numeric(adjusted_ts), 
                          start = start(ts_obj), 
                          frequency = frequency(ts_obj))
        
        message("Successfully applied STL seasonal adjustment to ", series_name)
        
        return(adjusted_ts)
      }, error = function(e2) {
        warning("Error in STL seasonal adjustment for ", series_name, ": ", e2$message,
                ". Returning original series.")
        return(ts_obj)
      })
    })
  } else {
    message("No significant seasonality detected in ", series_name, 
            ". Returning original series.")
    return(ts_obj)
  }
}

#' Apply seasonal adjustment to multiple time series
#'
#' @param ts_list List of time series objects
#' @param force Logical indicating whether to force seasonal adjustment
#'
#' @return List of seasonally adjusted time series
#' @export
apply_seasonal_adjustment_multiple <- function(ts_list, force = FALSE) {
  message("Applying seasonal adjustment to multiple series...")
  
  # Create a list to store adjusted time series
  adjusted_list <- list()
  
  # Apply seasonal adjustment to each time series
  for (name in names(ts_list)) {
    message("Processing series: ", name)
    adjusted_list[[name]] <- apply_seasonal_adjustment(ts_list[[name]], force = force)
  }
  
  message("Completed seasonal adjustment for ", length(adjusted_list), " series.")
  return(adjusted_list)
}

#' Plot comparison of original and seasonally adjusted series
#'
#' @param original Original time series
#' @param adjusted Seasonally adjusted time series
#' @param title Plot title
#'
#' @return ggplot object
#' @export
plot_seasonal_adjustment <- function(original, adjusted, title = "Seasonal Adjustment Comparison") {
  # Combine the series into a single data frame
  df_original <- data.frame(
    date = as.Date(zoo::index(zoo::as.zoo(original))),
    value = as.numeric(original),
    series = "Original"
  )
  
  df_adjusted <- data.frame(
    date = as.Date(zoo::index(zoo::as.zoo(adjusted))),
    value = as.numeric(adjusted),
    series = "Seasonally Adjusted"
  )
  
  df_combined <- rbind(df_original, df_adjusted)
  
  # Create the plot
  plot <- ggplot2::ggplot(df_combined, ggplot2::aes(x = date, y = value, color = series)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = title, x = "Date", y = "Value") +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c("Original" = "blue", "Seasonally Adjusted" = "red")) +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  
  return(plot)
}