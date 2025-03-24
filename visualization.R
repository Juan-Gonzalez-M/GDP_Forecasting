# visualization.R - Functions for creating visualizations
# This file contains functions for creating plots and figures

#' Ensure directories exist, creating them if necessary
#'
#' @param dirs Vector of directory paths to check/create
#'
#' @return Invisibly returns TRUE if all directories exist or were created
#' @export
ensure_directories <- function(dirs) {
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      message("Created directory: ", dir)
    }
  }
  return(invisible(TRUE))
}



#' Create a time series plot with historical data and forecasts
#'
#' @param historical_ts Time series of historical data
#' @param forecast_ts Time series of forecast values
#' @param title Plot title
#' @param ylab Y-axis label
#' @param historical_label Label for historical series
#' @param forecast_label Label for forecast series
#' @param uncertainty Logical indicating whether to include uncertainty bands
#' @param lower_ts Optional lower bound of forecast confidence interval
#' @param upper_ts Optional upper bound of forecast confidence interval
#'
#' @return ggplot object
#' @export
plot_forecast <- function(historical_ts, forecast_ts, title = "Forecast", 
                          ylab = "Value", historical_label = "Historical", 
                          forecast_label = "Forecast", uncertainty = FALSE,
                          lower_ts = NULL, upper_ts = NULL) {
  
  # Validate inputs
  if (is.null(historical_ts) || length(historical_ts) == 0) {
    warning("Invalid historical time series provided.")
    return(ggplot2::ggplot() + 
             ggplot2::annotate("text", x = 0, y = 0, label = "Invalid historical data") + 
             ggplot2::theme_minimal() + 
             ggplot2::labs(title = title))
  }
  
  # Safe conversion to data frames
  df_historical <- tryCatch({
    data.frame(
      date = zoo::index(zoo::as.zoo(historical_ts)),
      value = as.numeric(historical_ts),
      series = historical_label
    )
  }, error = function(e) {
    warning("Error creating zoo object from historical series: ", e$message)
    # Fallback to simpler method
    data.frame(
      date = seq(from = as.numeric(start(historical_ts)[1]), 
                 by = 1/frequency(historical_ts), 
                 length.out = length(historical_ts)),
      value = as.numeric(historical_ts),
      series = historical_label
    )
  })
  
  # Create base plot with historical data
  p <- ggplot2::ggplot(df_historical, ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_line(color = "steelblue", linewidth = 1) +
    ggplot2::labs(title = title, x = "Date", y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10)
    )
  
  # Add forecast if valid
  if (!is.null(forecast_ts) && length(forecast_ts) > 0) {
    df_forecast <- tryCatch({
      data.frame(
        date = zoo::index(zoo::as.zoo(forecast_ts)),
        value = as.numeric(forecast_ts),
        series = forecast_label
      )
    }, error = function(e) {
      warning("Error creating zoo object from forecast series: ", e$message)
      # Create continuation from historical end
      last_date <- max(df_historical$date)
      freq <- frequency(historical_ts)
      date_seq <- seq(from = last_date + 1/freq, by = 1/freq, length.out = length(forecast_ts))
      
      data.frame(
        date = date_seq,
        value = as.numeric(forecast_ts),
        series = forecast_label
      )
    })
    
    # Add forecast line
    if (nrow(df_forecast) > 0) {
      p <- p + ggplot2::geom_line(data = df_forecast, 
                                  ggplot2::aes(x = date, y = value), 
                                  color = "tomato", linewidth = 1, linetype = "dashed")
      
      # Add a vertical line at the start of the forecast
      forecast_start <- min(df_forecast$date)
      p <- p + ggplot2::geom_vline(
        xintercept = as.numeric(forecast_start),
        linetype = "dotted", 
        color = "gray50"
      )
      
      # Add a legend
      p <- p + ggplot2::annotate(
        "text", 
        x = min(df_historical$date) + 0.8 * (max(df_forecast$date) - min(df_historical$date)),
        y = min(c(df_historical$value, df_forecast$value)) + 
          0.1 * (max(c(df_historical$value, df_forecast$value)) - 
                   min(c(df_historical$value, df_forecast$value))),
        label = paste("— ", historical_label, "   --- ", forecast_label),
        hjust = 0
      )
    }
  }
  
  return(p)
}

#' Create a plot comparing multiple forecasts
#'
#' @param historical_ts Time series of historical data
#' @param forecast_list List of forecast time series
#' @param title Plot title
#' @param ylab Y-axis label
#' @param historical_label Label for historical series
#'
#' @return ggplot object
#' @export
plot_multiple_forecasts <- function(historical_ts, forecast_list, title = "Forecast Comparison", 
                                    ylab = "Value", historical_label = "Historical") {
  if (length(forecast_list) == 0) {
    stop("Empty forecast list")
  }
  
  # Convert historical time series to data frame
  df_historical <- data.frame(
    date = zoo::index(zoo::as.zoo(historical_ts)),
    value = as.numeric(historical_ts),
    series = historical_label
  )
  
  # Convert forecast time series to data frames
  df_forecasts <- data.frame()
  for (name in names(forecast_list)) {
    df_forecast <- data.frame(
      date = zoo::index(zoo::as.zoo(forecast_list[[name]])),
      value = as.numeric(forecast_list[[name]]),
      series = name
    )
    df_forecasts <- rbind(df_forecasts, df_forecast)
  }
  
  # Combine all data frames
  df <- rbind(df_historical, df_forecasts)
  
  # Create plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = value, color = series, linetype = series)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(title = title, x = "Date", y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::scale_linetype_manual(
      values = c("solid", rep("dashed", length(forecast_list)))
    )
  
  # Add a vertical line at the start of the forecasts
  forecast_start <- min(df_forecasts$date)
  p <- p + ggplot2::geom_vline(
    xintercept = as.numeric(forecast_start),
    linetype = "dotted", 
    color = "gray50"
  )
  
  return(p)
}

#' Plot year-over-year growth rates
#'
#' @param historical_ts Time series of historical data
#' @param forecast_ts Time series of forecast values
#' @param title Plot title
#' @param ylab Y-axis label
#' @param log_transform Logical indicating whether the series are in log scale
#'
#' @return ggplot object
#' @export
plot_yoy_growth <- function(historical_ts, forecast_ts, title = "Year-over-Year Growth", 
                            ylab = "Percent Change (%)", log_transform = FALSE) {
  # Validate inputs
  if (is.null(historical_ts) || length(historical_ts) == 0 ||
      is.null(forecast_ts) || length(forecast_ts) == 0) {
    warning("Invalid time series for YoY growth calculation")
    return(ggplot2::ggplot() + 
             ggplot2::annotate("text", x = 0, y = 0, 
                               label = "Insufficient data for YoY growth calculation") + 
             ggplot2::theme_minimal() + 
             ggplot2::labs(title = title))
  }
  
  # Fix frequency mismatch
  if (frequency(historical_ts) != frequency(forecast_ts)) {
    warning("Frequency mismatch between historical and forecast series. Adjusting forecast frequency.")
    forecast_ts <- tryCatch({
      ts(as.numeric(forecast_ts), 
         start = start(forecast_ts), 
         frequency = frequency(historical_ts))
    }, error = function(e) {
      warning("Error converting forecast frequency: ", e$message)
      # Create minimal valid forecast
      ts(rep(0, frequency(historical_ts)), 
         start = c(end(historical_ts)[1] + 1, 1), 
         frequency = frequency(historical_ts))
    })
  }
  
  # Calculate combined YoY growth
  growth_data <- tryCatch({
    calculate_combined_yoy_growth(historical_ts, forecast_ts, log_transform)
  }, error = function(e) {
    warning("Error calculating YoY growth: ", e$message)
    # Return empty list
    list(historical = NULL, forecast = NULL, combined = NULL)
  })
  
  if ((is.null(growth_data$historical) || length(growth_data$historical) == 0) && 
      (is.null(growth_data$forecast) || length(growth_data$forecast) == 0)) {
    # Not enough data for YoY calculation
    message("Not enough data to calculate year-over-year growth rates.")
    p <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0, y = 0, 
                        label = "Insufficient data for YoY growth calculation") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = title)
    return(p)
  }
  
  # Create data frames for plotting
  df_list <- list()
  
  if (!is.null(growth_data$historical) && length(growth_data$historical) > 0) {
    df_list$historical <- tryCatch({
      data.frame(
        date = zoo::index(zoo::as.zoo(growth_data$historical)),
        value = as.numeric(growth_data$historical),
        series = "Historical"
      )
    }, error = function(e) {
      warning("Error creating historical data frame: ", e$message)
      data.frame(
        date = seq(as.Date("2020-01-01"), by = "quarter", length.out = length(growth_data$historical)),
        value = as.numeric(growth_data$historical),
        series = "Historical"
      )
    })
  }
  
  if (!is.null(growth_data$forecast) && length(growth_data$forecast) > 0) {
    df_list$forecast <- tryCatch({
      data.frame(
        date = zoo::index(zoo::as.zoo(growth_data$forecast)),
        value = as.numeric(growth_data$forecast),
        series = "Forecast"
      )
    }, error = function(e) {
      warning("Error creating forecast data frame: ", e$message)
      # Use last historical date as reference if available
      last_date <- if (!is.null(df_list$historical)) max(df_list$historical$date) else as.Date("2020-01-01")
      data.frame(
        date = seq(last_date + 90, by = "quarter", length.out = length(growth_data$forecast)),
        value = as.numeric(growth_data$forecast),
        series = "Forecast"
      )
    })
  }
  
  # Check if we have any data to plot
  if (length(df_list) == 0) {
    return(ggplot2::ggplot() + 
             ggplot2::annotate("text", x = 0, y = 0, 
                               label = "No data available for YoY growth") + 
             ggplot2::theme_minimal() + 
             ggplot2::labs(title = title))
  }
  
  # Combine data frames
  df <- do.call(rbind, df_list)
  
  # Create plot
  p <- ggplot2::ggplot() +
    ggplot2::labs(title = title, x = "Date", y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10)
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "gray50")
  
  # Add lines if we have data
  if (!is.null(df_list$historical)) {
    p <- p + ggplot2::geom_line(data = df_list$historical,
                                ggplot2::aes(x = date, y = value),
                                color = "steelblue", linewidth = 1)
  }
  
  if (!is.null(df_list$forecast)) {
    p <- p + ggplot2::geom_line(data = df_list$forecast,
                                ggplot2::aes(x = date, y = value),
                                color = "tomato", linewidth = 1, linetype = "dashed")
  }
  
  # Add forecast transition line if we have both historical and forecast
  if (!is.null(df_list$historical) && !is.null(df_list$forecast)) {
    transition_x <- min(df_list$forecast$date)
    p <- p + ggplot2::geom_vline(
      xintercept = as.numeric(transition_x),
      linetype = "dotted", color = "gray50"
    )
  }
  
  # Add a legend
  p <- p + ggplot2::annotate(
    "text", 
    x = min(df$date, na.rm = TRUE) + 0.8 * (max(df$date, na.rm = TRUE) - min(df$date, na.rm = TRUE)),
    y = min(df$value, na.rm = TRUE) + 0.1 * (max(df$value, na.rm = TRUE) - min(df$value, na.rm = TRUE)),
    label = paste("— Historical   --- Forecast"),
    hjust = 0
  )
  
  return(p)
}

#' Create a barplot of model weights
#'
#' @param weights Named vector of weights
#' @param title Plot title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#'
#' @return ggplot object
#' @export
plot_model_weights <- function(weights, title = "Model Weights", 
                               xlab = "Model", ylab = "Weight") {
  # Convert weights to data frame
  df <- data.frame(
    model = names(weights),
    weight = as.numeric(weights)
  )
  
  # Sort by weight
  df <- df[order(df$weight, decreasing = TRUE), ]
  
  # Create plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(model, -weight), y = weight)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = ggplot2::element_text(size = 10)
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent)
  
  return(p)
}

#' Create a heatmap of error metrics
#'
#' @param error_metrics List of error metrics
#' @param metric Name of the error metric to plot ("MAE", "RMSE", "MSE", "MAPE")
#' @param variable_name Optional variable name for multivariate models
#' @param title Plot title
#'
#' @return ggplot object
#' @export
plot_error_heatmap <- function(error_metrics, metric = "RMSE", variable_name = NULL, 
                               title = paste(metric, "by Model and Horizon")) {
  # Determine if we have univariate or multivariate error metrics
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
  
  # Extract error metric values
  df <- data.frame(model = character(), horizon = numeric(), value = numeric())
  
  if (is_multivariate) {
    # Multivariate case (VAR, BVAR, FAVAR)
    if (is.null(variable_name)) {
      stop("variable_name must be provided for multivariate error metrics")
    }
    
    for (model_name in names(error_metrics)) {
      if (variable_name %in% names(error_metrics[[model_name]])) {
        var_metrics <- error_metrics[[model_name]][[variable_name]]
        for (h in seq_along(var_metrics)) {
          if (metric %in% names(var_metrics[[h]])) {
            value <- var_metrics[[h]][[metric]]
            df <- rbind(df, data.frame(model = model_name, horizon = h, value = value))
          }
        }
      }
    }
  } else {
    # Univariate case
    for (model_name in names(error_metrics)) {
      for (h in seq_along(error_metrics[[model_name]])) {
        if (metric %in% names(error_metrics[[model_name]][[h]])) {
          value <- error_metrics[[model_name]][[h]][[metric]]
          df <- rbind(df, data.frame(model = model_name, horizon = h, value = value))
        }
      }
    }
  }
  
  if (nrow(df) == 0) {
    stop("No error metric values found")
  }
  
  # Create plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = horizon, y = model, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(name = metric, direction = -1) +
    ggplot2::labs(title = title, x = "Forecast Horizon", y = "Model") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = "right"
    )
  
  return(p)
}

#' Create a combination of plots for a comprehensive view
#'
#' @param historical_ts Time series of historical data
#' @param forecast_ts Time series of forecast values
#' @param weights Named vector of model weights
#' @param error_metrics List of error metrics
#' @param metric Name of the error metric to plot
#' @param variable_name Optional variable name for multivariate models
#' @param title Main title for the combined plot
#' @param log_transform Logical indicating whether the series are in log scale
#'
#' @return A combined grid of plots
#' @export
create_forecast_summary_plots <- function(historical_ts, forecast_ts, weights, 
                                          error_metrics, metric = "RMSE", 
                                          variable_name = NULL, 
                                          title = "Forecast Summary", 
                                          log_transform = FALSE) {
  # Create level plot
  level_plot <- plot_forecast(historical_ts, forecast_ts, 
                              title = "Level Forecast", 
                              ylab = "Value")
  
  # Create growth plot
  growth_plot <- plot_yoy_growth(historical_ts, forecast_ts, 
                                 title = "Year-over-Year Growth", 
                                 ylab = "Percent Change (%)", 
                                 log_transform = log_transform)
  
  # Create weights plot
  weights_plot <- plot_model_weights(weights, 
                                     title = "Model Weights", 
                                     xlab = "Model", 
                                     ylab = "Weight")
  
  # Create error heatmap
  error_plot <- plot_error_heatmap(error_metrics, 
                                   metric = metric, 
                                   variable_name = variable_name, 
                                   title = paste(metric, "by Model and Horizon"))
  
  # Combine plots
  combined_plot <- gridExtra::grid.arrange(
    level_plot, growth_plot, 
    weights_plot, error_plot, 
    ncol = 2, 
    top = grid::textGrob(title, gp = grid::gpar(fontsize = 16, fontface = "bold"))
  )
  
  return(combined_plot)
}

#' Generate a comprehensive forecast report for a variable
#'
#' @param series_name Name of the series
#' @param historical_ts Time series of historical data
#' @param forecast_ts Time series of forecast values
#' @param original_ts Original (non-seasonally adjusted) time series
#' @param sa_ts Seasonally adjusted time series
#' @param weights Named vector of model weights
#' @param error_metrics List of error metrics
#' @param methodology_weights Named vector of methodology weights
#' @param output_dir Directory to save output files
#' @param log_transform Logical indicating whether the series are in log scale
#'
#' @return Invisibly returns the path to the generated report
#' @export
generate_forecast_report <- function(series_name, historical_ts, forecast_ts, 
                                     original_ts = NULL, sa_ts = NULL,
                                     weights, error_metrics, methodology_weights = NULL,
                                     output_dir = REPORTS_DIR, 
                                     log_transform = FALSE) {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Create figures directory within output directory
  figures_dir <- file.path(output_dir, "figures")
  if (!dir.exists(figures_dir)) {
    dir.create(figures_dir, recursive = TRUE)
  }
  
  # Sanitize series name for file naming
  sanitized_name <- gsub("[^a-zA-Z0-9]", "_", series_name)
  
  # Generate individual plots within tryCatch blocks
  
  # Level plot
  level_file <- NULL
  tryCatch({
    level_plot <- plot_forecast(historical_ts, forecast_ts, 
                                title = paste("Level Forecast:", series_name), 
                                ylab = "Value")
    level_file <- file.path(figures_dir, paste0(sanitized_name, "_level.png"))
    ggplot2::ggsave(level_file, level_plot, width = 10, height = 6, dpi = 300)
  }, error = function(e) {
    warning("Failed to create level plot: ", e$message)
  })
  
  # Original vs. Seasonally Adjusted plot (if available)
  sa_file <- NULL
  if (!is.null(original_ts) && !is.null(sa_ts)) {
    tryCatch({
      sa_plot <- plot_seasonal_adjustment(
        original_ts, sa_ts,
        title = paste("Quarterly levels (SA vs Original):", series_name)
      )
      sa_file <- file.path(figures_dir, paste0(sanitized_name, "_sa.png"))
      ggplot2::ggsave(sa_file, sa_plot, width = 10, height = 6, dpi = 300)
    }, error = function(e) {
      warning("Failed to create SA comparison plot: ", e$message)
    })
  }
  
  # Growth plot
  growth_file <- NULL
  tryCatch({
    growth_plot <- plot_yoy_growth(historical_ts, forecast_ts, 
                                   title = paste("Year-over-Year Growth:", series_name), 
                                   ylab = "Percent Change (%)", 
                                   log_transform = log_transform)
    growth_file <- file.path(figures_dir, paste0(sanitized_name, "_growth.png"))
    ggplot2::ggsave(growth_file, growth_plot, width = 10, height = 6, dpi = 300)
  }, error = function(e) {
    warning("Failed to create growth plot: ", e$message)
  })
  
  # Weights plot
  weights_file <- NULL
  tryCatch({
    weights_plot <- plot_model_weights(weights, 
                                       title = paste("Model Weights:", series_name), 
                                       xlab = "Model", 
                                       ylab = "Weight")
    weights_file <- file.path(figures_dir, paste0(sanitized_name, "_weights.png"))
    ggplot2::ggsave(weights_file, weights_plot, width = 10, height = 6, dpi = 300)
  }, error = function(e) {
    warning("Failed to create weights plot: ", e$message)
  })
  
  # Error metrics plot for RMSE
  rmse_file <- NULL
  tryCatch({
    rmse_plot <- plot_error_heatmap(error_metrics, 
                                    metric = "RMSE", 
                                    variable_name = series_name, 
                                    title = paste("RMSE by Model and Horizon:", series_name))
    rmse_file <- file.path(figures_dir, paste0(sanitized_name, "_rmse.png"))
    ggplot2::ggsave(rmse_file, rmse_plot, width = 10, height = 6, dpi = 300)
  }, error = function(e) {
    warning("Failed to create RMSE plot: ", e$message)
  })
  
  # Error metrics plot for MAPE
  mape_file <- NULL
  tryCatch({
    mape_plot <- plot_error_heatmap(error_metrics, 
                                    metric = "MAPE", 
                                    variable_name = series_name, 
                                    title = paste("MAPE by Model and Horizon:", series_name))
    mape_file <- file.path(figures_dir, paste0(sanitized_name, "_mape.png"))
    ggplot2::ggsave(mape_file, mape_plot, width = 10, height = 6, dpi = 300)
  }, error = function(e) {
    warning("Failed to create MAPE plot: ", e$message)
  })
  
  # Methodology weights plot (if available)
  method_file <- NULL
  if (!is.null(methodology_weights)) {
    tryCatch({
      method_plot <- plot_model_weights(methodology_weights, 
                                        title = paste("Methodology Weights:", series_name), 
                                        xlab = "Methodology", 
                                        ylab = "Weight")
      method_file <- file.path(figures_dir, paste0(sanitized_name, "_methodology.png"))
      ggplot2::ggsave(method_file, method_plot, width = 10, height = 6, dpi = 300)
    }, error = function(e) {
      warning("Failed to create methodology weights plot: ", e$message)
    })
  }
  
  # Generate report in Markdown format
  report_file <- file.path(output_dir, paste0(sanitized_name, "_report.md"))
  
  # Create initial report content
  report_content <- c(
    paste("# Forecast Report:", series_name),
    "",
    "## 1. Level Forecast",
    ""
  )
  
  # Add level plot if available
  if (!is.null(level_file)) {
    report_content <- c(report_content,
                        paste0("![Level Forecast](", file.path("figures", paste0(sanitized_name, "_level.png")), ")"),
                        ""
    )
  } else {
    report_content <- c(report_content,
                        "*Level forecast plot not available*",
                        ""
    )
  }
  
  # Add SA section if available
  if (!is.null(sa_file)) {
    sa_section <- c(
      "## 2. Original vs. Seasonally Adjusted Series",
      "",
      paste0("![Original vs. SA](", file.path("figures", paste0(sanitized_name, "_sa.png")), ")"),
      ""
    )
    # Append the SA section
    report_content <- c(report_content, sa_section)
  }
  
  # Add growth section if available
  if (!is.null(growth_file)) {
    growth_section <- c(
      "## 3. Year-over-Year Growth",
      "",
      paste0("![Growth Forecast](", file.path("figures", paste0(sanitized_name, "_growth.png")), ")"),
      ""
    )
    report_content <- c(report_content, growth_section)
  } else {
    report_content <- c(report_content,
                        "## 3. Year-over-Year Growth",
                        "",
                        "*Growth forecast plot not available*",
                        ""
    )
  }
  
  # Add weights section if available
  if (!is.null(weights_file)) {
    weights_section <- c(
      "## 4. Model Weights",
      "",
      paste0("![Model Weights](", file.path("figures", paste0(sanitized_name, "_weights.png")), ")"),
      ""
    )
    report_content <- c(report_content, weights_section)
  } else {
    report_content <- c(report_content,
                        "## 4. Model Weights",
                        "",
                        "*Model weights plot not available*",
                        ""
    )
  }
  
  # Add error metrics section
  report_content <- c(report_content,
                      "## 5. Error Metrics",
                      ""
  )
  
  # Add RMSE plot if available
  if (!is.null(rmse_file)) {
    report_content <- c(report_content,
                        "### RMSE by Model and Horizon",
                        "",
                        paste0("![RMSE Heatmap](", file.path("figures", paste0(sanitized_name, "_rmse.png")), ")"),
                        ""
    )
  } else {
    report_content <- c(report_content,
                        "### RMSE by Model and Horizon",
                        "",
                        "*RMSE heatmap not available*",
                        ""
    )
  }
  
  # Add MAPE plot if available
  if (!is.null(mape_file)) {
    report_content <- c(report_content,
                        "### MAPE by Model and Horizon",
                        "",
                        paste0("![MAPE Heatmap](", file.path("figures", paste0(sanitized_name, "_mape.png")), ")"),
                        ""
    )
  } else {
    report_content <- c(report_content,
                        "### MAPE by Model and Horizon",
                        "",
                        "*MAPE heatmap not available*",
                        ""
    )
  }
  
  # Add methodology weights section if available
  if (!is.null(method_file)) {
    method_section <- c(
      "",
      "## 6. Methodology Weights",
      "",
      paste0("![Methodology Weights](", file.path("figures", paste0(sanitized_name, "_methodology.png")), ")")
    )
    report_content <- c(report_content, method_section)
  }
  
  # Add forecast values table
  forecast_values <- tryCatch({
    data.frame(
      Date = zoo::index(zoo::as.zoo(forecast_ts)),
      Value = as.numeric(forecast_ts)
    )
  }, error = function(e) {
    warning("Error creating forecast values data frame: ", e$message)
    # Fallback - create minimal valid data frame
    data.frame(
      Date = paste0("Q", 1:length(forecast_ts)),
      Value = as.numeric(forecast_ts)
    )
  })
  
  # Create a table in markdown format
  table_rows <- c(
    "",
    "## Forecast Values",
    "",
    "| Date | Value |",
    "| ---- | ----- |"
  )
  
  for (i in 1:nrow(forecast_values)) {
    table_rows <- c(table_rows, 
                    paste0("| ", forecast_values$Date[i], " | ", 
                           format(forecast_values$Value[i], digits = 4), " |"))
  }
  
  report_content <- c(report_content, table_rows)
  
  # Write report to file
  tryCatch({
    writeLines(report_content, report_file)
    message("Generated Markdown report: ", report_file)
  }, error = function(e) {
    warning("Failed to write report file: ", e$message)
  })
  
  # Optional: Convert to PDF using rmarkdown
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    pdf_file <- file.path(output_dir, paste0(sanitized_name, "_report.pdf"))
    tryCatch({
      rmarkdown::render(report_file, output_file = pdf_file, quiet = TRUE)
      message("Generated PDF report: ", pdf_file)
    }, error = function(e) {
      warning("Could not convert to PDF: ", e$message)
    })
  }
  
  return(invisible(report_file))
}

#' Create a simple time series plot
#'
#' @param ts_obj Time series object
#' @param title Plot title
#' @param ylab Y-axis label
#'
#' @return ggplot object
#' @export
plot_simple_ts <- function(ts_obj, title = "Time Series", ylab = "Value") {
  # Convert to data frame
  df <- data.frame(
    date = zoo::index(zoo::as.zoo(ts_obj)),
    value = as.numeric(ts_obj)
  )
  
  # Create plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_line(color = "steelblue", linewidth = 1) +
    ggplot2::labs(title = title, x = "Date", y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10)
    )
  
  return(p)
}

#' Plot error metric evolution across forecast horizons
#'
#' @param error_metrics List of error metrics
#' @param metric Name of the error metric to plot ("MAE", "RMSE", "MSE", "MAPE")
#' @param title Plot title
#'
#' @return ggplot object
#' @export
plot_error_evolution <- function(error_metrics, metric = "MAE", title = "Error Evolution") {
  # Extract error metric values
  df <- data.frame(model = character(), horizon = numeric(), value = numeric())
  
  # Validate input
  if (is.null(error_metrics) || length(error_metrics) == 0) {
    warning("No error metrics provided for plot_error_evolution")
    return(ggplot2::ggplot() + 
             ggplot2::annotate("text", x = 0, y = 0, label = "No error metrics available") + 
             ggplot2::theme_minimal() + 
             ggplot2::labs(title = title))
  }
  
  # Extract available metrics and use fallback if needed
  available_metrics <- unique(unlist(lapply(error_metrics, function(model) {
    unique(unlist(lapply(model, function(horizon) {
      names(horizon)
    })))
  })))
  
  if (!(metric %in% available_metrics)) {
    warning("Metric ", metric, " not found. Available metrics: ", 
            paste(available_metrics, collapse = ", "))
    
    # Use first available metric instead
    if (length(available_metrics) > 0) {
      metric <- available_metrics[1]
      message("Using ", metric, " as fallback metric")
    } else {
      return(ggplot2::ggplot() + 
               ggplot2::annotate("text", x = 0, y = 0, label = "No metrics available") + 
               ggplot2::theme_minimal() + 
               ggplot2::labs(title = title))
    }
  }
  
  # Process each model
  for (model_name in names(error_metrics)) {
    for (h in seq_along(error_metrics[[model_name]])) {
      if (metric %in% names(error_metrics[[model_name]][[h]])) {
        value <- error_metrics[[model_name]][[h]][[metric]]
        if (!is.null(value) && !is.na(value) && is.finite(value)) {
          df <- rbind(df, data.frame(model = model_name, horizon = h, value = value))
        }
      }
    }
  }
  
  # Create plot if we have data
  if (nrow(df) > 0) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = horizon, y = value, color = model, group = model)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 2) +
      ggplot2::labs(title = title, x = "Forecast Horizon", y = metric) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        axis.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 10),
        legend.position = "bottom",
        legend.title = ggplot2::element_blank()
      )
    return(p)
  } else {
    # Return empty plot with message
    return(ggplot2::ggplot() + 
             ggplot2::annotate("text", x = 0, y = 0, 
                               label = paste("No valid", metric, "values available")) + 
             ggplot2::theme_minimal() + 
             ggplot2::labs(title = title))
  }
}

#' Plot model weights evolution over forecast horizons
#'
#' @param results Results list for a series
#' @param title Plot title
#'
#' @return ggplot object
#' @export
plot_weights_evolution <- function(results, title = "Weight per quarter for combining models") {
  # Check if methodology weights exist
  if (is.null(results) || is.null(results$methodology_weights)) {
    warning("No methodology weights found")
    return(ggplot2::ggplot() + 
             ggplot2::annotate("text", x = 0, y = 0, 
                               label = "No methodology weights available") + 
             ggplot2::theme_minimal() + 
             ggplot2::labs(title = title))
  }
  
  # Extract weights safely
  weights <- c()
  
  # Handle different possible structures
  if (is.list(results$methodology_weights)) {
    for (method in names(results$methodology_weights)) {
      weight_val <- results$methodology_weights[[method]]
      if (is.numeric(weight_val) && length(weight_val) == 1) {
        weights <- c(weights, setNames(weight_val, method))
      }
    }
  }
  
  # Create fallback weights if needed
  if (length(weights) == 0) {
    warning("Could not extract methodology weights. Using default values.")
    weights <- c(Univariate = 1.0)
  }
  
  # Normalize weights to ensure they sum to 1
  weights <- weights / sum(weights)
  
  # Create data frame
  df <- data.frame(
    methodology = names(weights),
    weight = as.numeric(weights)
  )
  
  # Create plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = methodology, y = weight, fill = methodology)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(title = title, x = "Methodology", y = "Weight") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = "none"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent)
  
  return(p)
}

#' Compare direct and indirect forecasting approaches
#'
#' @param historical_ts Time series of historical data
#' @param direct_fc Direct forecast object
#' @param indirect_fc Indirect forecast object
#' @param title Plot title
#'
#' @return ggplot object
#' @export
plot_direct_indirect_comparison <- function(historical_ts, direct_fc, indirect_fc, title = "Direct vs Indirect Forecasting") {
  # Extract forecast means
  if (is.list(direct_fc) && "mean" %in% names(direct_fc)) {
    direct_mean <- direct_fc$mean
  } else {
    direct_mean <- direct_fc
  }
  
  if (is.list(indirect_fc) && "mean" %in% names(indirect_fc)) {
    indirect_mean <- indirect_fc$mean
  } else {
    indirect_mean <- indirect_fc
  }
  
  # Convert to data frames
  df_hist <- data.frame(
    date = zoo::index(zoo::as.zoo(historical_ts)),
    value = as.numeric(historical_ts),
    series = "Historical"
  )
  
  df_direct <- data.frame(
    date = zoo::index(zoo::as.zoo(direct_mean)),
    value = as.numeric(direct_mean),
    series = "Direct"
  )
  
  df_indirect <- data.frame(
    date = zoo::index(zoo::as.zoo(indirect_mean)),
    value = as.numeric(indirect_mean),
    series = "Indirect"
  )
  
  # Combine data frames
  df <- rbind(df_hist, df_direct, df_indirect)
  
  # Create plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = value, color = series, linetype = series)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(title = title, x = "Date", y = "Value") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::scale_linetype_manual(values = c("solid", "dashed", "dotted"))
  
  return(p)
}

#' Generate RMarkdown content for the comprehensive report
#'
#' @param series_name Name of the series
#' @param plot_files List of file paths to plots
#'
#' @return Character vector containing RMarkdown content
#' @export
#' Generate RMarkdown content for the comprehensive report
#'
#' @param series_name Name of the series
#' @param plot_files List of file paths to plots
#'
#' @return Character vector containing RMarkdown content
#' @export
generate_rmd_content <- function(series_name, plot_files) {
  # Create YAML header
  rmd_content <- c(
    "---",
    paste0("title: \"GDP Forecasting Report: ", series_name, "\""),
    "output: pdf_document",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    "```",
    ""
  )
  
  # Page 1: DATA AVAILABLE AT THE MOMENT
  rmd_content <- c(rmd_content,
                   "# DATA AVAILABLE AT THE MOMENT",
                   "",
                   "## Quarterly YoY growth",
                   "")
  
  # Add plots conditionally, checking if they exist
  if (!is.null(plot_files$q_yoy) && file.exists(plot_files$q_yoy)) {
    rmd_content <- c(rmd_content, paste0("![](", plot_files$q_yoy, ")"), "")
  } else {
    rmd_content <- c(rmd_content, "*No quarterly YoY growth data available*", "")
  }
  
  rmd_content <- c(rmd_content, "## Quarterly levels (SA vs Original)", "")
  
  if (!is.null(plot_files$q_levels) && file.exists(plot_files$q_levels)) {
    rmd_content <- c(rmd_content, paste0("![](", plot_files$q_levels, ")"), "")
  } else {
    rmd_content <- c(rmd_content, "*No quarterly levels comparison available*", "")
  }
  
  rmd_content <- c(rmd_content, "## Monthly YoY growth", "")
  
  if (!is.null(plot_files$m_yoy) && file.exists(plot_files$m_yoy)) {
    rmd_content <- c(rmd_content, paste0("![](", plot_files$m_yoy, ")"), "")
  } else {
    rmd_content <- c(rmd_content, "*No monthly YoY growth data available*", "")
  }
  
  rmd_content <- c(rmd_content, "## Monthly levels (SA vs Original)", "")
  
  if (!is.null(plot_files$m_levels) && file.exists(plot_files$m_levels)) {
    rmd_content <- c(rmd_content, paste0("![](", plot_files$m_levels, ")"), "")
  } else {
    rmd_content <- c(rmd_content, "*No monthly levels comparison available*", "")
  }
  
  rmd_content <- c(rmd_content, "\\pagebreak", "")
  
  # Page 2: RESULTS VS FORECASTED
  rmd_content <- c(rmd_content,
                   "# RESULTS VS FORECASTED",
                   "",
                   "## Quarterly YoY growth (observed vs forecasted by all models)",
                   "")
  
  if (!is.null(plot_files$q_yoy_fc) && file.exists(plot_files$q_yoy_fc)) {
    rmd_content <- c(rmd_content, paste0("![](", plot_files$q_yoy_fc, ")"), "")
  } else {
    rmd_content <- c(rmd_content, "*No forecasted YoY growth data available*", "")
  }
  
  rmd_content <- c(rmd_content, 
                   "## Quarterly YoY growth (observed vs best direct vs best indirect forecasted)",
                   "")
  
  if (!is.null(plot_files$direct_indirect) && file.exists(plot_files$direct_indirect)) {
    rmd_content <- c(rmd_content, paste0("![](", plot_files$direct_indirect, ")"), "")
  } else {
    rmd_content <- c(rmd_content, "*No direct vs indirect comparison available*", "")
  }
  
  rmd_content <- c(rmd_content, "## MAE evolution per model per quarter", "")
  
  if (!is.null(plot_files$mae) && file.exists(plot_files$mae)) {
    rmd_content <- c(rmd_content, paste0("![](", plot_files$mae, ")"), "")
  } else {
    rmd_content <- c(rmd_content, "*No MAE evolution data available*", "")
  }
  
  rmd_content <- c(rmd_content, "## Weight per quarter for combining models", "")
  
  if (!is.null(plot_files$weights) && file.exists(plot_files$weights)) {
    rmd_content <- c(rmd_content, paste0("![](", plot_files$weights, ")"), "")
  } else {
    rmd_content <- c(rmd_content, "*No model weights data available*", "")
  }
  
  rmd_content <- c(rmd_content, "\\pagebreak", "")
  
  # Page 3: FORECASTING RESULTS
  rmd_content <- c(rmd_content,
                   "# FORECASTING RESULTS",
                   "",
                   "## Forecasted levels",
                   "")
  
  if (!is.null(plot_files$level) && file.exists(plot_files$level)) {
    rmd_content <- c(rmd_content, paste0("![](", plot_files$level, ")"), "")
  } else {
    rmd_content <- c(rmd_content, "*No forecasted levels available*", "")
  }
  
  rmd_content <- c(rmd_content, "## Forecasted quarterly YoY growth", "")
  
  if (!is.null(plot_files$yoy_forecast) && file.exists(plot_files$yoy_forecast)) {
    rmd_content <- c(rmd_content, paste0("![](", plot_files$yoy_forecast, ")"), "")
  } else {
    rmd_content <- c(rmd_content, "*No forecasted YoY growth available*", "")
  }
  
  rmd_content <- c(rmd_content,
                   "## Comments regarding the activity and expectations",
                   "",
                   paste0("The forecast for ", series_name, " shows ", 
                          if (!is.null(plot_files$yoy_forecast) && file.exists(plot_files$yoy_forecast)) 
                            "a pattern of growth " 
                          else "results ",
                          "that requires economic interpretation. The most reliable models for this forecast ",
                          "are indicated by the weights shown in the previous page. Based on historical patterns ",
                          "and recent data, we can expect this series to continue its current trend with ",
                          "seasonal factors appropriately accounted for in the projections."))
  
  return(rmd_content)
}

# Define the plot_weights_evolution function to handle missing data properly
plot_weights_evolution <- function(results, title = "Weight per quarter for combining models") {
  # Check if methodology weights exist
  if (is.null(results) || is.null(results$methodology_weights)) {
    warning("No methodology weights found")
    return(ggplot2::ggplot() + 
             ggplot2::annotate("text", x = 0, y = 0, 
                               label = "No methodology weights available") + 
             ggplot2::theme_minimal() + 
             ggplot2::labs(title = title))
  }
  
  # Extract weights safely
  weights <- c()
  
  # Handle different possible structures
  if (is.list(results$methodology_weights)) {
    for (method in names(results$methodology_weights)) {
      weight_val <- results$methodology_weights[[method]]
      if (is.numeric(weight_val) && length(weight_val) == 1) {
        weights <- c(weights, setNames(weight_val, method))
      }
    }
  }
  
  # Create fallback weights if needed
  if (length(weights) == 0) {
    warning("Could not extract methodology weights. Using default values.")
    weights <- c(Univariate = 1.0)
  }
  
  # Normalize weights to ensure they sum to 1
  weights <- weights / sum(weights)
  
  # Create data frame
  df <- data.frame(
    methodology = names(weights),
    weight = as.numeric(weights)
  )
  
  # Create plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = methodology, y = weight, fill = methodology)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(title = title, x = "Methodology", y = "Weight") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = "none"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent)
  
  return(p)
}

# Define a safer version of plot_direct_indirect_comparison
plot_direct_indirect_comparison <- function(historical_ts, direct_fc, indirect_fc, 
                                            title = "Direct vs Indirect Forecasting") {
  # Safety checks
  if (is.null(historical_ts) || is.null(direct_fc) || is.null(indirect_fc)) {
    warning("Missing input data for direct vs indirect comparison")
    return(ggplot2::ggplot() + 
             ggplot2::annotate("text", x = 0, y = 0, 
                               label = "Insufficient data for comparison") + 
             ggplot2::theme_minimal() + 
             ggplot2::labs(title = title))
  }
  
  # Extract forecast means
  direct_mean <- NULL
  if (is.list(direct_fc) && "mean" %in% names(direct_fc)) {
    direct_mean <- direct_fc$mean
  } else if (is.numeric(direct_fc) || is.ts(direct_fc)) {
    direct_mean <- direct_fc
  } else {
    warning("Invalid direct forecast format")
    return(ggplot2::ggplot() + 
             ggplot2::annotate("text", x = 0, y = 0, 
                               label = "Invalid direct forecast format") + 
             ggplot2::theme_minimal() + 
             ggplot2::labs(title = title))
  }
  
  indirect_mean <- NULL
  if (is.list(indirect_fc) && "mean" %in% names(indirect_fc)) {
    indirect_mean <- indirect_fc$mean
  } else if (is.numeric(indirect_fc) || is.ts(indirect_fc)) {
    indirect_mean <- indirect_fc
  } else {
    warning("Invalid indirect forecast format")
    return(ggplot2::ggplot() + 
             ggplot2::annotate("text", x = 0, y = 0, 
                               label = "Invalid indirect forecast format") + 
             ggplot2::theme_minimal() + 
             ggplot2::labs(title = title))
  }
  
  # Convert to data frames
  df_hist <- tryCatch({
    data.frame(
      date = zoo::index(zoo::as.zoo(historical_ts)),
      value = as.numeric(historical_ts),
      series = "Historical"
    )
  }, error = function(e) {
    warning("Error creating historical data frame: ", e$message)
    # Create dummy data frame as fallback
    data.frame(
      date = seq(as.Date("2020-01-01"), by = "quarter", length.out = length(historical_ts)),
      value = as.numeric(historical_ts),
      series = "Historical"
    )
  })
  
  df_direct <- tryCatch({
    if (is.ts(direct_mean)) {
      data.frame(
        date = zoo::index(zoo::as.zoo(direct_mean)),
        value = as.numeric(direct_mean),
        series = "Direct"
      )
    } else {
      # For non-ts objects, create continuing dates
      last_date <- max(df_hist$date)
      data.frame(
        date = seq(last_date + 90, by = "quarter", length.out = length(direct_mean)),
        value = as.numeric(direct_mean),
        series = "Direct"
      )
    }
  }, error = function(e) {
    warning("Error creating direct forecast data frame: ", e$message)
    # Create dummy data frame as fallback
    last_date <- max(df_hist$date)
    data.frame(
      date = seq(last_date + 90, by = "quarter", length.out = length(direct_mean)),
      value = as.numeric(direct_mean),
      series = "Direct"
    )
  })
  
  df_indirect <- tryCatch({
    if (is.ts(indirect_mean)) {
      data.frame(
        date = zoo::index(zoo::as.zoo(indirect_mean)),
        value = as.numeric(indirect_mean),
        series = "Indirect"
      )
    } else {
      # For non-ts objects, create continuing dates
      last_date <- max(df_hist$date)
      data.frame(
        date = seq(last_date + 90, by = "quarter", length.out = length(indirect_mean)),
        value = as.numeric(indirect_mean),
        series = "Indirect"
      )
    }
  }, error = function(e) {
    warning("Error creating indirect forecast data frame: ", e$message)
    # Create dummy data frame as fallback
    last_date <- max(df_hist$date)
    data.frame(
      date = seq(last_date + 90, by = "quarter", length.out = length(indirect_mean)),
      value = as.numeric(indirect_mean),
      series = "Indirect"
    )
  })
  
  # Combine data frames
  df <- rbind(df_hist, df_direct, df_indirect)
  
  # Create plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = value, color = series, linetype = series)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(title = title, x = "Date", y = "Value") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::scale_linetype_manual(values = c("solid", "dashed", "dotted"))
  
  return(p)
}

#' Generate a comprehensive multi-page forecasting report
#'
#' @param series_name Name of the series
#' @param results Results list for this series
#' @param ts_list List of original time series
#' @param sa_list List of seasonally adjusted time series
#' @param output_dir Directory to save output files
#'
#' @return Path to the generated report
#' @export
generate_comprehensive_forecasting_report <- function(series_name, results, ts_list, sa_list, 
                                                      output_dir = REPORTS_DIR) {
  # Ensure output directories exist
  dirs_to_create <- c(
    output_dir,
    file.path(output_dir, "figures"),
    file.path(output_dir, "figures", gsub("[^a-zA-Z0-9]", "_", series_name))
  )
  
  for (dir in dirs_to_create) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      message("Created directory: ", dir)
    }
  }
  
  # Sanitize series name for file naming
  sanitized_name <- gsub("[^a-zA-Z0-9]", "_", series_name)
  
  # Create figures directory for this report
  figures_dir <- file.path(output_dir, "figures", sanitized_name)
  if (!dir.exists(figures_dir)) {
    dir.create(figures_dir, recursive = TRUE)
  }
  
  # Ensure results contain necessary components
  if (is.null(results) || !is.list(results)) {
    warning("Invalid results object for ", series_name)
    return(invisible(NULL))
  }
  
  # Validate and safely extract needed data
  historical_ts <- tryCatch({
    if (!is.null(sa_list$quarterly[[series_name]])) {
      sa_list$quarterly[[series_name]]
    } else {
      warning("No SA series found for ", series_name)
      # Create dummy time series
      ts(rep(0, 20), start = c(2020, 1), frequency = 4)
    }
  }, error = function(e) {
    warning("Error extracting historical series: ", e$message)
    ts(rep(0, 20), start = c(2020, 1), frequency = 4)
  })
  
  original_ts <- tryCatch({
    if (!is.null(ts_list$quarterly[[series_name]])) {
      ts_list$quarterly[[series_name]]
    } else {
      warning("No original series found for ", series_name)
      historical_ts  # Use historical as fallback
    }
  }, error = function(e) {
    warning("Error extracting original series: ", e$message)
    historical_ts
  })
  
  series_results <- results[[series_name]]
  if (is.null(series_results)) {
    warning("No results found for series: ", series_name)
    return(invisible(NULL))
  }
  
  forecast_ts <- tryCatch({
    if (!is.null(series_results$final_forecast)) {
      # Ensure forecast has same frequency as historical
      ts(as.numeric(series_results$final_forecast),
         start = start(series_results$final_forecast),
         frequency = frequency(historical_ts))
    } else {
      warning("No forecast found for ", series_name)
      # Create dummy forecast
      next_period <- c(end(historical_ts)[1], end(historical_ts)[2] + 1)
      if (next_period[2] > frequency(historical_ts)) {
        next_period[1] <- next_period[1] + 1
        next_period[2] <- 1
      }
      ts(rep(mean(historical_ts, na.rm = TRUE), 4), 
         start = next_period, 
         frequency = frequency(historical_ts))
    }
  }, error = function(e) {
    warning("Error extracting forecast: ", e$message)
    next_period <- c(end(historical_ts)[1], end(historical_ts)[2] + 1)
    if (next_period[2] > frequency(historical_ts)) {
      next_period[1] <- next_period[1] + 1
      next_period[2] <- 1
    }
    ts(rep(mean(historical_ts, na.rm = TRUE), 4), 
       start = next_period, 
       frequency = frequency(historical_ts))
  })
  
  # Ensure methodology weights exist
  methodology_weights <- NULL
  if (!is.null(series_results$methodology_weights)) {
    methodology_weights <- series_results$methodology_weights
  } else {
    methodology_weights <- list(
      univariate = 1.0,
      monthly = 0.0,
      multivariate = 0.0
    )
  }
  
  ########################
  # PAGE 1: DATA AVAILABLE
  ########################
  
  # Panel 1: Quarterly YoY growth
  q_yoy_file <- NULL
  tryCatch({
    q_yoy <- calculate_yoy_growth_rates(historical_ts, LOG_TRANSFORM)
    q_yoy_plot <- plot_simple_ts(q_yoy, "Quarterly YoY growth")
    q_yoy_file <- file.path(figures_dir, paste0(sanitized_name, "_q_yoy.png"))
    ggplot2::ggsave(q_yoy_file, q_yoy_plot, width = 8, height = 6, dpi = 300)
  }, error = function(e) {
    warning("Failed to create quarterly YoY growth plot: ", e$message)
  })
  
  # Panel 2: Quarterly levels (SA vs Original)
  q_levels_file <- NULL
  tryCatch({
    q_levels_plot <- plot_seasonal_adjustment(original_ts, historical_ts, 
                                              "Quarterly levels (SA vs Original)")
    q_levels_file <- file.path(figures_dir, paste0(sanitized_name, "_q_levels.png"))
    ggplot2::ggsave(q_levels_file, q_levels_plot, width = 8, height = 6, dpi = 300)
  }, error = function(e) {
    warning("Failed to create quarterly levels plot: ", e$message)
  })
  
  # Panel 3 & 4: Monthly data if available
  monthly_plots <- list()
  
  if (!is.null(series_results$monthly)) {
    # Get most correlated monthly indicator
    monthly_mapping <- series_results$monthly_mapping
    if (!is.null(monthly_mapping) && nrow(monthly_mapping) > 0) {
      top_indicator <- monthly_mapping$monthly_indicator[1]
      
      # Monthly YoY growth
      tryCatch({
        if (!is.null(sa_list$monthly[[top_indicator]])) {
          m_ts <- sa_list$monthly[[top_indicator]]
          m_yoy <- calculate_yoy_growth_rates(m_ts, LOG_TRANSFORM)
          m_yoy_plot <- plot_simple_ts(m_yoy, "Monthly YoY growth")
          m_yoy_file <- file.path(figures_dir, paste0(sanitized_name, "_m_yoy.png"))
          ggplot2::ggsave(m_yoy_file, m_yoy_plot, width = 8, height = 6, dpi = 300)
          monthly_plots$yoy <- m_yoy_file
        }
      }, error = function(e) {
        warning("Failed to create monthly YoY growth plot: ", e$message)
      })
      
      # Monthly levels (SA vs Original)
      tryCatch({
        if (!is.null(ts_list$monthly[[top_indicator]]) && !is.null(sa_list$monthly[[top_indicator]])) {
          m_orig <- ts_list$monthly[[top_indicator]]
          m_sa <- sa_list$monthly[[top_indicator]]
          m_levels_plot <- plot_seasonal_adjustment(m_orig, m_sa, 
                                                    "Monthly levels (SA vs Original)")
          m_levels_file <- file.path(figures_dir, paste0(sanitized_name, "_m_levels.png"))
          ggplot2::ggsave(m_levels_file, m_levels_plot, width = 8, height = 6, dpi = 300)
          monthly_plots$levels <- m_levels_file
        }
      }, error = function(e) {
        warning("Failed to create monthly levels plot: ", e$message)
      })
    }
  }
  
  ########################
  # PAGE 2: RESULTS VS FORECASTED
  ########################
  
  # Panel 1: Quarterly YoY growth (observed vs forecasted)
  q_yoy_fc_file <- NULL
  tryCatch({
    q_yoy_fc_plot <- plot_yoy_growth(historical_ts, forecast_ts, 
                                     "Quarterly YoY growth (observed vs forecasted)")
    q_yoy_fc_file <- file.path(figures_dir, paste0(sanitized_name, "_q_yoy_fc.png"))
    ggplot2::ggsave(q_yoy_fc_file, q_yoy_fc_plot, width = 8, height = 6, dpi = 300)
  }, error = function(e) {
    warning("Failed to create quarterly YoY forecast plot: ", e$message)
  })
  
  # Panel 2: Direct vs Indirect forecasting (if available)
  direct_indirect_file <- NULL
  if (!is.null(series_results$monthly) && 
      !is.null(series_results$univariate)) {
    tryCatch({
      # Get most recent forecast window
      uni_forecasts <- series_results$univariate$forecasts
      monthly_forecasts <- series_results$monthly$quarterly_forecasts
      
      if (length(uni_forecasts) > 0 && length(monthly_forecasts) > 0) {
        # Get univariate weights
        uni_weights <- NULL
        if (!is.null(series_results$univariate$error_metrics)) {
          uni_weights <- calculate_weights(series_results$univariate$error_metrics)
        }
        
        # Default weight if calculation fails
        if (is.null(uni_weights) || length(uni_weights) == 0) {
          uni_weights <- c("default_model" = 1.0)
        }
        
        # Get best direct forecast - safely
        uni_best_model <- names(which.max(uni_weights))
        direct_fc <- NULL
        if (length(uni_forecasts) > 0 && 
            uni_best_model %in% names(uni_forecasts[[length(uni_forecasts)]])) {
          direct_fc <- uni_forecasts[[length(uni_forecasts)]][[uni_best_model]]
        }
        
        # Get monthly weights - safely
        monthly_weights <- NULL
        if (!is.null(series_results$monthly$error_metrics)) {
          monthly_weights <- calculate_weights(series_results$monthly$error_metrics)
        }
        
        # Default weight if calculation fails
        if (is.null(monthly_weights) || length(monthly_weights) == 0) {
          if (length(monthly_forecasts) > 0 && 
              length(monthly_forecasts[[length(monthly_forecasts)]]) > 0) {
            first_model <- names(monthly_forecasts[[length(monthly_forecasts)]])[1]
            monthly_weights <- setNames(1.0, first_model)
          }
        }
        
        # Get best indirect forecast - safely
        indirect_fc <- NULL
        if (!is.null(monthly_weights) && length(monthly_weights) > 0) {
          monthly_best_model <- names(which.max(monthly_weights))
          if (length(monthly_forecasts) > 0 && 
              monthly_best_model %in% names(monthly_forecasts[[length(monthly_forecasts)]])) {
            indirect_fc <- monthly_forecasts[[length(monthly_forecasts)]][[monthly_best_model]]
          }
        }
        
        # Create comparison plot only if both forecasts are available
        if (!is.null(direct_fc) && !is.null(indirect_fc)) {
          comparison_plot <- plot_direct_indirect_comparison(
            historical_ts, direct_fc, indirect_fc,
            "Direct vs Indirect Forecasting")
          direct_indirect_file <- file.path(figures_dir, 
                                            paste0(sanitized_name, "_direct_indirect.png"))
          ggplot2::ggsave(direct_indirect_file, comparison_plot, width = 8, height = 6, dpi = 300)
        }
      }
    }, error = function(e) {
      warning("Failed to create direct vs indirect comparison plot: ", e$message)
    })
  }
  
  # Panel 3: MAE evolution per model per quarter
  mae_file <- NULL
  tryCatch({
    if (!is.null(series_results$univariate$error_metrics)) {
      mae_plot <- plot_error_evolution(series_results$univariate$error_metrics, "MAE",
                                       "MAE evolution per model per quarter")
      mae_file <- file.path(figures_dir, paste0(sanitized_name, "_mae.png"))
      ggplot2::ggsave(mae_file, mae_plot, width = 8, height = 6, dpi = 300)
    }
  }, error = function(e) {
    warning("Failed to create MAE evolution plot: ", e$message)
  })
  
  # Panel 4: Weight per quarter for combining models
  weights_file <- NULL
  tryCatch({
    weights_evolution_plot <- plot_weights_evolution(series_results,
                                                     "Weight per quarter for combining models")
    weights_file <- file.path(figures_dir, paste0(sanitized_name, "_weights_evol.png"))
    ggplot2::ggsave(weights_file, weights_evolution_plot, width = 8, height = 6, dpi = 300)
  }, error = function(e) {
    warning("Failed to create weights evolution plot: ", e$message)
  })
  
  ########################
  # PAGE 3: FORECASTING RESULTS
  ########################
  
  # Panel 1: Forecasted levels
  level_file <- NULL
  tryCatch({
    level_plot <- plot_forecast(historical_ts, forecast_ts, "Forecasted levels")
    level_file <- file.path(figures_dir, paste0(sanitized_name, "_level.png"))
    ggplot2::ggsave(level_file, level_plot, width = 8, height = 6, dpi = 300)
  }, error = function(e) {
    warning("Failed to create forecast levels plot: ", e$message)
  })
  
  # Panel 2: Forecasted quarterly YoY growth
  yoy_forecast_file <- NULL
  tryCatch({
    # If we have enough forecast periods for YoY calculation
    growth_data <- calculate_combined_yoy_growth(historical_ts, forecast_ts, LOG_TRANSFORM)
    if (!is.null(growth_data$forecast) && length(growth_data$forecast) > 0) {
      yoy_forecast_plot <- plot_simple_ts(growth_data$forecast, "Forecasted quarterly YoY growth")
      yoy_forecast_file <- file.path(figures_dir, paste0(sanitized_name, "_yoy_forecast.png"))
      ggplot2::ggsave(yoy_forecast_file, yoy_forecast_plot, width = 8, height = 6, dpi = 300)
    }
  }, error = function(e) {
    warning("Failed to create forecast YoY growth plot: ", e$message)
  })
  
  # Collect all generated plot files
  plot_files <- list(
    q_yoy = q_yoy_file,
    q_levels = q_levels_file,
    m_yoy = monthly_plots$yoy,
    m_levels = monthly_plots$levels,
    q_yoy_fc = q_yoy_fc_file,
    direct_indirect = direct_indirect_file,
    mae = mae_file,
    weights = weights_file,
    level = level_file,
    yoy_forecast = yoy_forecast_file
  )
  
  # Generate comprehensive report in RMarkdown format
  report_rmd <- file.path(output_dir, paste0(sanitized_name, "_comprehensive.Rmd"))
  
  # Generate RMarkdown content
  rmd_content <- generate_rmd_content(series_name, plot_files)
  
  # Write RMarkdown content to file
  tryCatch({
    writeLines(rmd_content, report_rmd)
    message("Generated RMarkdown report: ", report_rmd)
  }, error = function(e) {
    warning("Failed to write RMarkdown file: ", e$message)
    return(invisible(NULL))
  })
  
  # Render PDF if rmarkdown is available
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    tryCatch({
      pdf_file <- file.path(output_dir, paste0(sanitized_name, "_comprehensive.pdf"))
      rmarkdown::render(report_rmd, 
                        output_format = "pdf_document",
                        output_file = pdf_file,
                        quiet = TRUE)
      message("Generated comprehensive PDF report: ", pdf_file)
    }, error = function(e) {
      warning("Failed to generate PDF: ", e$message)
    })
  }
  
  return(report_rmd)
}

