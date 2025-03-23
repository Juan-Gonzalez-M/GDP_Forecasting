# visualization.R - Functions for creating visualizations
# This file contains functions for creating plots and figures

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
  # Convert time series to data frames
  df_historical <- data.frame(
    date = zoo::index(zoo::as.zoo(historical_ts)),
    value = as.numeric(historical_ts),
    series = historical_label
  )
  
  df_forecast <- data.frame(
    date = zoo::index(zoo::as.zoo(forecast_ts)),
    value = as.numeric(forecast_ts),
    series = forecast_label
  )
  
  # Combine data frames
  df <- rbind(df_historical, df_forecast)
  
  # Create base plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = df_historical, 
                       ggplot2::aes(x = date, y = value), 
                       color = "steelblue", linewidth = 1) +
    ggplot2::geom_line(data = df_forecast, 
                       ggplot2::aes(x = date, y = value), 
                       color = "tomato", linewidth = 1, linetype = "dashed") +
    ggplot2::labs(title = title, x = "Date", y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = "bottom"
    )
  
  # Add uncertainty bands if requested
  if (uncertainty && !is.null(lower_ts) && !is.null(upper_ts)) {
    df_lower <- data.frame(
      date = zoo::index(zoo::as.zoo(lower_ts)),
      value = as.numeric(lower_ts)
    )
    
    df_upper <- data.frame(
      date = zoo::index(zoo::as.zoo(upper_ts)),
      value = as.numeric(upper_ts)
    )
    
    # Combine lower and upper bounds for ribbon
    df_ribbon <- merge(df_lower, df_upper, by = "date")
    names(df_ribbon) <- c("date", "lower", "upper")
    
    # Add ribbon to plot
    p <- p + ggplot2::geom_ribbon(
      data = df_ribbon, 
      ggplot2::aes(x = date, ymin = lower, ymax = upper), 
      fill = "tomato", alpha = 0.2
    )
  }
  
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
    x = min(df$date) + 0.8 * (max(df$date) - min(df$date)),
    y = min(df$value) + 0.1 * (max(df$value) - min(df$value)),
    label = paste("— ", historical_label, "   --- ", forecast_label),
    hjust = 0
  )
  
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

#' Create a plot of year-over-year growth rates
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
  # Calculate YoY growth rates
  historical_yoy <- calculate_yoy_growth_rates(historical_ts, log_transform)
  forecast_yoy <- calculate_yoy_growth_rates(forecast_ts, log_transform)
  
  # Convert to data frames
  df_historical <- data.frame(
    date = zoo::index(zoo::as.zoo(historical_yoy)),
    value = as.numeric(historical_yoy),
    series = "Historical"
  )
  
  df_forecast <- data.frame(
    date = zoo::index(zoo::as.zoo(forecast_yoy)),
    value = as.numeric(forecast_yoy),
    series = "Forecast"
  )
  
  # Combine data frames
  df <- rbind(df_historical, df_forecast)
  
  # Create plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = df_historical, 
                       ggplot2::aes(x = date, y = value), 
                       color = "steelblue", linewidth = 1) +
    ggplot2::geom_line(data = df_forecast, 
                       ggplot2::aes(x = date, y = value), 
                       color = "tomato", linewidth = 1, linetype = "dashed") +
    ggplot2::labs(title = title, x = "Date", y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = "bottom"
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "gray50")
  
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
    x = min(df$date) + 0.8 * (max(df$date) - min(df$date)),
    y = min(df$value) + 0.1 * (max(df$value) - min(df$value)),
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
#' @param weights Named vector of model weights
#' @param error_metrics List of error metrics
#' @param methodology_weights Named vector of methodology weights
#' @param output_dir Directory to save output files
#' @param log_transform Logical indicating whether the series are in log scale
#'
#' @return Invisibly returns the path to the generated report
#' @export
generate_forecast_report <- function(series_name, historical_ts, forecast_ts, 
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
  
  # Generate individual plots
  
  # Level plot
  level_plot <- plot_forecast(historical_ts, forecast_ts, 
                              title = paste("Level Forecast:", series_name), 
                              ylab = "Value")
  level_file <- file.path(figures_dir, paste0(sanitized_name, "_level.png"))
  ggplot2::ggsave(level_file, level_plot, width = 10, height = 6, dpi = 300)
  
  # Growth plot
  growth_plot <- plot_yoy_growth(historical_ts, forecast_ts, 
                                 title = paste("Year-over-Year Growth:", series_name), 
                                 ylab = "Percent Change (%)", 
                                 log_transform = log_transform)
  growth_file <- file.path(figures_dir, paste0(sanitized_name, "_growth.png"))
  ggplot2::ggsave(growth_file, growth_plot, width = 10, height = 6, dpi = 300)
  
  # Weights plot
  weights_plot <- plot_model_weights(weights, 
                                     title = paste("Model Weights:", series_name), 
                                     xlab = "Model", 
                                     ylab = "Weight")
  weights_file <- file.path(figures_dir, paste0(sanitized_name, "_weights.png"))
  ggplot2::ggsave(weights_file, weights_plot, width = 10, height = 6, dpi = 300)
  
  # Error metrics plot for RMSE
  rmse_plot <- plot_error_heatmap(error_metrics, 
                                  metric = "RMSE", 
                                  variable_name = series_name, 
                                  title = paste("RMSE by Model and Horizon:", series_name))
  rmse_file <- file.path(figures_dir, paste0(sanitized_name, "_rmse.png"))
  ggplot2::ggsave(rmse_file, rmse_plot, width = 10, height = 6, dpi = 300)
  
  # Error metrics plot for MAPE
  mape_plot <- plot_error_heatmap(error_metrics, 
                                  metric = "MAPE", 
                                  variable_name = series_name, 
                                  title = paste("MAPE by Model and Horizon:", series_name))
  mape_file <- file.path(figures_dir, paste0(sanitized_name, "_mape.png"))
  ggplot2::ggsave(mape_file, mape_plot, width = 10, height = 6, dpi = 300)
  
  # Methodology weights plot (if available)
  if (!is.null(methodology_weights)) {
    method_plot <- plot_model_weights(methodology_weights, 
                                      title = paste("Methodology Weights:", series_name), 
                                      xlab = "Methodology", 
                                      ylab = "Weight")
    method_file <- file.path(figures_dir, paste0(sanitized_name, "_methodology.png"))
    ggplot2::ggsave(method_file, method_plot, width = 10, height = 6, dpi = 300)
  }
  
  # Generate report in Markdown format
  report_file <- file.path(output_dir, paste0(sanitized_name, "_report.md"))
  
  # Create report content
  report_content <- c(
    paste("# Forecast Report:", series_name),
    "",
    "## 1. Level Forecast",
    "",
    paste0("![Level Forecast](", file.path("figures", paste0(sanitized_name, "_level.png")), ")"),
    "",
    "## 2. Year-over-Year Growth",
    "",
    paste0("![Growth Forecast](", file.path("figures", paste0(sanitized_name, "_growth.png")), ")"),
    "",
    "## 3. Model Weights",
    "",
    paste0("![Model Weights](", file.path("figures", paste0(sanitized_name, "_weights.png")), ")"),
    "",
    "## 4. Error Metrics",
    "",
    "### RMSE by Model and Horizon",
    "",
    paste0("![RMSE Heatmap](", file.path("figures", paste0(sanitized_name, "_rmse.png")), ")"),
    "",
    "### MAPE by Model and Horizon",
    "",
    paste0("![MAPE Heatmap](", file.path("figures", paste0(sanitized_name, "_mape.png")), ")")
  )
  
  # Add methodology weights section if available
  if (!is.null(methodology_weights)) {
    method_section <- c(
      "",
      "## 5. Methodology Weights",
      "",
      paste0("![Methodology Weights](", file.path("figures", paste0(sanitized_name, "_methodology.png")), ")")
    )
    report_content <- c(report_content, method_section)
  }
  
  # Add forecast values table
  forecast_values <- data.frame(
    Date = zoo::index(zoo::as.zoo(forecast_ts)),
    Value = as.numeric(forecast_ts)
  )
  
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
  writeLines(report_content, report_file)
  
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
  
  message("Generated Markdown report: ", report_file)
  return(invisible(report_file))
}