# config.R - Configuration parameters for GDP forecasting system
# This file centralizes all configuration settings for the forecasting system

#------------------------------------------------------------------------------
# File paths
#------------------------------------------------------------------------------
DATA_FILE <- "data.xlsx"  # Main data file

# Output directories
OUTPUT_DIR <- "output"
FIGURES_DIR <- file.path(OUTPUT_DIR, "figures")
MODELS_DIR <- file.path(OUTPUT_DIR, "models")
REPORTS_DIR <- file.path(OUTPUT_DIR, "reports")

#------------------------------------------------------------------------------
# Data configuration
#------------------------------------------------------------------------------
# Excel sheet indices
QUARTERLY_SHEET <- 1  # Sheet with quarterly data
MONTHLY_SHEET <- 2    # Sheet with monthly data
AGGREGATION_SHEET <- 3  # Sheet with aggregation schemes
AUXILIARY_SHEET <- 4    # Sheet with auxiliary data

#------------------------------------------------------------------------------
# Model parameters
#------------------------------------------------------------------------------
# General parameters
LOG_TRANSFORM <- TRUE  # Whether to log-transform series before modeling
ROLLING_WINDOW_SIZE <- 20  # Size of rolling window for evaluation
FORECAST_HORIZON <- 4  # Number of quarters to forecast
SEASONALITY_SIGNIFICANCE <- 0.05  # Significance level for seasonality tests

# ARIMA parameters
ARIMA_MAX_P <- 4  # Maximum AR order
ARIMA_MAX_Q <- 4  # Maximum MA order
ARIMA_MAX_D <- 2  # Maximum differencing order

# VAR parameters
VAR_MAX_LAGS <- 4  # Maximum lag order for VAR models
VAR_IC <- "AIC"    # Information criterion for VAR order selection

# BVAR parameters
BVAR_LAGS <- c(1, 2, 4)  # Lag orders for BVAR models
BVAR_PRIOR <- "minnesota"  # Prior type for BVAR

# Error metrics to calculate
ERROR_METRICS <- c("MAPE", "MAE", "RMSE", "MSE", "Theil_U")

#------------------------------------------------------------------------------
# Packages required
#------------------------------------------------------------------------------
REQUIRED_PACKAGES <- c(
  # Core packages
  "tidyverse",   # For data manipulation
  "readxl",      # For reading Excel files
  "lubridate",   # For date handling
  
  # Time series packages
  "forecast",    # For time series forecasting
  "seasonal",    # For X-13 seasonal adjustment
  "tseries",     # For time series analysis
  
  # Multivariate modeling
  "vars",        # For VAR models
  "BVAR",        # For Bayesian VAR models
  "FactoMineR",  # For factor analysis (FAVAR)
  
  # Visualization and reporting
  "ggplot2",     # For visualization
  "knitr",       # For report generation
  "rmarkdown",   # For report generation
  "gridExtra"    # For arranging multiple plots
)

#------------------------------------------------------------------------------
# Functions
#------------------------------------------------------------------------------

#' Create required directories if they don't exist
create_directories <- function() {
  dirs <- c(OUTPUT_DIR, FIGURES_DIR, MODELS_DIR, REPORTS_DIR)
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      message(paste("Created directory:", dir))
    }
  }
}

#' Check and install required packages
check_and_install_packages <- function() {
  missing_packages <- REQUIRED_PACKAGES[!REQUIRED_PACKAGES %in% installed.packages()[,"Package"]]
  
  if (length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages)
  }
  
  # Load all required packages
  for (pkg in REQUIRED_PACKAGES) {
    library(pkg, character.only = TRUE)
  }
  
  message("All required packages are installed and loaded.")
}