# GDP Forecasting System

## Overview

This is a comprehensive GDP forecasting system implementing a variety of econometric methods to create accurate projections of GDP components. The system follows a multi-step approach combining univariate, monthly indicators, and multivariate models to generate optimized forecasts.

## Key Features

- **Multiple Forecasting Methodologies**: Combines ARIMA, ETS, Neural Networks, VAR, BVAR, and FAVAR models
- **Automatic Seasonal Adjustment**: Uses X-13-ARIMA-SEATS methodology for seasonal adjustment
- **Monthly Indicators**: Incorporates monthly data to improve quarterly forecasts
- **Model Evaluation**: Implements rolling and recursive forecasting for robust method assessment
- **Optimal Model Combination**: Uses inverse MSE weights for evidence-based forecast combinations
- **Comprehensive Reporting**: Generates visualizations and detailed reports for each GDP component

## System Requirements

- R (>= 4.0.0)
- Required packages:
  - tidyverse, readxl, lubridate (data manipulation)
  - forecast, seasonal, tseries (time series analysis)
  - vars, BVAR, FactoMineR (multivariate models)
  - ggplot2, gridExtra, knitr, rmarkdown (visualization and reporting)

## Installation

1. Clone this repository:
  ```
git clone https://github.com/yourusername/gdp-forecasting-system.git
cd gdp-forecasting-system
```

2. Install required packages:
  ```R
# Run in R
source("R/config.R")
check_and_install_packages()
```

## Usage

### Basic Usage

To run the GDP forecasting system with default settings:
  
  ```R
source("main.R")
results <- run_gdp_forecasting()
```

### Custom Configuration

You can customize the system by modifying parameters in `R/config.R` or by passing different values to the `run_gdp_forecasting()` function:
  
  ```R
results <- run_gdp_forecasting(
  data_file = "path/to/your/data.xlsx",
  output_dir = "custom_output",
  refresh_cache = TRUE  # Force recalculation instead of using cached results
)
```

### Input Data Format

The input Excel file should contain:
- **Sheet 1**: Quarterly data for GDP components
- **Sheet 2**: Monthly indicator data
- **Sheet 3**: Data aggregation schemes
- **Sheet 4**: Auxiliary quarterly data (if needed)

Each sheet should have a date/time column followed by data columns for each series.

## Project Structure

```
gdp-forecasting-system/
├── R/
│   ├── config.R              # Configuration parameters
│   ├── data_import.R         # Data import functions
│   ├── seasonal_adjustment.R # Seasonal adjustment functions
│   ├── univariate_models.R   # Univariate model functions
│   ├── monthly_models.R      # Monthly indicators functions
│   ├── multivariate_models.R # Multivariate model functions
│   ├── model_combination.R   # Model combination functions
│   ├── visualization.R       # Visualization functions
│   └── utils.R               # Utility functions
├── main.R                    # Main script orchestrating workflow
├── data.xlsx                 # Example data file
├── output/                   # Output directory (created automatically)
│   ├── figures/              # Generated visualizations
│   ├── models/               # Saved model objects
│   └── reports/              # Generated reports
└── cache/                    # Cache directory for intermediate results
  ```

## Forecasting Process

The system follows this sequential process:
  
1. **Data Import**: Reads data from Excel file, ensuring correct time series formats
2. **Seasonal Adjustment**: Applies X-13-ARIMA-SEATS methodology to adjust for seasonality
3. **Univariate Modeling**: Fits ARIMA, ETS, and NNET models to each series
4. **Monthly Indicator Analysis**: Identifies related monthly indicators and builds forecasting models
5. **Multivariate Modeling**: Fits VAR, BVAR, and FAVAR models to capture inter-series relationships
6. **Model Evaluation**: Calculates error metrics through rolling window forecasting
7. **Forecast Combination**: Weights forecasts based on historical accuracy
8. **Result Presentation**: Generates visualizations and comprehensive reports

## Customization

### Adding New Models

To add new forecasting models, modify the appropriate module file (e.g., `univariate_models.R`) and update the model specifications in the `model_specs` list.

### Modifying Evaluation Criteria

You can change the evaluation metrics and model selection criteria by updating the relevant parameters in `config.R` and modifying the error calculation functions.

## Contact

Juan Manuel Gonzalez Masulli - 96.g.juan@gmail.com
