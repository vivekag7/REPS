# REPS: Hedonic and Multilateral Index Methods for Real Estate Price Statistics

<!-- badges: start -->
[![R-CMD-check](https://github.com/vivekag7/REPS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vivekag7/REPS/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**REPS** (Real Estate Price Statistics) is an R package for computing price indices using a variety of hedonic regression and multilateral methods. It provides a unified interface to calculate quality-adjusted price indices across time, supporting classical index formulas as well as advanced techniques tailored to property price analysis. While developed with real estate in mind, the package is applicable to any domain where reliable, quality-adjusted price comparisons over time are needed. REPS was developed by Statistics Netherlands (CBS) with funding from Eurostat, aligning with international guidelines for compiling residential property price indices.

## Installation

You can install the development version of **REPS** from GitHub using **devtools**:

```r
# install.packages("devtools")  # if devtools is not installed
devtools::install_github("vivekag7/REPS")
```

## Hedonic Price Index Methods

REPS offers a **single entry point** `calculate_price_index()` to compute price indices using different methodologies. By changing the `method` argument, you can generate indices with any of the supported approaches, all from the same dataset and function call. Supported index calculation methods include:

- **Laspeyres** – Base-period weighted hedonic double imputation index.
- **Paasche** – Current-period weighted hedonic double imputation index.
- **Fisher** – The geometric mean of Laspeyres and Paasche indices.
- **Time Dummy** – A single regression with time dummy variables (log-linear hedonic model).
- **Rolling Time Dummy** – A rolling window (chained) time-dummy hedonic index.
- **Repricing** – A quasi-repeat-sales method comparing observed vs. predicted price changes between consecutive periods.
- **HMTS** – *Hedonic Multilateral Time Series re-estimation with Splicing*. An advanced multilateral index method that leverages hedonic models across all period pairs and uses Kalman filtering to improve stability.

All methods are accessed via `calculate_price_index()` with a consistent interface. The function returns either a single index or, for multiple methods, a list of results. A reference period can be specified to normalize index values (default sets the first period to 100).

## Diagnostics and Visualization Tools

- **Regression Diagnostics:** `calculate_regression_diagnostics()` fits a separate log-linear model per period and checks normality, R², autocorrelation, and heteroscedasticity of residuals. `plot_regression_diagnostics()` visualizes these over time in a 3×2 grid.
- **Index Visualization:** `plot_price_index()` plots one or more index series as a time-series chart for comparison and exploration.

## Getting Started and Documentation

- **Example Dataset:** `data_constraxion` is included (7,800 × 6), simulating real estate transactions. Use `data("data_constraxion")` to load it.
- **Vignettes:** Use `vignette("calculate_price_index", package="REPS")` and `vignette("calculate_regression_diagnostics", package="REPS")` for step-by-step guides using the example dataset.

REPS provides a comprehensive and extensible framework to compute and evaluate real estate price indices. Its flexible architecture enables comparison across methods and statistical validation of underlying models, suitable for both official statistics and research applications.
