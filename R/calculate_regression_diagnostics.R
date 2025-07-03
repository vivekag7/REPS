#' Calculate regression diagnostics by period
#'
#' For each period in the data, fits a log-linear model and computes diagnostics:
#' - Normality test (Shapiro-Wilk)
#' - Adjusted R-squared
#' - Breusch-Pagan test for heteroscedasticity
#' - Durbin-Watson test for autocorrelation
#'
#' @author Mohammad Kardal, Vivek Gajadhar
#' @param dataset A data.frame with input data
#' @param period_variable Name of the period variable (string)
#' @param dependent_variable Name of the dependent variable (string)
#' @param numerical_variables Vector of numerical independent variables (default = NULL)
#' @param categorical_variables Vector of categorical independent variables (default = NULL)
#' @return A data.frame with diagnostics by period
#' @importFrom stats lm shapiro.test
#' @importFrom lmtest dwtest bptest
#' @export
#'
#' @examples
#' diagnostics <- calculate_regression_diagnostics(
#'   dataset = data_constraxion,
#'   period_variable = "period",
#'   dependent_variable = "price",
#'   numerical_variables = c("floor_area", "dist_trainstation"),
#'   categorical_variables = c("dummy_large_city", "neighbourhood_code")
#' )
#' head(diagnostics)
calculate_regression_diagnostics <- function(dataset,
                                  period_variable,
                                  dependent_variable,
                                  numerical_variables = NULL,
                                  categorical_variables = NULL) {
  
  validate_input(
    dataset = dataset,
    period_variable = period_variable,
    dependent_variable = dependent_variable,
    numerical_variables = numerical_variables,
    categorical_variables = categorical_variables
  )
  
  
  independent_variables <- c(numerical_variables, categorical_variables)
  
  # Subset and clean data
  dataset <- dataset |>
    dplyr::select(all_of(c(period_variable, dependent_variable, independent_variables)))
  dataset[dataset == ""] <- NA
  dataset <- stats::na.omit(dataset)
  dataset <- droplevels(dataset)
  
  # Build formula 
  dependent_log <- paste0("log(", dependent_variable, ")")
  model_formula <- dependent_log
  for (i in seq_along(independent_variables)) {
    if (i == 1) {
      model_formula <- paste0(model_formula, "~", independent_variables[i])
    } else {
      model_formula <- paste0(model_formula, "+", independent_variables[i])
    }
  }
  
  # Loop over periods
  periods <- sort(unique(dataset[[period_variable]]))
  diagnostics_list <- lapply(periods, function(p) {
    df <- subset(dataset, dataset[[period_variable]] == p)
    if (nrow(df) < 3) return(NULL)
    
    mod <- try(lm(model_formula, data = df), silent = TRUE)
    if (inherits(mod, "try-error")) return(NULL)
    
    # 1. Shapiro-Wilk test
    df_log_price <- if (nrow(df) <= 5000) log(df[[dependent_variable]]) else log(sample(df[[dependent_variable]], 5000))
    norm_pvalue <- tryCatch(shapiro.test(df_log_price)$p.value, error = function(e) NA)
    
    # 2. Adjusted R-squared
    r_adjust <- tryCatch(summary(mod)$adj.r.squared, error = function(e) NA)
    
    # 3. Durbin-Watson test
    autoc_dw <- NA
    autoc_pvalue <- NA
    dw_result <- tryCatch(lmtest::dwtest(mod), error = function(e) NULL)
    if (!is.null(dw_result)) {
      autoc_dw <- as.numeric(dw_result$statistic)
      autoc_pvalue <- as.numeric(dw_result$p.value)
    }
    
    # 4. Breusch-Pagan test
    bp_pvalue <- NA
    bp_result <- tryCatch(lmtest::bptest(mod), error = function(e) NULL)
    if (!is.null(bp_result)) {
      bp_pvalue <- as.numeric(bp_result$p.value)
    }
    
    # Return row
    data.frame(
      period = p,
      norm_pvalue = norm_pvalue,
      r_adjust = r_adjust,
      bp_pvalue = bp_pvalue,
      autoc_pvalue = autoc_pvalue,
      autoc_dw = autoc_dw,
      stringsAsFactors = FALSE
    )
  })
  
  diagnostics <- do.call(rbind, diagnostics_list)
  rownames(diagnostics) <- NULL
  return(diagnostics)
}

#' Plot diagnostics output from calculate_regression_diagnostics as a multi-panel grid (base R)
#'
#' Creates a static 3x2 grid of base R plots showing regression diagnostics:
#' - Normality (Shapiro-Wilk)
#' - Linearity (Adjusted R-squared)
#' - Heteroscedasticity (Breusch-Pagan)
#' - Autocorrelation (Durbin-Watson)
#' - Autocorrelation (p-value DW)
#'
#' @author Vivek Gajadhar
#' @param diagnostics A data.frame as returned by calculate_regression_diagnostics()
#' @param title Optional overall title for the entire plot grid (default: "Regression Diagnostics")
#' @return None. Produces plots in the active graphics device.
#' @importFrom graphics abline axis mtext par plot text
#' @export
#'
#' @examples
#' plot_regression_diagnostics(
#'   calculate_regression_diagnostics(
#'     dataset = data_constraxion,
#'     period_variable = "period",
#'     dependent_variable = "price",
#'     numerical_variables = c("floor_area", "dist_trainstation"),
#'     categorical_variables = c("dummy_large_city", "neighbourhood_code")
#'   )
#' )
plot_regression_diagnostics <- function(diagnostics, title = "Regression Diagnostics") {
  if (!is.data.frame(diagnostics)) {
    stop("Input must be a data.frame as returned by calculate_regression_diagnostics().")
  }
  
  # Ensure periods are sorted and treated as factor levels
  diagnostics <- diagnostics[order(diagnostics$period), ]
  periods <- as.factor(diagnostics$period)
  period_levels <- levels(periods)
  
  # Helper to get only first period of each year
  get_year_start_periods <- function(periods) {
    years <- sub("[^0-9].*$", "", periods)
    periods[!duplicated(years)]
  }
  year_start_periods <- get_year_start_periods(period_levels)
  year_start_indices <- match(year_start_periods, period_levels)
  
  # Set up 3x2 grid with overall title space
  op <- par(mfrow = c(3, 2), oma = c(0, 0, 3, 0), mar = c(5, 4, 4, 2) + 0.1)
  
  # Helper function for consistent axis with 45-degree labels
  draw_x_axis <- function() {
    axis(1, at = year_start_indices, labels = FALSE)
    text(
      x = year_start_indices,
      y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]),
      labels = year_start_periods,
      srt = 45,
      adj = 1,
      xpd = TRUE,
      cex = 0.8
    )
  }
  
  # 1. Normality (Shapiro-Wilk)
  plot(
    diagnostics$norm_pvalue, type = "b", pch = 19, xaxt = "n",
    xlab = "", ylab = "", main = "Normality (Shapiro-Wilk)"
  )
  draw_x_axis()
  abline(h = 0.05, col = "red", lty = 2)
  
  # 2. Linearity (Adjusted R-squared)
  plot(
    diagnostics$r_adjust, type = "b", pch = 19, xaxt = "n",
    xlab = "", ylab = "", main = "Linearity (Adjusted R-squared)"
  )
  draw_x_axis()
  abline(h = 0.6, col = "red", lty = 2)
  
  # 3. Heteroscedasticity (Breusch-Pagan)
  plot(
    diagnostics$bp_pvalue, type = "b", pch = 19, xaxt = "n",
    xlab = "", ylab = "", main = "Heteroscedasticity (Breusch-Pagan)"
  )
  draw_x_axis()
  abline(h = 0.05, col = "red", lty = 2)
  
  # 4. Autocorrelation (Durbin-Watson)
  plot(
    diagnostics$autoc_dw, type = "b", pch = 19, xaxt = "n",
    xlab = "", ylab = "", main = "Autocorrelation (Durbin-Watson)"
  )
  draw_x_axis()
  abline(h = c(1.75, 2.25), col = "red", lty = 2)
  
  # 5. Autocorrelation (p-value DW)
  plot(
    diagnostics$autoc_pvalue, type = "b", pch = 19, xaxt = "n",
    xlab = "", ylab = "", main = "Autocorrelation (p-value DW)"
  )
  draw_x_axis()
  abline(h = 0.05, col = "red", lty = 2)
  
  # Add overall main title
  mtext(title, outer = TRUE, cex = 1.5, line = 1)
  
  # Restore par settings
  par(op)
}






