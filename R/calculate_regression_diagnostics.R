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
  dataset <- dataset[, c(period_variable, dependent_variable, independent_variables), drop = FALSE]
  dataset[dataset == ""] <- NA
  dataset <- na.omit(dataset)
  dataset[] <- lapply(dataset, function(x) if (is.factor(x)) droplevels(x) else x)
  
  
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
    df_log_price <- if (nrow(df) <= 5000) log(df[[dependent_variable]]) else sample(log(df[[dependent_variable]]), 5000)
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
#' @importFrom graphics abline axis mtext par plot text rect lines 
#' @importFrom grDevices rgb
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
  
  diagnostics <- diagnostics[order(diagnostics$period), ]
  periods <- as.factor(diagnostics$period)
  period_levels <- levels(periods)
  
  get_year_start_periods <- function(periods) {
    years <- sub("[^0-9].*$", "", periods)
    periods[!duplicated(years)]
  }
  year_start_periods <- get_year_start_periods(period_levels)
  year_start_indices <- match(year_start_periods, period_levels)
  
  # Colors
  soft_green <- rgb(0, 1, 0, 0.1)
  soft_red   <- rgb(1, 0, 0, 0.1)
  
  op <- par(mfrow = c(3, 2), oma = c(0, 0, 3, 0), mar = c(5, 4, 4, 2) + 0.1)
  on.exit(par(op))
  
  
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
  
  x_range <- c(0, length(period_levels) + 1)
  
  ### 1. Normality (Shapiro-Wilk)
  y_norm <- diagnostics$norm_pvalue
  plot(y_norm, type = "n", xaxt = "n", xlab = "", ylab = "", main = "Normality (p-value Shapiro-Wilk)")
  abline(h = 0.05, col = "red", lty = 2)
  # Shading
  usr <- par("usr")
  rect(x_range[1], 0.05, x_range[2], usr[4], col = soft_green, border = NA)
  rect(x_range[1], usr[3], x_range[2], 0.05, col = soft_red, border = NA)
  lines(y_norm, type = "b", pch = 19)
  draw_x_axis()
  
  ### 2. Linearity (Adjusted R-squared)
  y_r2 <- diagnostics$r_adjust
  plot(y_r2, type = "n", xaxt = "n", xlab = "", ylab = "", main = "Linearity (Adjusted R-squared)")
  abline(h = 0.6, col = "red", lty = 2)
  usr <- par("usr")
  rect(x_range[1], usr[3], x_range[2], 0.6, col = soft_red, border = NA)
  rect(x_range[1], 0.6, x_range[2], usr[4], col = soft_green, border = NA)
  lines(y_r2, type = "b", pch = 19)
  draw_x_axis()
  
  
  ### 4. Autocorrelation (Durbin-Watson)
  y_dw <- diagnostics$autoc_dw
  plot(y_dw, type = "n", xaxt = "n", xlab = "", ylab = "", main = "Autocorrelation (Durbin-Watson)")
  abline(h = c(1.75, 2.25), col = "red", lty = 2)
  usr <- par("usr")
  # Green band between
  rect(x_range[1], 1.75, x_range[2], 2.25, col = soft_green, border = NA)
  # Red bands outside
  rect(x_range[1], usr[3], x_range[2], 1.75, col = soft_red, border = NA)
  rect(x_range[1], 2.25, x_range[2], usr[4], col = soft_red, border = NA)
  lines(y_dw, type = "b", pch = 19)
  draw_x_axis()
  
  ### 5. Autocorrelation (p-value DW)
  y_dwp <- diagnostics$autoc_pvalue
  plot(y_dwp, type = "n", xaxt = "n", xlab = "", ylab = "", main = "Autocorrelation (p-value Durbin-Watson)")
  abline(h = 0.05, col = "red", lty = 2)
  usr <- par("usr")
  rect(x_range[1], 0.05, x_range[2], usr[4], col = soft_green, border = NA)
  rect(x_range[1], usr[3], x_range[2], 0.05, col = soft_red, border = NA)
  lines(y_dwp, type = "b", pch = 19)
  draw_x_axis()
  
  ### 3. Heteroscedasticity (Breusch-Pagan)
  y_bp <- diagnostics$bp_pvalue
  plot(y_bp, type = "n", xaxt = "n", xlab = "", ylab = "", main = "Heteroscedasticity (p-value Breusch-Pagan)")
  abline(h = 0.05, col = "red", lty = 2)
  usr <- par("usr")
  rect(x_range[1], 0.05, x_range[2], usr[4], col = soft_green, border = NA)
  rect(x_range[1], usr[3], x_range[2], 0.05, col = soft_red, border = NA)
  lines(y_bp, type = "b", pch = 19)
  draw_x_axis()
  
  ### Overall title
  mtext(title, outer = TRUE, cex = 1.5, line = 1)
  
}







