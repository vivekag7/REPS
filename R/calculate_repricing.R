#' Calculate repricing index based on hedonic model (geometric adjustment)
#'
#' For each pair of subsequent periods, this method compares the observed geometric mean price
#' with the predicted mean price from a hedonic regression model. The ratio of these two values
#' forms the basis of the repricing growth rate, which is then accumulated into an index.
#'
#' @author Vivek Gajadhar, Farley Ishaak
#' @param dataset a data frame containing the data
#' @param period_variable character name of the time period variable
#' @param dependent_variable character name of the dependent variable (e.g., sale price)
#' @param numerical_variables character vector of numeric quality-determining variables
#' @param categorical_variables character vector of categorical variables (including dummies)
#' @param periods_in_year if month, then 12. If quarter, then 4, etc. (default = 4)
#' @param reference_period reference period (numeric or string) to normalize index to 100
#' @param number_of_observations logical, if TRUE, adds number of observations column
#' @return a data.frame with columns: period, Index, (optionally number_of_observations)
#' @keywords internal
#' @noRd
#' @importFrom stats lm predict as.formula aggregate

calculate_repricing <- function(dataset,
                                period_variable,
                                dependent_variable,
                                numerical_variables,
                                categorical_variables,
                                reference_period = NULL,
                                number_of_observations = FALSE,
                                periods_in_year = 4) {
  
  # 1. PREPARE DATA
  clean_data <- prepare_hedonic_data(
    dataset = dataset,
    period_variable = period_variable,
    dependent_variable = dependent_variable,
    numerical_variables = numerical_variables,
    categorical_variables = categorical_variables,
    log_dependent = TRUE # Repricing models log-prices directly
  )
  independent_variables <- c(numerical_variables, categorical_variables)
  dep_var_log <- paste0("log_", dependent_variable)
  
  # Sort unique periods
  period_list <- sort(unique(clean_data[[period_variable]]), decreasing = FALSE)
  period_values <- clean_data[[period_variable]]

  # Subset base year
  base_year <- period_list[c(1:periods_in_year)]
  subset_data_base <- clean_data[period_values %in% base_year, , drop = FALSE]
  
  # Fit model period base year using the centralized helper
  model_base <- fit_hedonic_model(
    dataset = subset_data_base,
    dependent_variable = dep_var_log, 
    independent_variables = independent_variables
  )
 
  # Predict mean price for observations in all periods using the centralized helper
  predicted_log_price <- predict_hedonic(model = model_base, newdata = clean_data)
  
  # Calculate geometric means per period
  observed_gmean <- exp(tapply(log(clean_data[[dependent_variable]]), period_values, mean, na.rm = TRUE))
  predicted_price <- exp(tapply(predicted_log_price, period_values, mean, na.rm = TRUE))
  observed_gmean <- observed_gmean[period_list]
  predicted_price <- predicted_price[period_list]
  
  # Calculate index
  index <- (observed_gmean / observed_gmean[1]) /
           (predicted_price / predicted_price[1]) * 100
  
  # 2. FORMAT OUTPUT
  obs_counts <- NULL
  if (number_of_observations) {
    counts <- table(period_values)
    obs_counts <- as.integer(counts[period_list])
  }
  
  results <- format_index_output(
    periods = period_list,
    index_values = index,
    reference_period = reference_period,
    observation_counts = obs_counts
  )
  
  return(results)
}
