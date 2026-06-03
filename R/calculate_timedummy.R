#' Calculate Time Dummy Index
#'
#' Estimates a price index using a single regression with time dummy variables.
#'
#' @author Vivek Gajadhar
#' @param dataset data frame with input data
#' @param period_variable name of the time variable (string)
#' @param dependent_variable name of the dependent variable (usually price, assumed unlogged)
#' @param numerical_variables vector of numeric quality-determining variables
#' @param categorical_variables vector of categorical variables
#' @param reference_period period to be normalized to index = 100 (e.g., "2015")
#' @param number_of_observations logical, whether to return number of observations per period (default = FALSE)
#' @return data frame with period, Index, and optionally number_of_observations
#' @importFrom stats lm coefficients as.formula na.omit
#' @importFrom utils tail
#' @keywords internal
#' @noRd

calculate_time_dummy <- function(dataset,
                                 period_variable,
                                 dependent_variable,
                                 numerical_variables,
                                 categorical_variables,
                                 reference_period = NULL,
                                 number_of_observations = FALSE) {
  # 1. PREPARE DATA
  clean_data <- prepare_hedonic_data(
    dataset = dataset, 
    period_variable = period_variable, 
    dependent_variable = dependent_variable, 
    numerical_variables = numerical_variables, 
    categorical_variables = categorical_variables,
    log_dependent = TRUE # Time dummy logs the dependent variable
  )
  
  # 2. FIT MODEL
  independent_vars <- c(numerical_variables, categorical_variables, period_variable)
  model <- fit_hedonic_model(
    dataset = clean_data,
    dependent_variable = paste0("log_", dependent_variable),
    independent_variables = independent_vars
  )
  
  # 3. EXTRACT INDEX
  coefs <- stats::coefficients(model)
  period_levels <- sort(unique(clean_data[[period_variable]]))
  
  log_time_dummies <- stats::setNames(rep(0, length(period_levels)), period_levels)
  time_dummy_names <- grep(paste0("^", period_variable), names(coefs), value = TRUE)
  
  for (name in time_dummy_names) {
    level <- sub(paste0("^", period_variable), "", name)
    if(level %in% names(log_time_dummies)) log_time_dummies[level] <- coefs[name]
  }
  
  # Base index values 
  index_vals <- exp(log_time_dummies) * 100
  
  # 4. FORMAT OUTPUT
  obs_counts <- NULL
  if (number_of_observations) {
    obs_counts <- as.integer(table(clean_data[[period_variable]])[names(index_vals)])
  }
  
  results <- format_index_output(
    periods = names(index_vals),
    index_values = index_vals,
    reference_period = reference_period,
    observation_counts = obs_counts
  )
  
  return(results)
}