#' Calculate index based on specified method (Fisher, Laspeyres, Paasche, HMTS, Time Dummy, Rolling Time Dummy)
#'
#' Central hub function to calculate index figures using different methods.
#'
#' @param method One of: "fisher", "laspeyres", "paasche", "hmts", "timedummy", "rolling_timedummy"
#' @param dataset Data frame with input data
#' @param period_variable Name of the variable indicating time periods
#' @param dependent_variable Usually the price
#' @param continuous_variables Vector with numeric quality-determining variables
#' @param categorical_variables Vector with categorical variables (also dummies)
#' @param reference_period Period or group of periods that will be set to 100
#' @param number_of_observations Logical, whether to show number of observations (default = TRUE)
#' @param periods_in_year (HMTS only) Number of periods per year (e.g. 12 for months)
#' @param production_since (HMTS only) Start period for production simulation
#' @param number_preliminary_periods (HMTS only) Number of preliminary periods
#' @param resting_points (HMTS only) Whether to return detailed outputs (default = FALSE)
#' @param index (Laspeyres/Paasche only) Include index column? Default = TRUE
#' @param imputation (Laspeyres/Paasche only) Include imputation values? Default = FALSE
#' @param window_length (Rolling Time Dummy only) Window size in number of periods
#'
#' @return A data.frame (or list for HMTS with resting_points = TRUE)
#' @export
#'
#' @examples
#' # Fisher index (geometric mean of Laspeyres and Paasche)
#' Tbl_Fisher <- calculate_price_index(
#'   method = "fisher",
#'   dataset = data_constraxion,
#'   period_variable = "period",
#'   dependent_variable = "price",
#'   continuous_variables = "floor_area",
#'   categorical_variables = "neighbourhood_code",
#'   reference_period = "2015",
#'   number_of_observations = FALSE
#' )

#'
#' # Rolling Time Dummy index
#' Tbl_TD_Rolling <- calculate_price_index(
#'   method = "rolling_timedummy",
#'   dataset = data_constraxion,
#'   period_variable = "period",
#'   dependent_variable = "price",
#'   continuous_variables = "floor_area",
#'   categorical_variables = "neighbourhood_code",
#'   reference_period = "2015",
#'   window_length = 5,
#'   number_of_observations = FALSE
#' )
calculate_price_index <- function(method,
                                  dataset,
                                  period_variable,
                                  dependent_variable,
                                  continuous_variables,
                                  categorical_variables,
                                  reference_period = NULL,
                                  number_of_observations = TRUE,
                                  periods_in_year = 4,
                                  production_since = NULL,
                                  number_preliminary_periods = 3,
                                  resting_points = FALSE,
                                  index = TRUE,
                                  imputation = FALSE,
                                  window_length = 5) {
  
  method <- tolower(method)
  valid_methods <- c("fisher", "laspeyres", "paasche", "hmts", "timedummy", "rolling_timedummy", "repricing")
  
  if (!method %in% valid_methods) {
    stop(paste0("Invalid method: '", method, "'. Please choose one of: ",
                paste(shQuote(valid_methods), collapse = ", "), "."))
  }
  
  validate_input(dataset, period_variable, dependent_variable, continuous_variables, categorical_variables)
  
  if (method == "fisher") {
    return(calculate_fisher(
      dataset = dataset,
      period_variable = period_variable,
      dependent_variable = dependent_variable,
      continuous_variables = continuous_variables,
      categorical_variables = categorical_variables,
      reference_period = reference_period,
      number_of_observations = number_of_observations
    ))
  }
  
  if (method == "laspeyres") {
    return(calculate_laspeyres(
      dataset = dataset,
      period_variable = period_variable,
      dependent_variable = dependent_variable,
      continuous_variables = continuous_variables,
      categorical_variables = categorical_variables,
      reference_period = reference_period,
      index = index,
      number_of_observations = number_of_observations,
      imputation = imputation
    ))
  }
  
  if (method == "paasche") {
    return(calculate_paasche(
      dataset = dataset,
      period_variable = period_variable,
      dependent_variable = dependent_variable,
      continuous_variables = continuous_variables,
      categorical_variables = categorical_variables,
      reference_period = reference_period,
      index = index,
      number_of_observations = number_of_observations,
      imputation = imputation
    ))
  }
  
  if (method == "hmts") {
    return(calculate_hmts(
      dataset = dataset,
      period_variable = period_variable,
      dependent_variable = dependent_variable,
      continuous_variables = continuous_variables,
      categorical_variables = categorical_variables,
      reference_period = reference_period,
      periods_in_year = periods_in_year,
      production_since = production_since,
      number_preliminary_periods = number_preliminary_periods,
      number_of_observations = number_of_observations,
      resting_points = resting_points
    ))
  }
  
  if (method == "timedummy") {
    return(calculate_time_dummy_index(
      dataset = dataset,
      period_variable = period_variable,
      dependent_variable = dependent_variable,
      continuous_variables = continuous_variables,
      categorical_variables = categorical_variables,
      reference_period = reference_period,
      number_of_observations = number_of_observations
    ))
  }
  
  if (method == "rolling_timedummy") {
    if (is.null(window_length)) stop("You must specify 'window_length' for rolling time dummy method.")
    return(calculate_rolling_timedummy_index(
      dataset = dataset,
      period_variable = period_variable,
      dependent_variable = dependent_variable,
      continuous_variables = continuous_variables,
      categorical_variables = categorical_variables,
      reference_period = reference_period,
      window_length = window_length,
      number_of_observations = number_of_observations
    ))
  }
  
  if (method == "repricing") {
    return(calculate_repricing(
      dataset = dataset,
      period_variable = period_variable,
      dependent_variable = dependent_variable,
      continuous_variables = continuous_variables,
      categorical_variables = categorical_variables,
      reference_period = reference_period,
      number_of_observations = number_of_observations
    ))
  }
}
