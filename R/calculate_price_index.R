#' Calculate index based on specified method (Fisher, Laspeyres, Paasche, HMTS)
#'
#' Central hub function to calculate index figures using different methods.
#' @author Vivek Gajadhar 
#' 
#' @param method one of: "fisher", "laspeyres", "paasche", "hmts"
#' @param dataset data frame with input data
#' @param period_variable name of the variable indicating time periods
#' @param dependent_variable usually the price
#' @param continuous_variables vector with numeric quality-determining variables
#' @param categorical_variables vector with categorical variables (also dummies)
#' @param reference_period period or group of periods that will be set to 100
#' @param number_of_observations show number of observations? Default = TRUE
#' @param periods_in_year (HMTS only) number of periods per year (e.g. 12 for months)
#' @param production_since (HMTS only) start period for production simulation
#' @param number_preliminary_periods (HMTS only) number of preliminary periods
#' @param resting_points (HMTS only) return detailed outputs? Default = FALSE
#' @param index (Laspeyres/Paasche only) include index column? Default = TRUE
#' @param imputation (Laspeyres/Paasche only) include imputation values? Default = FALSE
#'
#' @return A data.frame (or list for when method is HMTS with resting_points = TRUE)
#' @export
#' @examples
#' # Laspeyres index
#' Tbl_Laspeyres <- calculate_price_index(
#'   method = "laspeyres",
#'   dataset = data_constraxion,
#'   period_variable = "period",
#'   dependent_variable = "price",
#'   continuous_variables = "floor_area",
#'   categorical_variables = "neighbourhood_code",
#'   reference_period = 2015,
#'   number_of_observations = TRUE,
#'   imputation = FALSE
#' )
#' head(Tbl_Laspeyres)
#'
#' # Paasche index
#' Tbl_Paasche <- calculate_price_index(
#'   method = "paasche",
#'   dataset = data_constraxion,
#'   period_variable = "period",
#'   dependent_variable = "price",
#'   continuous_variables = "floor_area",
#'   categorical_variables = "neighbourhood_code",
#'   reference_period = 2015,
#'   number_of_observations = TRUE,
#'   imputation = FALSE
#' )
#' head(Tbl_Paasche)
#'
#' # Fisher index (geometric mean of Laspeyres and Paasche)
#' Tbl_Fisher <- calculate_price_index(
#'   method = "fisher",
#'   dataset = data_constraxion,
#'   period_variable = "period",
#'   dependent_variable = "price",
#'   continuous_variables = "floor_area",
#'   categorical_variables = "neighbourhood_code",
#'   reference_period = 2015,
#'   number_of_observations = TRUE
#' )
#' head(Tbl_Fisher)

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
                            imputation = FALSE) {
  
  method <- tolower(method)
  valid_methods <- c("fisher", "laspeyres", "paasche", "hmts")
  
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
}
