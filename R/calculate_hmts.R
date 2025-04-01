
#' Calculate HMTS index (Hedonic Multilateral Time series re-estimation Splicing)
#'
#' Based on a hedonic model, an index is calculated in below steps. See also Ishaak, Ouwehand, Remoy & De Haan (2023).
#' 1: for each period, average imputed prices are calculated with the first period as base period.
#' 2: step 1 is repeated for every possible base period. This result in as many series of imputed values as the number of periods.
#' 3: All series with imputed prices are re-estimated with a Kalman filter (also time series model/state space model)
#'    This step can be turned off with a parameter.
#' 4: The series of imputed values are transformed into index series.
#' 5: a specified (parameter) window is chosen of index figures that continues in the calculation.
#'    This step can be turned off with a parameter.
#' 6: Of the remaining index figures, the geometric average per period is calculated.
#'    The remaining index figures form the final index.
#'
#' Parameter 'production_since':
#' To simulate a series, where 1 period a time expires (as in production), a manual choice in the past is possible.
#' Until this period, all periods are imputed. After that, 1 period is added.
#'
#' Parameter 'resting_points':
#' If TRUE, the output is a list of tables. These tables can be called with a $ after the output.
#' $Index table with periods, index and number of observations
#' $Window table with the index figures within the chosen window
#' $Chosen_index_series table with index series before the window splice
#' $Matrix_HMTS_index table with index series based on re-estimated imputations (time series model)
#' $Matrix_HMTS table with re-estimated imputations (time series model)
#' $Matrix_HMTS_index table with index series based on estimated imputations (hedonic model)
#' $Matrix_HMTS table with estimated imputations (time series model)l
#' $Matrix_HMTS_analyse table with diagnostic values of the time series model per base period
#'
#' @author Farley Ishaak
#' @param period_variable variable in the dataset with the period
#' @param dependent_variable usually the sale price
#' @param continuous_variables vector with quality-determining continues variables (numeric, no dummies)
#' @param categorical_variables vector with categorical variables (also dummy)
#' @param log_dependent should the dependent variable be transformed to its logarithm? default = TRUE
#' @param reference_period period or group of periods that will be set to 100 (numeric/string)
#' @param number_of_observations number of observations per period (default = TRUE)
#' @param periods_in_year if month, then 12. If quarter, then 4, etc. (default = 4)
#' @param production_since 1 period in the format of the period_variable. See description above (default = NULL)
#' @param number_preliminary_periods number of periods that the index is preliminary. Only works if production_since <> NULL. default = 3
#' @param resting_points Should analyses values be returned? (default = FALSE)
#' @return
#' $Matrix_HMTS_index table with index series based on estimations with time series re-estimations
#' $Matrix_HMTS table with estimated values based on time series re-estimations
#' $Matrix_HMS_index table with index series based on estimations with the hedonic model
#' $Matrix_HMS table with estimated values based on the hedonic model
#' $Matrix_HMTS_analysis table with analysis values of the time series model per base period
#' @keywords internal
#' @return table with periods, index (and optional confidence intervals) and number of observations. If resting_points = TRUE, then list with tables. See general description and examples.

calculate_hmts <- function(
    dataset,
    period_variable,
    dependent_variable,
    continuous_variables,
    categorical_variables,
    log_dependent,
    reference_period,
    periods_in_year,
    production_since = NULL,
    number_preliminary_periods,
    number_of_observations,
    resting_points) {
  
  
  periods_in_year <- as.numeric(periods_in_year)
  number_preliminary_periods <- as.numeric(number_preliminary_periods)
  
  dataset <- dataset |>
    dplyr::rename(period = all_of(period_variable)) |>
    dplyr::mutate(period = as.character(period),
                  dplyr::across(dplyr::all_of(categorical_variables),
                                as.factor))
  
  results <- calculate_hmts_index(
    dataset = dataset,
    period_variable = period_variable,
    dependent_variable = dependent_variable,
    continuous_variables = continuous_variables,
    categorical_variables = categorical_variables,
    log_dependent = log_dependent,
    reference_period = reference_period,
    periods_in_year = periods_in_year,
    production_since = production_since,
    number_preliminary_periods = number_preliminary_periods,
    number_of_observations = number_of_observations,
    resting_points = resting_points)
  
  if (resting_points == TRUE) {
    tbl_resting_points <- results
    results <- as.data.frame(results$index)
    
    results <- list(Index = results
                    , Window = tbl_resting_points$window
                    , Chosen_index_series = tbl_resting_points$chosen_index_series 
                    , Matrix_HMTS_index = tbl_resting_points$matrix_hmts_index
                    , Matrix_HMTS = tbl_resting_points$matrix_hmts
                    , Matrix_HMTS_analysis = tbl_resting_points$matrix_hmts_analysis)
  }
  
  return(results)
}
